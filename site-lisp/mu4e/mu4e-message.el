;;; mu4e-message.el -- part of mu4e, the mu mail user agent -*- lexical-binding: t -*-

;; Copyright (C) 2012-2020 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>

;; This file is not part of GNU Emacs.

;; mu4e is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; mu4e is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with mu4e.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Functions to get data from mu4e-message plist structure

;;; Code:

(require 'cl-lib)
(require 'mu4e-vars)
(require 'flow-fill)
(require 'shr)

(declare-function mu4e-error "mu4e-utils")
(declare-function mu4e-warn  "mu4e-utils")
(declare-function mu4e-personal-address-p "mu4e-utils")
(declare-function mu4e-make-temp-file  "mu4e-utils")

(defvar mu4e~view-message)
(defvar shr-inhibit-images)

(defcustom mu4e-html2text-command 'mu4e-shr2text
  "Either a shell command or a function that converts from html to plain text.

If it is a shell command, the command reads html from standard
input and outputs plain text on standard output. If you use the
htmltext program, it's recommended you use \"html2text -utf8
-width 72\". Alternatives are the python-based html2markdown, w3m
and on MacOS you may want to use textutil.

It can also be a function, which takes a messsage-plist as
argument and is expected to return the textified html as output.

For backward compatibility, it can also be a parameterless
function which is run in the context of a buffer with the html
and expected to transform this (like the `html2text' function).

In all cases, the output is expected to be in UTF-8 encoding.

The default is to use the shr renderer."
  :type '(choice string function)
  :group 'mu4e-view)

(defcustom mu4e-view-prefer-html nil
  "Whether to base the body display on the html-version.
If the e-mail message has no html-version the plain-text version
is always used."
  :type 'boolean
  :group 'mu4e-view)

(defcustom mu4e-view-html-plaintext-ratio-heuristic 5
  "Ratio between the length of the html and the plain text part.
Below this ratio mu4e will consider the plain text part to be
'This messages requires html' text bodies. You can neutralize
it (always show the text version) by using
`most-positive-fixnum'."
  :type 'integer
  :group 'mu4e-view)

(defvar mu4e-message-body-rewrite-functions '(mu4e-message-outlook-cleanup)
  "List of functions to transform the message body text.
The functions take two parameters, MSG and TXT, which are the
message-plist and the text, which is the plain-text version,
ossibly converted from html and/or transformed by earlier rewrite
functions.")

;;; Message fields

(defsubst mu4e-message-field-raw (msg field)
  "Retrieve FIELD from message plist MSG.
FIELD is one of :from, :to, :cc, :bcc, :subject, :data,
:message-id, :path, :maildir, :priority, :attachments,
:references, :in-reply-to, :body-txt, :body-html

Returns nil if the field does not exist.

A message plist looks something like:
\(:docid 32461
 :from ((\"Nikola Tesla\" . \"niko@example.com\"))
 :to ((\"Thomas Edison\" . \"tom@example.com\"))
 :cc ((\"Rupert The Monkey\" . \"rupert@example.com\"))
 :subject \"RE: what about the 50K?\"
 :date (20369 17624 0)
 :size 4337
 :message-id \"238C8233AB82D81EE81AF0114E4E74@123213.mail.example.com\"
 :path  \"/home/tom/Maildir/INBOX/cur/133443243973_1.10027.atlas:2,S\"
 :maildir \"/INBOX\"
 :priority normal
 :flags (seen)
 :attachments
     ((:index 2 :name \"photo.jpg\" :mime-type \"image/jpeg\" :size 147331)
      (:index 3 :name \"book.pdf\" :mime-type \"application/pdf\" :size 192220))
 :references  (\"238C8384574032D81EE81AF0114E4E74@123213.mail.example.com\"
 \"6BDC23465F79238203498230942D81EE81AF0114E4E74@123213.mail.example.com\")
 :in-reply-to \"238203498230942D81EE81AF0114E4E74@123213.mail.example.com\"
 :body-txt \"Hi Tom, ...\"
\)).
Some notes on the format:
- The address fields are lists of pairs (NAME . EMAIL), where NAME can be nil.
- The date is in format emacs uses in `current-time'
- Attachments are a list of elements with fields :index (the number of
  the MIME-part), :name (the file name, if any), :mime-type (the
  MIME-type, if any) and :size (the size in bytes, if any).
- Messages in the Headers view come from the database and do not have
  :attachments, :body-txt or :body-html fields. Message in the
  Message view use the actual message file, and do include these fields."
  ;; after all this documentation, the spectacular implementation
  (if msg
      (plist-get msg field)
    (mu4e-error "Message must be non-nil")))

(defsubst mu4e-message-field (msg field)
  "Retrieve FIELD from message plist MSG.
Like `mu4e-message-field-nil', but will sanitize nil values:
- all string field except body-txt/body-html: nil -> \"\"
- numeric fields + dates                    : nil -> 0
- all others                                : return the value
Thus, function will return nil for empty lists, non-existing body-txt or body-html."
  (let ((val (mu4e-message-field-raw msg field)))
    (cond
     (val
      val)   ;; non-nil -> just return it
     ((member field '(:subject :message-id :path :maildir :in-reply-to))
      "")    ;; string fields except body-txt, body-html: nil -> ""
     ((member field '(:body-html :body-txt))
      val)
     ((member field '(:docid :size))
      0)     ;; numeric type: nil -> 0
     (t
      val)))) ;; otherwise, just return nil

(defsubst mu4e-message-has-field (msg field)
  "If MSG has a FIELD return t, nil otherwise."
  (plist-member msg field))

(defsubst mu4e-message-at-point (&optional noerror)
  "Get the message s-expression for the message at point.
Either the headers buffer or the view buffer, or nil if there is
no such message. If optional NOERROR is non-nil, do not raise an
error when there is no message at point."
  (let ((msg (or (get-text-property (point) 'msg) mu4e~view-message)))
    (if msg
        msg
      (unless noerror (mu4e-warn "No message at point")))))

(defsubst mu4e-message-field-at-point (field)
  "Get the field FIELD from the message at point.
This is equivalent to:
  (mu4e-message-field (mu4e-message-at-point) FIELD)."
  (mu4e-message-field (mu4e-message-at-point) field))

(defvar mu4e~message-body-html nil
  "Whether the body text uses HTML.")

(defun mu4e~message-use-html-p (msg prefer-html)
  "Do we want to PREFER-HTML for MSG?
Determine whether we want
to use html or text. The decision is based on PREFER-HTML and
whether the message supports the given representation."
  (let* ((txt (mu4e-message-field msg :body-txt))
         (html (mu4e-message-field msg :body-html))
         (txt-len (length txt))
         (html-len (length html))
         (txt-limit (* mu4e-view-html-plaintext-ratio-heuristic txt-len))
         (txt-limit (if (>= txt-limit 0) txt-limit most-positive-fixnum)))
    (cond
                                        ; user prefers html --> use html if there is
     (prefer-html (> html-len 0))
     ;; otherwise (user prefers text) still use html if there is not enough
     ;; text
     ((< txt-limit html-len) t)
     ;; otherwise, use text
     (t nil))))

(defun mu4e~message-body-has-content-type-param (msg param)
  "Does the MSG have a content-type parameter PARAM?"
  (cdr
   (assoc param (mu4e-message-field msg :body-txt-params))))

(defun mu4e~safe-iequal (a b)
  "Is string A equal to a downcased B?"
  (and b (equal (downcase b) a)))

(defun mu4e-message-body-text (msg &optional prefer-html)
  "Get the body in text form for message MSG.
This is either :body-txt, or if not available, :body-html
converted to text, using `mu4e-html2text-command' is non-nil, it
will use that. Normally, this function prefers the text part,
unless PREFER-HTML is non-nil."
  (setq mu4e~message-body-html (mu4e~message-use-html-p msg prefer-html))
  (let ((body
         (if mu4e~message-body-html
             ;; use an htmml body
             (cond
              ((stringp mu4e-html2text-command)
               (mu4e~html2text-shell msg mu4e-html2text-command))
              ((functionp mu4e-html2text-command)
               (if (help-function-arglist mu4e-html2text-command)
                   (funcall mu4e-html2text-command msg)
                 ;; oldskool parameterless mu4e-html2text-command
                 (mu4e~html2text-wrapper mu4e-html2text-command msg)))
              (t (mu4e-error "Invalid `mu4e-html2text-command'")))
           ;; use a text body
           (or (with-temp-buffer
                 (insert (or (mu4e-message-field msg :body-txt) ""))
                 (if (mu4e~safe-iequal "flowed"
                                       (mu4e~message-body-has-content-type-param
                                        msg "format"))
                     (fill-flowed nil
                                  (mu4e~safe-iequal
                                   "yes"
                                   (mu4e~message-body-has-content-type-param
                                    msg "delsp"))))
                 (buffer-string)) ""))))
    (dolist (func mu4e-message-body-rewrite-functions)
      (setq body (funcall func msg body)))
    body))

(defun mu4e-message-outlook-cleanup (_msg body)
  "Clean-up MSG's BODY.
Esp. MS-Outlook-originating message may not advertise the correct
encoding (e.g. 'iso-8859-1' instead of 'windows-1252'), thus
giving us these funky chars. here, we either remove them, or
replace with."
  (with-temp-buffer
    (insert body)
    (goto-char (point-min))
    (while (re-search-forward "\015 ]" nil t)
      (replace-match
       (cond
        ((string= (match-string 0) "") "'")
        ((string= (match-string 0) " ") " ")
        (t ""))))
    (buffer-string)))

(defun mu4e-message-contact-field-matches (msg cfield rx)
  "Does MSG's contact-field CFIELD match rx?
Check if any of the of the CFIELD in MSG matches RX. I.e.
anything in field CFIELD (either :to, :from, :cc or :bcc, or a
list of those) of msg MSG matches (with their name or e-mail
address) regular expressions RX. If there is a match, return
non-nil; otherwise return nil. RX can also be a list of regular
expressions, in which case any of those are tried for a match."
  (if (and cfield (listp cfield))
      (or (mu4e-message-contact-field-matches msg (car cfield) rx)
          (mu4e-message-contact-field-matches msg (cdr cfield) rx))
    (when cfield
      (if (listp rx)
          ;; if rx is a list, try each one of them for a match
          (cl-find-if
           (lambda (a-rx) (mu4e-message-contact-field-matches msg cfield a-rx))
           rx)
        ;; not a list, check the rx
        (cl-find-if
         (lambda (ct)
           (let ((name (car ct)) (email (cdr ct))
                 ;; the 'rx' may be some `/rx/` from mu4e-personal-addresses;
                 ;; so let's detect and extract in that case.
                 (rx (if (string-match-p  "^\\(.*\\)/$" rx)
                         (substring rx  1 -1) rx)))
             (or
              (and name  (string-match rx name))
              (and email (string-match rx email)))))
         (mu4e-message-field msg cfield))))))

(defun mu4e-message-contact-field-matches-me (msg cfield)
  "Does contact-field CFIELD in MSG match me?  Checks whether any
of the of the contacts in field CFIELD (either :to, :from, :cc or
:bcc) of msg MSG matches *me*, that is, any of the addresses for
which `mu4e-personal-address-p' return t. Returns the contact
cell that matched, or nil."
  (cl-find-if (lambda (cell) (mu4e-personal-address-p (cdr cell)))
                (mu4e-message-field msg cfield)))

(defun mu4e-message-sent-by-me (msg)
  "Is this message (to be) sent by me?
Checks if the from field matches user's personal addresses."
  (mu4e-message-contact-field-matches-me msg :from))

(defun mu4e-message-personal-p (msg)
  "Does message have user's personal address in any of the
contact fields?"
  (cl-some
   (lambda (field)
     (mu4e-message-contact-field-matches-me msg field))
   '(:from :to :cc :bcc)))

(defsubst mu4e-message-part-field  (msgpart field)
  "Get some FIELD from MSGPART.
A part would look something like:
  (:index 2 :name \"photo.jpg\" :mime-type \"image/jpeg\" :size 147331)."
  (plist-get msgpart field))

;; backward compatibility ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'mu4e-msg-field 'mu4e-message-field)
(defalias 'mu4e-body-text 'mu4e-message-body-text) ;; backward compatibility

(defun mu4e-field-at-point (field)
  "Get FIELD for the message at point.
Either in the headers buffer or the view buffer. Field is a
symbol, see `mu4e-header-info'."
  (plist-get (mu4e-message-at-point) field))

;;; Html2Text

(defun mu4e~html2text-wrapper (func msg)
  "Apply FUNC on a temporary buffer with html from MSG.
Return the buffer contents."
  (with-temp-buffer
    (insert (or (mu4e-message-field msg :body-html) ""))
    (funcall func)
    (or (buffer-string) "")))

(defun mu4e-shr2text (msg)
  "Convert html in MSG to text using the shr engine.
This can be used in `mu4e-html2text-command' in a new enough
Emacs. Based on code by Titus von der Malsburg."
  (mu4e~html2text-wrapper
   (lambda ()
     (let (
           ;; When HTML emails contain references to remote images,
           ;; retrieving these images leaks information. For example,
           ;; the sender can see when I opened the email and from which
           ;; computer (IP address). For this reason, it is preferable
           ;; to not retrieve images.
           ;; See this discussion on mu-discuss:
           ;; https://groups.google.com/forum/#!topic/mu-discuss/gr1cwNNZnXo
           (shr-inhibit-images t))
       (shr-render-region (point-min) (point-max)))) msg))

(defun mu4e~html2text-shell (msg _cmd)
  "Convert html2 text in MSG using a shell function CMD."
  (mu4e~html2text-wrapper
   (lambda ()
     (let* ((tmp-file (mu4e-make-temp-file "html")))
       (write-region (point-min) (point-max) tmp-file)
       (erase-buffer)
       (call-process-shell-command mu4e-html2text-command tmp-file t t)
       (delete-file tmp-file))) msg))

;;; _
(provide 'mu4e-message)
;;; mu4e-message.el ends here
