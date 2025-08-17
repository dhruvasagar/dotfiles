(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
(require 'smtpmail)

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials
      '(("mail.google.com" 587 nil nil))
      smtpmail-default-smtp-server "mail.google.com"
      smtpmail-smtp-server "mail.google.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t)

(require 'mu4e)

(require 'mu4e-alert)
(mu4e-alert-set-default-style 'notifier)

(setq mu4e-mu-binary "/usr/local/bin/mu")
(setq mu4e-maildir "~/mail")
(setq mu4e-headers-date-format "%Y-%m-%d %H:%M")
(setq message-citation-line-format "%N @ %Y-%m-%d %H:%M %Z:\n")
(setq message-citation-line-function 'message-insert-formatted-citation-line)

;; show full addresses in view message (instead of just names)
;; toggle per name with M-RET
(setq mu4e-view-show-addresses 't)

;; without this, "symbol's value as variable is void: mml2014-use" when signing
;; then found http://www.gnu.org/software/emacs/manual/html_node/gnus/Security.html
;; so set to epg and all was good!
;; to sign a mail: M-x mml-secure-sign-pgpmime
(setq mml2015-use 'epg)

;; the next are relative to `mu4e-maildir'
;; instead of strings, they can be functions too, see
;; their docstring or the chapter 'Dynamic folders'
(setq mu4e-sent-folder   "/Gmail/[Gmail].Sent Mail"
      mu4e-drafts-folder "/Gmail/[Gmail].Drafts"
      mu4e-trash-folder  "/Gmail/[Gmail].Trash")

;; don't save message to Sent Messages, IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; the maildirs you use frequently; access them with 'j' ('jump')
(setq   mu4e-maildir-shortcuts
	'( ("/Gmail/INBOX"             . ?i)
	   ("/Gmail/[Gmail].Important" . ?I)
	   ("/Gmail/[Gmail].Starred"   . ?s)
	   ("/Gmail/[Gmail].Drafts"    . ?d)))

;; list of my email addresses.
;; (setq mu4e-user-mail-address-list '("dhruva.sagar@gmail.com"
;;                                     "dhruva@tarkalabs.com"
;;                                     "dhruva.sagar@tunecore.com"))


;; when you want to use some external command for html->text
;; conversion, e.g. the 'html2text' program
;; (cpbotha: html2text sees to work better than the built-in one)
(setq mu4e-html2text-command 'mu4e-shr2text
      mu4e-update-interval 120
      mu4e-headers-auto-update t)

;; mu4e-action-view-in-browser is built into mu4e
;; by adding it to these lists of custom actions
;; it can be invoked by first pressing a, then selecting
(add-to-list 'mu4e-headers-actions
             '("in browser" . mu4e-action-view-in-browser) t)
(add-to-list 'mu4e-view-actions
             '("in browser" . mu4e-action-view-in-browser) t)


;; the headers to show in the headers list -- a pair of a field
;; and its width, with `nil' meaning 'unlimited'
;; (better only use that for the last field.
;; These are the defaults:
(setq mu4e-headers-fields
    '( (:date          .  25)
       (:flags         .   6)
       (:from          .  22)
       (:subject       .  nil)))

;; program to get mail; alternatives are 'fetchmail', 'getmail'
;; isync or your own shellscript. called when 'U' is pressed in
;; main view.

;; If you get your mail without an explicit command,
;; use "true" for the command (this is the default)
;; (setq mu4e-get-mail-command "true")

;; setup default identity here:
;; general emacs mail settings; used when composing e-mail
;; the non-mu4e-* stuff is inherited from emacs/message-mode
(setq mu4e-reply-to-address "dhruva.sagar@gmail.com"
      user-mail-address "dhruva.sagar@gmail.com"
      user-full-name  "Dhruva Sagar")

;; set this to nil so signature is not included by default
;; you can include in message with C-c C-w
(setq mu4e-compose-signature-auto-include 't)
(setq mu4e-compose-signature (with-temp-buffer
                               (insert-file-contents "~/dotfiles/signature.gmail.txt")
                               (buffer-string)))
;; message-signature-file NOT used by mu4e
(setq message-signature-file "~/.mu4e.signature.txt")

;; many recipes online use an alist with the different email identities
;; I like to use these functions, because then I have more flexibility
(defun cpb-mu4e-personal()
  (interactive)
  (message "personal mail account")
  (setq  user-mail-address "dhruva.sagar@gmail.com"
         mu4e-compose-signature (get-string-from-file "~/dotfilessignature.gmail.txt"))
  )

;; (defun cpb-mu4e-tarkalabs()
;;   (interactive)
;;   (message "tarkalabs mail account")
;;   (setq  user-mail-address "dhruva@tarkalabs.com"
;; 	 mu4e-compose-signature (get-string-from-file "~/.mu4e.signature.tarkalabs.txt"))
;;   )

;; (defun cpb-mu4e-tunecore()
;;   (interactive)
;;   (message "tunecore mail account")
;;   (setq  user-mail-address "dhruva.sagar@tunecore.com"
;;          mu4e-compose-signature (get-string-from-file "~/.mu4e.signature.tunecore.txt"))
;;   )

;; quickly change account. got his idea from:
;; https://github.com/skybert/my-little-friends/blob/master/emacs/.emacs.d/tkj-mu4e.el
;; (define-key mu4e-main-mode-map (kbd "<f1>") 'cpb-mu4e-personal)
;; (define-key mu4e-main-mode-map (kbd "<f2>") 'cpb-mu4e-tarkalabs)
;; (define-key mu4e-main-mode-map (kbd "<f4>") 'cpb-mu4e-tunecore)
;; (define-key mu4e-headers-mode-map (kbd "<f1>") 'cpb-mu4e-personal)
;; (define-key mu4e-headers-mode-map (kbd "<f2>") 'cpb-mu4e-tarkalsb)
;; (define-key mu4e-headers-mode-map (kbd "<f4>") 'cpb-mu4e-tunecore)


;; for sendmail read this http://www.gnus.org/manual/message_36.html
;; am using nullmailer, so my mail sending just became STUPID fast
(setq message-send-mail-function 'message-send-mail-with-sendmail)

;; smtp mail setting - if you DON'T want to use nullmailer, instead
;; connecting to your smtp server and waiting...
;; (setq
;;    message-send-mail-function 'smtpmail-send-it
;;    smtpmail-stream-type 'starttls
;;    smtpmail-default-smtp-server "mymailserver.com"
;;    smtpmail-smtp-server "mymailserver.com"
;;    smtpmail-smtp-service 587

;;    ;; if you need offline mode, set these -- and create the queue dir
;;    ;; with 'mu mkdir', i.e.. mu mkdir /home/user/Maildir/queue
;;    smtpmail-queue-mail  nil
;;    smtpmail-queue-dir  "/home/user/Maildir/queue/cur")

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)
;; attachments go here
(setq mu4e-attachment-dir  "~/Downloads")

;; when you reply to a message, use the identity that the mail was sent to
;; the cpbotha variation -- function that checks to, cc and bcc fields
(defun cpb-mu4e-is-message-to (msg rx)
  "Check if to, cc or bcc field in MSG has any address in RX."
  (or (mu4e-message-contact-field-matches msg :to rx)
      (mu4e-message-contact-field-matches msg :cc rx)
      (mu4e-message-contact-field-matches msg :bcc rx)))

;; we only do something if we recognize something (i.e. no stupid default)
;; (add-hook 'mu4e-compose-pre-hook
;;           (defun my-set-from-address ()
;;             "Set current identity based on to, cc, bcc of original."
;;             (let ((msg mu4e-compose-parent-message)) ;; msg is shorter...
;;               (if msg
;;                   (cond
;;                    ((cpb-mu4e-is-message-to msg (list "dhruva.sagar@gmail.com"))
;;                     (cpb-mu4e-personal))
;;                    ((cpb-mu4e-is-message-to msg (list "dhruva@tarkalabs.com"))
;;                     (cpb-mu4e-tarkalabs))
;;                    ((cpb-mu4e-is-message-to msg (list "dhruva.sagar@tunecore.com"))
;;                     (cpb-mu4e-tunecore)))))))

;; convenience function for starting the whole mu4e in its own frame
;; posted by the author of mu4e on the mailing list
(defun mu4e-in-new-frame ()
  "Start mu4e in new frame."
  (interactive)
  (select-frame (make-frame))
  (mu4e))

(provide 'init-mu4e)
