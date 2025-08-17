(define-prefix-command 'ds/erc-map)
(define-key ctl-x-map "e" 'ds/erc-map)

(defun ds/erc-connect-libera ()
  (interactive)
  (erc-tls :server "irc.libera.chat" :port "6697"
	   :nick "dhruvasagar"))
(define-key ds/erc-map "s" 'ds/erc-connect-libera)
(define-key ds/erc-map "a" 'erc-track-switch-buffer)

(defun ds/erc-quit ()
    "Kill ERC buffers and terminate any child processes."
    (interactive)
    (let ((kill-buffer-query-functions nil)
	  (erc-buffers (erc-buffer-list)))
      (if (not erc-buffers)
	  (message "There are no ERC buffers to kill."))
      (progn
	(dolist (buffer erc-buffers)
	  (kill-buffer buffer))
	(message "Killed all ERC buffers."))))
(define-key ds/erc-map "q" 'ds/erc-quit)

(setopt erc-sasl-mechanism 'plain)
;; Join the #emacs and #erc channels whenever connecting to
;; Libera.Chat.
(setq erc-autojoin-channels-alist
      '(("Libera.Chat" "#vim" "#neovim" "#emacs" "#ruby" "#rust" "#golang")))

;; Interpret mIRC-style color commands in IRC chats
(setq erc-interpret-mirc-color t)

;; Ingore annoying (and unnecessary) Join, Part, Quit messages
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; The following are commented out by default, but users of other
;; non-Emacs IRC clients might find them useful.
;; Kill buffers for channels after /part
(setq erc-kill-buffer-on-part t)
;; Kill buffers for private queries after quitting the server
(setq erc-kill-queries-on-quit t)
;; Kill buffers for server messages after quitting the server
(setq erc-kill-server-buffer-on-quit t)

(add-hook 'erc-mode-hook (lambda () (local-set-key (kbd "C-<tab>") 'erc-track-switch-buffer)))

(provide 'init-erc)
