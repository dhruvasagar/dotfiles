(global-set-key (kbd "C-c e s") (lambda () (interactive)
			   (erc-tls :server "irc.libera.chat" :port "6697"
				:nick "dhruvasagar")))

(global-set-key (kbd "C-c e a") 'erc-track-switch-buffer)

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
