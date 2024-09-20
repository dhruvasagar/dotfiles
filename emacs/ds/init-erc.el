(global-set-key "\C-cef" (lambda () (interactive)
			   (erc-tls :server "irc.libera.chat" :port "6697"
				:nick "dhruvasagar")))

;; Join the #emacs and #erc channels whenever connecting to
;; Libera.Chat.
(setq erc-autojoin-channels-alist
      '(("Libera.Chat" "vim" "neovim" "#emacs" "#ruby" "#rust" "#golang")))

;; Interpret mIRC-style color commands in IRC chats
(setq erc-interpret-mirc-color t)

;; The following are commented out by default, but users of other
;; non-Emacs IRC clients might find them useful.
;; Kill buffers for channels after /part
(setq erc-kill-buffer-on-part t)
;; Kill buffers for private queries after quitting the server
(setq erc-kill-queries-on-quit t)
;; Kill buffers for server messages after quitting the server
(setq erc-kill-server-buffer-on-quit t)

(provide 'init-erc)
