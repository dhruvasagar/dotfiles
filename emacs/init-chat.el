(add-hook 'after-init-hook
					(lambda ()
						(require 'jabber)
						(require 'jabber-autoloads)

						(setq jabber-alert-presence-hooks nil
									jabber-auto-reconnect t)

						(require 'password-cache)
						(setq password-cache-expiry nil)
						
						(defun chat ()
							(interactive)
							(erc :server "irc.foonetic.net" :nick "dhruvasagar" :full-name "Dhruva Sagar" :password (password-read-and-add "foonetic irc pass : " "irc"))
							(jabber-connect-all))
						
						(setq erc-autojoin-channels-alist '(("foonetic.net" "#activesphere"))
									erc-try-new-nick-p nil
									erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

						(require 'notify)
						(defun my-notify-erc (match-type nickuserhost message)
							(message  message)
							"Notify when a message is received."
							(notify (format "%s in %s"
															;; Username of sender
															(car (split-string nickuserhost "!"))
															;; Channel
															(or (erc-default-target) "#unknown"))
											;; Remove duplicate spaces
											(replace-regexp-in-string " +" " " message)
											:icon "emacs-snapshot"
											:timeout -1))

						(add-hook 'erc-text-matched-hook 'my-notify-erc)))
(provide 'init-chat)
