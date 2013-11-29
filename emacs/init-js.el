(add-hook 'after-init-hook
					(lambda()
						(require 'js2-mode)
						(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
						(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
						
						(setq js2-consistent-level-indent-inner-bracket-p t)
						(setq js-indent-level 8)
						
						(add-hook 'js2-mode-hook (lambda()
																			 (setq indent-tabs-mode nil)))
						
						(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
						(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))))
(provide 'init-js)
