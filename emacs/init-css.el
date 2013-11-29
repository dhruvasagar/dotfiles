(add-hook 'after-init-hook
					(lambda()
						(require 'sass-mode)
						(require 'haml-mode)
						(require 'rainbow-mode)
						(require 'less-css-mode)

						(setq scss-compile-at-save nil)

						(add-hook 'css-mode-hook 'rainbow-turn-on)
						(add-hook 'sass-mode-hook 'rainbow-turn-on)
						(add-hook 'scss-mode-hook (lambda()
																				(setq indent-tabs-mode nil)))))

(provide 'init-css)
