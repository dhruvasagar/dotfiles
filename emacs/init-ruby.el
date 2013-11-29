(add-hook 'after-init-hook
					(lambda()
						(require 'yari)
						(require 'inf-ruby)
						(require 'rinari)
						(require 'rspec-mode)
						
						(setq rspec-spec-command "rspec")
						(setq rspec-use-rbenv t)
						(setq rspec-use-rake-flag nil)

						(require 'ruby-electric)
						(add-hook 'ruby-mode-hook 'ruby-electric-mode)

						(setq auto-mode-alist (append auto-mode-alist
																					'(("\\.rake$" . ruby-mode)
																						("\\.gemspec$" . ruby-mode)
																						("\\.ru$" . ruby-mode)
																						("\\.god$" . ruby-mode)
																						("Rakefile$" . ruby-mode)
																						("Gemfile$" . ruby-mode)
																						("capfile$" . ruby-mode)
																						("Capfile$" . ruby-mode)
																						("Vagrantfile$" . ruby-mode))))
						
						(require 'flymake-ruby)
						(add-hook 'ruby-mode-hook 'flymake-ruby-load)

						(require 'thingatpt)
						(defun jump-to-view ()
							"jumps to corresponding haml view"
							(interactive)
							(let ((sentence (thing-at-point 'sentence)))
								(if (string-match ".*haml(?[[:blank:]]*:\\(?:'\\|\"\\)?\\([^\"\', \f\t\n\r\v]+\\).*" sentence)
										(let ((file (match-string-no-properties 1 sentence)))
											(if (one-window-p)
													(split-window-vertically))
											(select-window (next-window))
											(find-file (concat (project-root) (concat "app/views/" (concat file ".haml"))))))))
						
						(require 'rhtml-mode)
						(require 'ruby-end)
						
						(defun pry ()
							(interactive)
							(run-ruby "pry" "pry")
							(setq comint-get-old-input #'pry-get-old-input))
						
						(defun pry-get-old-input ()
							(let ((inf-ruby-first-prompt-pattern "pry(.*).*> *"))
								(inf-ruby-get-old-input)))))
(provide 'init-ruby)
