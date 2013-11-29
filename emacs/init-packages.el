(require 'package)
(package-initialize)

(add-to-list 'package-archives
						 '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
						 '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))

(defvar my-packages
			'(
				evil
				surround evil-indent-textobject evil-leader evil-nerd-commenter evil-numbers evil-tabs
				auto-complete
				coffee-mode
				fiplr
				flymake-ruby
				haml-mode
				inf-ruby
				jabber
				jade-mode
				js2-mode
				less-css-mode
				magit
				rainbow-mode
				rhtml-mode
				rinari
				rspec-mode
				ruby-electric
				ruby-end
				ruby-hash-syntax
				sass-mode
				sws-mode
				web-mode
				yari
				yasnippet
				slime
				slime-js)
			"A list of packages to check for and install at launch.")

(defun my-missing-packages ()
  (let (missing-packages)
    (dolist (package my-packages missing-packages)
      (or (package-installed-p package)
					(push package missing-packages)))))

(defun ensure-my-packages ()
  (let ((missing (my-missing-packages)))
    (when missing
      ;; Check for new packages (package versions)
      (package-refresh-contents)
      ;; Install the missing packages
      (mapc (lambda (package)
							(when (not (package-installed-p package))
								(package-install package)))
						missing)
      ;; Close the compilation log.
      (let ((compile-window (get-buffer-window "*Compile-Log*")))
				(if compile-window
						(delete-window compile-window))))))

(ensure-my-packages)

(provide 'init-packages)
