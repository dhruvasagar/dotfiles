(require 'package)

(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/"))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(
    evil
    evil-surround
    evil-numbers
    projectile-rails
    evil-rails
    evil-commentary
    evil-matchit
    evil-exchange
    evil-visualstar
    evil-collection

    railscasts-theme

    org
    org-evil

    markdown-mode))

(dolist (p my-packages)
  (if (not (package-installed-p p))
      (package-install p)))

(load-theme 'railscasts t nil)

(provide 'init-packages)
