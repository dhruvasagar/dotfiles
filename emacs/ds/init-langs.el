(use-package python
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "-i --simple-prompt")
  (python-indent-guess-indent-offset-verbose nil)
  :bind
  ( :map python-mode-map
    ("C-c r" . python-indent-shift-right)
    ("C-c l" . python-indent-shift-left))
  :hook
  ;; With pyls:
  ;; pip install python-language-server flake8 pyls-black(optional) pyls-isort(optional)
  ;; With pyright
  ;; sudo npm install -g pyright && pip install flake8 black(optional) django-stubs(optional)
  ;; NOTE: these hooks runs in reverse order
  (python-mode . (lambda () (setq-local company-prescient-sort-length-enable nil)))
  (python-mode . (lambda () (unless (and buffer-file-name (file-in-directory-p buffer-file-name "~/.virtualenvs/"))
			      (flycheck-mode))))
  ;; importmagic runs ~100mb ipython process per python file, and it does not
  ;; always find imports, 60%-70% maybe. I stop using this, but still want to keep.
  ;; (python-mode . importmagic-mode)
  (python-mode . fk/activate-pyvenv)
  (python-mode . (lambda ()
		   (when (and (buffer-file-name)
			      (string=
			       (car (last (f-split (f-parent (buffer-file-name)))))
			       "tests"))
		     (fk/hide-second-level-blocks))))
  (python-mode . fk/tree-sitter-hl-mode)
  (python-mode . (lambda () (setq-local fill-column 88)))
  :config
  (defvar python-walrus-operator-regexp ":=")

  ;; Make walrus operator ":=" more visible
  (font-lock-add-keywords
   'python-mode
   `((,python-walrus-operator-regexp 0 'escape-glyph t))
   'set))

(use-package pip-requirements
  :mode (("\\.pip\\'" . pip-requirements-mode)
	 ("requirements[^z-a]*\\.txt\\'" . pip-requirements-mode)
	 ("requirements\\.in" . pip-requirements-mode))
  :config
  ;; Assign a non nil value to `pip-packages' to prevent fetching pip packages.
  (setq pip-packages '("ipython")))

(use-package json-mode
  :mode ("\\.json\\'" . json-mode))
(use-package json-navigator
  :commands json-navigator-navigate-region)

(use-package js2-mode
  :mode "\\.js\\'"
  :custom
  (js-indent-level 2)
  :hook
  (js2-mode . flycheck-mode))

(use-package tide
  :after (company flycheck)
  :hook ((typescript-ts-mode . tide-setup)
	 (tsx-ts-mode . tide-setup)
	 (typescript-ts-mode . tide-hl-identifier-mode)
	 (before-save . tide-format-before-save)))

(use-package ng2-mode)

(use-package go-mode
  ;; install go & go-tools, for arch based linux:
  ;; sudo pacman -S go go-tools
  :mode "\\.go\\'"
  :custom
  (gofmt-command "goimports")
  :hook
  (go-mode . flycheck-mode)
  (go-mode . (lambda () (require 'tree-sitter-langs) (tree-sitter-hl-mode)))
  (go-mode . (lambda () (fk/add-local-hook 'before-save-hook 'gofmt))))

(use-package cc-mode
  :bind
  ( :map c-mode-base-map
    ("C-c C-c" . fk/c-run)))

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :custom (markdown-header-scaling t)
  :bind
  ( :map markdown-mode-map
    ("M-n" . markdown-next-visible-heading)
    ("M-p" . markdown-previous-visible-heading)
    ("C-M-j" . markdown-follow-thing-at-point))
  :hook
  (markdown-mode . emojify-mode))

(use-package edit-indirect
  :after markdown-mode
  :bind ("C-c '" . markdown-edit-code-block))

(use-package poly-markdown
  :hook
  (markdown-mode . poly-gfm-mode))

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package docker-compose-mode
  :mode "docker-compose\\'")

(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :hook
  (yaml-mode . highlight-indent-guides-mode)
  (yaml-mode . display-line-numbers-mode))

(use-package docker
  :commands docker)

(use-package git-modes
  :mode (("/.gitignore\\'" . gitignore-mode)
	 ("/.dockerignore\\'" . gitignore-mode)))

(use-package terraform-mode
  :mode "\\.tf\\'")

(use-package kubernetes
  :commands (kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency 3600
	kubernetes-redraw-frequency 3600))

(use-package kubernetes-evil
  :after kubernetes)

(use-package d2-mode)

(use-package ledger-mode)

(use-package rust-mode)

(use-package plantuml-mode)

(use-package haskell-mode)

(use-package kotlin-mode)

(use-package julia-mode)

(use-package vimrc-mode
 :mode "\\.vim\\(rc\\)?\\'")

;; Lisp / Elisp
(use-package sly)
(use-package highlight-defined
  :hook (emacs-lis-mode . highlight-defined-mode))
(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package zig-mode)

(use-package clojure-mode)

(provide 'init-langs)
