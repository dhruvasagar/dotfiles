(use-package python
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "-i --simple-prompt")
  (python-indent-guess-indent-offset-verbose nil)
  :bind
  ( :map python-mode-map
    ("C-c r" . python-indent-shift-right)
    ("C-c l" . python-indent-shift-left))
  :config
  (defvar python-walrus-operator-regexp ":=")

  ;; Make walrus operator ":=" more visible
  (font-lock-add-keywords
   'python-mode
   `((,python-walrus-operator-regexp 0 'escape-glyph t))
   'set))

(use-package python-pytest
  :general
  (:states 'normal :keymaps 'python-base-mode-map "C-c C-t C-d" 'python-pytest-dispatch))

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

(use-package gotest)

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :custom
  ;; (markdown-header-scaling t)
  (markdown-command "multimarkdown")
  (markdown-fontify-code-blocks-natively t)
  :bind
  ( :map markdown-mode-map
    ("M-n" . markdown-next-visible-heading)
    ("M-p" . markdown-previous-visible-heading)
    ("C-M-j" . markdown-follow-thing-at-point))
  :custom-face
  (markdown-header-delimiter-face ((t (:foreground "#616161" :height 0.9))))
  (markdown-header-face-1 ((t (:height 1.6  :foreground "#A3BE8C" :weight extra-bold :inherit markdown-header-face))))
  (markdown-header-face-2 ((t (:height 1.4  :foreground "#EBCB8B" :weight extra-bold :inherit markdown-header-face))))
  (markdown-header-face-3 ((t (:height 1.2  :foreground "#D08770" :weight extra-bold :inherit markdown-header-face))))
  (markdown-header-face-4 ((t (:height 1.15 :foreground "#BF616A" :weight bold :inherit markdown-header-face))))
  (markdown-header-face-5 ((t (:height 1.1  :foreground "#b48ead" :weight bold :inherit markdown-header-face))))
  (markdown-header-face-6 ((t (:height 1.05 :foreground "#5e81ac" :weight semi-bold :inherit markdown-header-face))))
  :hook
  (markdown-mode . abbrev-mode))

(use-package edit-indirect
  :after markdown-mode
  :bind ("C-c '" . markdown-edit-code-block))

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package docker-compose-mode
  :mode "docker-compose\\'")

(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :hook
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
