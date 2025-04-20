(use-package evil
  :init
  (setq evil-want-keybinding nil
	evil-split-window-below t
	evil-vsplit-window-right t
	evil-want-integration t
	evil-want-fine-undo t)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-tree)
  (evil-select-search-module 'evil-search-module 'evil-search)
  :general
  (:states 'normal "C" "c$")
  (:states 'normal "Y" "y$")
  :bind
  (:map evil-normal-state-map
	("-" . dired-jump)
	("C-l" . (lambda ()
		   (interactive)
		   (redraw-display)
		   (evil-ex-nohighlight)))
	("C-c C-g" . evil-show-file-info)
	("C-c C-u" . evil-delete-back-to-indentation))
  :hook
  (org-mode . (lambda () (evil-local-set-key 'normal (kbd "RET") 'org-return))))

(use-package undo-tree
  :init
  (global-undo-tree-mode t)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/var/undo")))
  :bind
  ("C-x C-v" . undo-tree-visualize))

(use-package evil-collection
  :after evil
  :init
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))

(use-package evil-exchange
  :after evil
  :config
  (evil-exchange-cx-install))

(use-package evil-visualstar
  :after evil
  :config
  (global-evil-visualstar-mode t))

(use-package evil-args
  :after evil
  :bind
  (:map evil-inner-text-objects-map ("a" . evil-inner-arg))
  (:map evil-outer-text-objects-map ("a" . evil-outer-arg)))

(use-package evil-numbers
  :bind
  (:map evil-normal-state-map
	("C-c +" . evil-numbers/inc-at-pt)
	("C-c -" . evil-numbers/dec-at-pt)))

(use-package evil-anzu
  :after anzu)

(provide 'init-evil)
