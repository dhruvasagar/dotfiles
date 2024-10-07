(use-package evil
  :init
  (setq evil-want-keybinding nil
	evil-split-window-below t
	evil-vsplit-window-right t
	evil-want-integration t
	evil-want-fine-undo t
	evil-want-minibuffer t)
  :config
  (evil-mode 1)
  (evil-global-set-key 'normal "C" "c$")
  (evil-global-set-key 'normal "Y" "y$")
  (evil-set-undo-system 'undo-tree)
  :bind
  (:map evil-normal-state-map
	("-" . dired-jump)
	("C-l" . redraw-display)
	("C-c C-g" . evil-show-file-info)
	("C-c C-u" . evil-delete-back-to-indentation)))

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
  :config
  (global-evil-surround-mode 1))

(use-package evil-rails)

(use-package evil-commentary
  :config
  (evil-commentary-mode))

(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))

(use-package evil-exchange
  :config
  (evil-exchange-cx-install))

(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode t))

(provide 'init-evil)
