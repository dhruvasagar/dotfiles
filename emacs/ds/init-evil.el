(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-tree)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (setq evil-want-integration t)
  :config
  (evil-mode 1)
  (evil-global-set-key 'normal "C" "c$")
  (evil-global-set-key 'normal "Y" "y$")
  :bind
  (:map evil-normal-state-map
	("-" . dired-jump)
	("C-l" . redraw-display)
	("C-c C-g" . evil-show-file-info)
	("C-c C-u" . evil-delete-back-to-indentation)))

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
