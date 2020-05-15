(setq evil-want-keybinding nil)

(require 'evil)
(evil-mode 1)

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'evil-numbers)
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)
(define-key evil-normal-state-map (kbd "C-l") 'redraw-display)

(require 'evil-rails)

(require 'evil-commentary)
(evil-commentary-mode)

(require 'evil-matchit)
(global-evil-matchit-mode 1)

(require 'evil-exchange)
(evil-exchange-cx-install)

(require 'evil-visualstar)
(global-evil-visualstar-mode t)

(require 'evil-collection)
(evil-collection-init)

(require 'evil-magit)

(provide 'init-evil)
