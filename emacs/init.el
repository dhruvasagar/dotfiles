;; Custom Vars
(defvar emacs-home "~/.emacs.d")
(add-to-list 'load-path emacs-home)

;; Custom Functions
(defun find-user-init-file ()
	(interactive)
  (find-file user-init-file))

(defun switch-to-previous-buffer ()
	(interactive)
	(switch-to-buffer (other-buffer (current-buffer) 1)))

;; load & install packages
(require 'init-packages)

;; load all extensions recursively
(let ((default-directory "~/.emacs.d/extensions/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; load railscasts-theme
(require 'railscasts-theme)
(require 'auto-complete)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu t)
 '(ac-auto-start t)
 '(ac-show-menu-immediately-on-auto-complete t)
 '(nrepl-hide-special-buffers t)
 '(nrepl-popup-stacktraces-in-repl t)
 '(recentf-max-saved-items 50))

;; Global Settings
(evil-mode 1)
(require 'evil-rails)
(global-surround-mode 1)
(evilnc-default-hotkeys)
(global-evil-leader-mode)
(evil-ex-define-cmd "Exp[lore]" 'dired-jump)
(evil-ex-define-cmd "color[scheme]" 'customize-themes)

(setq sml/theme 'dark)(sml/setup)
(setq show-trailing-whitespace t)
(global-auto-complete-mode t)
(global-auto-revert-mode t)
(tool-bar-mode -1)
(show-paren-mode 1)(setq show-paren-delay 0)
(global-linum-mode 1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(blink-cursor-mode t)
(column-number-mode t)
(setq-default tab-width 2)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq inhibit-startup-message t)
(put 'narrow-to-region 'disabled nil)

(require 'init-js)
(require 'init-css)
(require 'init-ido)
(require 'init-org)
(require 'init-ruby)
(require 'init-global-keys)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
