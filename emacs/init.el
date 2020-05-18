
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
; (package-initialize)

(add-to-list 'load-path "~/.emacs.d/customizations")

(require 'init-packages)
(require 'ui)
(require 'init-evil)

(setq backup-inhibited t)
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq auto-save-default nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (mu4e-alert evil-collection railscasts-theme org-evil evil-visualstar evil-surround evil-rails evil-numbers evil-matchit evil-exchange evil-commentary))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
