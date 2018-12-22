(add-to-list 'load-path "~/.emacs.d/customizations")

(require 'init-packages)
(require 'ui)
(require 'init-evil)

(setq backup-inhibited t)
(setq auto-save-default nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (railscasts-theme org-evil evil-visualstar evil-surround evil-rails evil-numbers evil-matchit evil-exchange evil-commentary))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
