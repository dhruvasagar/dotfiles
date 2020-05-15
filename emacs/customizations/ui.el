(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(blink-cursor-mode 0)

(setq inhibit-splash-screen t)

(global-display-line-numbers-mode)

(set-face-attribute 'default nil :height 150)

(add-hook 'emacs-startup-hook 'toggle-frame-maximized)

(provide 'ui)
