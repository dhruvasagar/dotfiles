(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(global-hl-line-mode)

;; Do not show default modeline until doom-modeline is loaded
(setq-default mode-line-format nil)

(blink-cursor-mode 0)

(setq inhibit-splash-screen t)

(global-display-line-numbers-mode)

(set-face-attribute 'default nil :height 150)

(add-hook 'emacs-startup-hook 'toggle-frame-maximized)

(add-to-list 'completion-styles 'flex t)

(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

(defun fk/split-window-below-and-switch ()
  "Split the window below, then switch to the new window."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun fk/split-window-right-and-switch ()
  "Split the window right, then switch to the new window."
  (interactive)
  (split-window-right)
  (other-window 1))

(provide 'ui)
