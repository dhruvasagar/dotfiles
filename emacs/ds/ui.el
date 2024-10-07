(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(global-hl-line-mode)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(global-visual-line-mode t)
(set-frame-font "FiraCode Nerd Font Mono 15" nil t)
(setq tab-bar-show 1)
(winner-mode 1)
(tab-bar-history-mode 1)

(setq default-frame-alist '((undecorated . t)))
(add-to-list 'default-frame-alist '(drag-internal-border . 1))
(add-to-list 'default-frame-alist '(internal-border-width . 5))

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

(add-to-list 'display-buffer-alist
	     '("\\*sly-mrepl"
	       (display-buffer-at-bottom)
	       (window-height . 12)))
(add-to-list 'display-buffer-alist
	     '("\\*Calendar*"
	       (display-buffer-at-bottom)))
(add-to-list 'display-buffer-alist
	     '("\\*shell:"
	       (display-buffer-below-selected)
	       (window-height . 12)))
(add-to-list 'display-buffer-alist
	     '("\\magit:"
	       (display-buffer-same-window)))
(add-to-list 'display-buffer-alist
	     '("\\*Man"
	       (display-buffer-same-window)))
(add-to-list 'display-buffer-alist
	     '("\\*Help"
	       (display-buffer-same-window)))
(add-to-list 'display-buffer-alist
	     '("\\*helpful"
	       (display-buffer-same-window)))
(add-to-list 'display-buffer-alist
	     '("\\*lsp-help"
	       (display-buffer-same-window)))

(provide 'ui)
