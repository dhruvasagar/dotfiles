(add-hook 'emacs-setup-hook (lambda ()
			       (inhibit-startup-screen t)
			       (toggle-frame-maximized t)
			       (tool-bar-mode -1)
			       (scroll-bar-mode -1)
			       (menu-bar-mode -1)
			       ))

(global-hl-line-mode)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(global-visual-line-mode t)
(setq help-window-select t)
(set-frame-font "FiraCode Nerd Font Mono 15" nil t)
(setq tab-bar-show 1)
(winner-mode 1)
(tab-bar-history-mode 1)
(which-function-mode 1)

(setq default-frame-alist '((undecorated . t)))
(add-to-list 'default-frame-alist '(drag-internal-border . 1))
(add-to-list 'default-frame-alist '(internal-border-width . 5))

;; Do not show default modeline until doom-modeline is loaded
(setq-default mode-line-format nil)

(blink-cursor-mode 0)

(global-display-line-numbers-mode)

(set-face-attribute 'default nil :height 150)

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

;; (add-to-list 'display-buffer-alist
;;	     '("\\*sly-mrepl"
;;	       (display-buffer-at-bottom)
;;	       (window-height . 12)))
;; (add-to-list 'display-buffer-alist
;;	     '("\\*Calendar*"
;;	       (display-buffer-at-bottom)))
;; (add-to-list 'display-buffer-alist
;;	     '("\\*shell:"
;;	       (display-buffer-below-selected)
;;	       (window-height . 12)))
;; (add-to-list 'display-buffer-alist
;;	     '("\\*vterm*\\*"
;;	       (display-buffer-below-selected)
;;	       (window-height . 12)))

(defun ds/common-window-bounds ()
  "Return start and end points in the window as a cons cell."
  (cons (window-start) (window-end)))

(defun ds/common-window-small-p ()
  "Return non-nil if window is small.
Check if the `window-width' or `window-height' is less than
`split-width-threshold' and `split-height-threshold',
respectively."
  (or (and (numberp split-width-threshold)
	   (< (window-total-width) split-width-threshold))
      (and (numberp split-height-threshold)
	   (> (window-total-height) split-height-threshold))))

(defun ds/common-window-narrow-p ()
  "Return non-nil if window is narrow.
Check if the `window-width' is less than `split-width-threshold'."
  (and (numberp split-width-threshold)
       (< (window-total-width) split-width-threshold)))

;;;###autoload
(defun ds/common-three-or-more-windows-p (&optional frame)
  "Return non-nil if three or more windows occupy FRAME.
If FRAME is non-nil, inspect the current frame."
  (>= (length (window-list frame :no-minibuffer)) 3))

(defvar ds/window-window-sizes
  '( :max-height (lambda () (floor (frame-height) 3))
     :min-height 10
     :max-width (lambda () (floor (frame-width) 4))
     :min-width 20)
  "Property list of maximum and minimum window sizes.
The property keys are `:max-height', `:min-height', `:max-width',
and `:min-width'.  They all accept a value of either a
number (integer or floating point) or a function.")

(defun ds/window--get-window-size (key)
  "Extract the value of KEY from `ds/window-window-sizes'."
  (when-let ((value (plist-get ds/window-window-sizes key)))
    (cond
     ((functionp value)
      (funcall value))
     ((numberp value)
      value)
     (t
      (error "The value of `%s' is neither a number nor a function" key)))))

(defun ds/window-select-fit-size (window)
  "Select WINDOW and resize it.
The resize pertains to the maximum and minimum values for height
and width, per `ds/window-window-sizes'.

Use this as the `body-function' in a `display-buffer-alist' entry."
  (select-window window)
  (fit-window-to-buffer
   window
   (ds/window--get-window-size :max-height)
   (ds/window--get-window-size :min-height)
   (ds/window--get-window-size :max-width)
   (ds/window--get-window-size :min-width))
  ;; If we did not use `display-buffer-below-selected', then we must
  ;; be in a lateral window, which has more space.  Then we do not
  ;; want to dedicate the window to this buffer, because we will be
  ;; running out of space.
  (when (or (window-in-direction 'above) (window-in-direction 'below))
    (set-window-dedicated-p window t)))

(defun ds/window--get-display-buffer-below-or-pop ()
  "Return list of functions for `ds/window-display-buffer-below-or-pop'."
  (list
   #'display-buffer-reuse-mode-window
   (if (or (ds/common-window-small-p)
	   (ds/common-three-or-more-windows-p))
       #'display-buffer-below-selected
     #'display-buffer-pop-up-window)))

(defun ds/window-display-buffer-below-or-pop (&rest args)
  "Display buffer below current window or pop a new window.
The criterion for choosing to display the buffer below the
current one is a non-nil return value for
`ds/common-window-small-p'.

Apply ARGS expected by the underlying `display-buffer' functions.

This as the action function in a `display-buffer-alist' entry."
  (let ((functions (ds/window--get-display-buffer-below-or-pop)))
    (catch 'success
      (dolist (fn functions)
	(when (apply fn args)
	  (throw 'success fn))))))

(defun ds/window-shell-or-term-p (buffer &rest _)
  "Check if BUFFER is a shell or terminal.
This is a predicate function for `buffer-match-p', intended for
use in `display-buffer-alist'."
  (when (string-match-p "\\*.*\\(e?shell\\|v?term\\).*" (buffer-name (get-buffer buffer)))
    (with-current-buffer buffer
      ;; REVIEW 2022-07-14: Is this robust?
      (and (not (derived-mode-p 'message-mode 'text-mode))
	   (derived-mode-p 'eshell-mode 'shell-mode 'comint-mode 'fundamental-mode)))))

(setq display-buffer-alist
      `(;; no window
	("\\`\\*Async Shell Command\\*\\'"
	 (display-buffer-no-window))
	("\\`\\*\\(Warnings\\|Compile-Log\\|Org Links\\)\\*\\'"
	 (display-buffer-no-window)
	 (allow-no-window . t))
	;; bottom side window
	("\\*Org \\(Select\\|Note\\)\\*" ; the `org-capture' key selection and `org-add-log-note'
	 (display-buffer-in-side-window)
	 (dedicated . t)
	 (side . bottom)
	 (slot . 0)
	 (window-parameters . ((mode-line-format . none))))
	;; bottom buffer (NOT side window)
	((or . ((derived-mode . flymake-diagnostics-buffer-mode)
		(derived-mode . flymake-project-diagnostics-mode)
		(derived-mode . messages-buffer-mode)
		(derived-mode . backtrace-mode)))
	 (display-buffer-reuse-mode-window display-buffer-at-bottom)
	 (window-height . 0.3)
	 (dedicated . t)
	 (preserve-size . (t . t)))
	("\\*Embark Actions\\*"
	 (display-buffer-reuse-mode-window display-buffer-below-selected)
	 (window-height . fit-window-to-buffer)
	 (window-parameters . ((no-other-window . t)
			       (mode-line-format . none))))
	("\\*\\(Man\\|Help\\|helpful\\|lsp-help\\).*"
	 (display-buffer-reuse-mode-window display-buffer-below-selected))
	("\\*\\(shell\\|vterm\\|eat\\)*"
	 (display-buffer-reuse-mode-window display-buffer-below-selected))
	("\\*\\(Output\\|Register Preview\\).*"
	 (display-buffer-reuse-mode-window display-buffer-at-bottom))
	;; below current window
	("\\(\\*Capture\\*\\|CAPTURE-.*\\)"
	 (display-buffer-reuse-mode-window display-buffer-below-selected))
	("\\*\\vc-\\(incoming\\|outgoing\\|git : \\).*"
	 (display-buffer-reuse-mode-window display-buffer-below-selected)
	 (window-height . 0.1)
	 (dedicated . t)
	 (preserve-size . (t . t)))
	((derived-mode . reb-mode) ; M-x re-builder
	 (display-buffer-reuse-mode-window display-buffer-below-selected)
	 (window-height . 4) ; note this is literal lines, not relative
	 (dedicated . t)
	 (preserve-size . (t . t)))
	((or . ((derived-mode . occur-mode)
		(derived-mode . grep-mode)
		(derived-mode . Buffer-menu-mode)
		(derived-mode . log-view-mode)
		(derived-mode . help-mode) ; See the hooks for `visual-line-mode'
		"\\*\\(|Buffer List\\|Occur\\|vc-change-log\\|eldoc.*\\).*"
		ds/window-shell-or-term-p
		;; ,world-clock-buffer-name
		))
	 (ds/window-display-buffer-below-or-pop)
	 (body-function . ds/window-select-fit-size))
	("\\*\\(Calendar\\|Bookmark Annotation\\|ert\\).*"
	 (display-buffer-reuse-mode-window display-buffer-below-selected)
	 (dedicated . t)
	 (window-height . fit-window-to-buffer))
	;; NOTE 2022-09-10: The following is for `ispell-word', though
	;; it only works because I override `ispell-display-buffer'
	;; with `ds/spell-ispell-display-buffer' and change the
	;; value of `ispell-choices-buffer'.
	("\\*ispell-top-choices\\*.*"
	 (display-buffer-reuse-mode-window display-buffer-below-selected)
	 (window-height . fit-window-to-buffer))
	;; same window

	;; NOTE 2023-02-17: `man' does not fully obey the
	;; `display-buffer-alist'.  It works for new frames and for
	;; `display-buffer-below-selected', but otherwise is
	;; unpredictable.  See `Man-notify-method'.
	((or . ((derived-mode . Man-mode)
		(derived-mode . woman-mode)
		"\\*\\(Man\\|woman\\).*"))
	 (display-buffer-same-window))))

(provide 'ui)
