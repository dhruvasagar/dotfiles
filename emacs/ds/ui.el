(toggle-frame-maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-hl-line-mode)
(pixel-scroll-mode)
(set-frame-font "FiraCode Nerd Font Mono 14" nil t)
(progn
  (set-frame-parameter (selected-frame) 'alpha '(100 . 100))
  (add-to-list 'default-frame-alist '(alpha . (100 . 100))))
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(set-default 'truncate-lines t)
(global-visual-line-mode t)
(setq help-window-select t)
(setq tab-bar-show 1)
(winner-mode 1)
(tab-bar-history-mode 1)

(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 0)
(window-divider-mode +1)

(defconst jetbrains-ligature-mode--ligatures
  '("-->" "//" "/**" "/*" "*/" "<!--" ":=" "->>" "<<-" "->" "<-"
    "<=>" "==" "!=" "<=" ">=" "=:=" "!==" "&&" "||" "..." ".."
    "|||" "///" "&&&" "===" "++" "--" "=>" "|>" "<|" "||>" "<||"
    "|||>" "<|||" ">>" "<<" "::=" "|]" "[|" "{|" "|}"
    "[<" ">]" ":?>" ":?" "/=" "[||]" "!!" "?:" "?." "::"
    "+++" "??" "###" "##" ":::" "####" ".?" "?=" "=!=" "<|>"
    "<:" ":<" ":>" ">:" "<>" "***" ";;" "/==" ".=" ".-" "__"
    "=/=" "<-<" "<<<" ">>>" "<=<" "<<=" "<==" "<==>" "==>" "=>>"
    ">=>" ">>=" ">>-" ">-" "<~>" "-<" "-<<" "=<<" "---" "<-|"
    "<=|" "/\\" "\\/" "|=>" "|~>" "<~~" "<~" "~~" "~~>" "~>"
    "<$>" "<$" "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</>" "</" "/>"
    "<->" "..<" "~=" "~-" "-~" "~@" "^=" "-|" "_|_" "|-" "||-"
    "|=" "||=" "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#="
    "&="))

(sort jetbrains-ligature-mode--ligatures (lambda (x y) (> (length x) (length y))))

(dolist (pat jetbrains-ligature-mode--ligatures)
  (set-char-table-range composition-function-table
			(aref pat 0)
			(nconc (char-table-range composition-function-table (aref pat 0))
			       (list (vector (regexp-quote pat)
					     0
					     'compose-gstring-for-graphic)))))

(setq compilation-scroll-output t
      compilation-always-kill t)

(setq blink-matching-paren t
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t
      show-paren-context-when-offscreen t)
(show-paren-mode 1)
(setq electric-pair-preserve-balance t
      electric-pair-delete-adjacent-pairs t
      electric-pair-open-newline-between-pairs t
      electric-pair-skip-whitespace t)
(electric-pair-mode t)

(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
	((region-active-p)
	 (narrow-to-region (region-beginning)
			   (region-end)))
	((derived-mode-p 'org-mode)
	 ;; `org-edit-src-code' is not a real narrowing
	 ;; command. Remove this first conditional if
	 ;; you don't want it.
	 (cond ((ignore-errors (org-edit-src-code) t)
		(delete-other-windows))
	       ((ignore-errors (org-narrow-to-block) t))
	       (t (org-narrow-to-subtree))))
	((derived-mode-p 'latex-mode)
	 (LaTeX-narrow-to-environment))
	(t (narrow-to-defun))))

(evil-global-set-key 'normal (kbd "C-x n t") 'narrow-or-widen-dwim)

(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (define-key LaTeX-mode-map "\C-xn"
	      nil)))

(which-function-mode 1)
(setq which-func-format
      `(" "
	(:propertize which-func-current local-map
		     (keymap
		      (mode-line keymap
				 (mouse-3 . end-of-defun)
				 (mouse-2 . narrow-to-defun)
				 (mouse-1 . beginning-of-defun)))
		     face which-func
		     mouse-face mode-line-highlight
		     help-echo "mouse-1: go to beginning\n\
mouse-2: toggle rest visibility\n\
mouse-3: go to end")
	" "))
;; Set the window-function-mode details in the headerline but the mouse doesn't work
;; (setq-default header-line-format
;;	      '((which-func-mode ("" which-func-format " "))))
(setq default-frame-alist '((undecorated . t)))
(add-to-list 'default-frame-alist '(drag-internal-border . 1))
(add-to-list 'default-frame-alist '(internal-border-width . 5))

;; Do not show default modeline until doom-modeline is loaded
(setq-default mode-line-format nil)

(blink-cursor-mode 0)

(global-display-line-numbers-mode)

(set-face-attribute 'default nil :height 150)

(setq completion-ignore-case t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)

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
	("\\*compilation\\*"
	 (display-buffer-in-side-window)
	 (dedicated . t)
	 (side . right))
	("\\*\\(Org\\|Org Agenda.*\\)"
	 (display-buffer-reuse-mode-window display-buffer-below-selected)
	 (dedicated . t)
	 (window-height . 0.5)
	 (preserve-size . (t . t)))
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
	("\\*\\(Man\\|Help\\|helpful\\|lsp-help\\|eldoc\\).*"
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
	   ("\\*grep\\*"
	    (display-buffer-no-window)
	    (allow-no-window . t))
	   ((or . ((derived-mode . Man-mode)
		   (derived-mode . woman-mode)
		   "\\*\\(Man\\|woman\\).*"))
	    (display-buffer-same-window))))

;; Better ediff experience
(setq ediff-diff-options ""
      ediff-make-buffers-readonly-at-startup nil
      ediff-show-clashes-only t
      ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

(provide 'ui)
