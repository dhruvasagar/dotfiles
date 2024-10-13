(defun ds/embark-act-no-quit ()
  "Call `embark-act' but do not quit after the action."
  (interactive)
  (let ((embark-quit-after-action nil))
    (call-interactively #'embark-act)))

(defun ds/embark-act-quit ()
  "Call `embark-act' and quit after the action."
  (interactive)
  (let ((embark-quit-after-action t))
    (call-interactively #'embark-act))
  (when (and (> (minibuffer-depth) 0)
	     (derived-mode-p 'completion-list-mode))
    (abort-recursive-edit)))

(use-package embark
  :ensure t

  :bind
  ;;  (("C-SPC" . embark-act)          ;; pick some comfortable binding
  ;;   ("C-S-SPC" . embark-dwim)       ;; good alternative: M-.
  ;;   ("C-h B" .
  ;; embark-bindings)     ;; alternative for `describe-bindings'
  ( :map minibuffer-local-map
    ("C-c C-c" . embark-collect)
    ("C-c C-e" . embark-export))
  ( :map global-map
    ("C-," . ds/embark-act-no-quit)
    ("C-." . ds/embark-act-quit)
    ("C-h B" . embark-bindings)
    :map embark-collect-mode-map
    ("C-," . ds/embark-act-no-quit)
    ("C-." . ds/embark-act-quit)
    :map minibuffer-local-filename-completion-map
    ("C-," . ds/embark-act-no-quit)
    ("C-." . ds/embark-act-quit))
  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command
	embark-indicators '())

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  ;; :config

  ;; Add identifiers in LSP-mode as their own target type
  ;; (with-eval-after-load 'lsp-mode
  ;;   (defun embark-target-lsp-identifier-at-point ()
  ;;     (when lsp-mode
  ;;       (when-let ((sym (embark-target-identifier-at-point)))
  ;;         (cons 'lsp-identifier (cdr sym)))))
  ;;   (add-to-list 'embark-target-finders 'embark-target-lsp-identifier-at-point)
  ;;   (embark-define-keymap embark-lsp-identifier-actions
  ;;     "Keymap for actions on LSP identifiers."
  ;;     :parent embark-identifier-map
  ;;     ("a" lsp-execute-code-action))
  ;;   (add-to-list 'embark-keymap-alist '(lsp-identifier . embark-lsp-identifier-actions))
  ;;   (add-to-list 'embark-target-injection-hooks '(lsp-execute-code-action embark--ignore-target)))

  ;; Hide the mode line of the Embark live/completions buffers
  ;; (add-to-list 'display-buffer-alist
  ;;              '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
  ;;                nil
  ;;                (window-parameters (mode-line-format . none))))
  )

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-embark)
