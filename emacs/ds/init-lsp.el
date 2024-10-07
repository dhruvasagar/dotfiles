;;; init-lsp
(defun lsp-custom-bindings ()
  (evil-local-set-key 'normal (kbd "K") 'lsp-describe-thing-at-point)
  (evil-local-set-key 'normal (kbd "g r") 'lsp-find-references)
  (evil-local-set-key 'normal (kbd "g I") 'lsp-find-implementation))

(defun lsp-format-on-save ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(defun ds/lsp-ui-settings ()
  (setq-local lsp-ui-doc-position 'at-point
	      lsp-ui-sideline-show-hover t
	      lsp-ui-peek-fontify 'always
	      lsp-ui-doc-show-with-cursor nil
	      lsp-ui-doc-show-with-mouse nil
	      lsp-ui-peek-always-show t))

(defun ds/lsp-mode-cb ()
  (ds/lsp-ui-settings)
  (lsp-enable-which-key-integration)
  (lsp-custom-bindings))
  ;; (lsp-format-on-save))

(use-package lsp-mode
  :commands lsp
  :custom
  (lsp-auto-guess-root t)
  (lsp-auto-select-workspace t)
  (lsp-keymap-prefix "M-m l")
  (lsp-modeline-diagnostics-enable t)
  (lsp-keep-workspace-alive t)
  (lsp-auto-execute-action nil)
  (lsp-before-save-edits t)
  ;; (lsp-eldoc-enable-hover t)
  ;; (lsp-diagnostic-package :none)
  (lsp-completion-provider :none)
  (lsp-file-watch-threshold 1500)  ; pyright has more than 1000
  (lsp-enable-links t)
  (lsp-headerline-breadcrumb-enable nil)
  (gc-cons-threshold (* 100 1024 1024))
  (read-process-output-max (* 1024 1024))
  :custom-face
  (lsp-face-highlight-read ((t (:underline t :background nil :foreground nil))))
  (lsp-face-highlight-write ((t (:underline t :background nil :foreground nil))))
  (lsp-face-highlight-textual ((t (:underline t :background nil :foreground nil))))
  :hook
  (lsp-mode . ds/lsp-mode-cb)
  (lsp-completion-mode . (lambda () (setq-local completion-category-defaults nil))))

(use-package lsp-ui
  :after lsp
  :custom
  (lsp-ui-sideline t)
  :custom-face
  (lsp-ui-peek-highlight ((t (:inherit nil :background nil :foreground nil :weight semi-bold :box (:line-width -1)))))
  :bind
  ( :map lsp-ui-mode-map
    ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
    ([remap xref-find-references] . lsp-ui-peek-find-references)
    ("C-M-l" . lsp-ui-peek-find-definitions)
    ("C-c C-d" . lsp-ui-doc-show))
  :config
  (lsp-ui-mode 1))

(use-package lsp-pyright)

(use-package lsp-java
  :init
  (setenv "JAVA_HOME" (string-trim-right (shell-command-to-string "asdf where java")))
  :after lsp-mode
  :hook
  (java-mode . lsp)
  (java-ts-mode . lsp))

(defun ds/dap-custom-bindings ()
  (local-set-key (kbd "C-c d h") 'dap-hydra)
  (local-set-key (kbd "C-c d n") 'dap-next)
  (local-set-key (kbd "C-c d c") 'dap-continue)
  (local-set-key (kbd "C-c d q") 'dap-disconnect)
  (local-set-key (kbd "C-c d r") 'dap-ui-repl)
  (local-set-key (kbd "C-c d b a") 'dap-breakpoint-add)
  (local-set-key (kbd "C-c d b t") 'dap-breakpoint-toggle)
  (local-set-key (kbd "C-c d b d") 'dap-breakpoint-delete)
  (local-set-key (kbd "C-c d b a") 'dap-breakpoint-add)
  (local-set-key (kbd "C-c d b c") 'dap-breakpoint-condition)
  (local-set-key (kbd "C-c d b D") 'dap-breakpoint-delete-all)
  (local-set-key (kbd "C-c d e e") 'dap-eval)
  (local-set-key (kbd "C-c d e a") 'dap-ui-expression-add)
  (local-set-key (kbd "C-c d e r") 'dap-eval-region)
  (local-set-key (kbd "C-c d e s") 'dap-eval-thing-at-point)
  (local-set-key (kbd "C-c d s i") 'dap-step-in)
  (local-set-key (kbd "C-c d s o") 'dap-step-out))

(use-package dap-mode
  :after lsp-mode
  :config (dap-auto-configure-mode)
  :bind
  (("C-c d d" . dap-debug)
   ("C-c d l" . dap-debug-last)
   ("C-c d r" . dap-debug-recent))
  :hook (dap-mode . ds/dap-custom-bindings))

(provide 'init-lsp)
