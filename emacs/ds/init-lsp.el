;;; init-lsp
(defun ds/lsp-describe-thing-at-point ()
  (interactive)
  (if (lsp-ui-doc--visible-p) (lsp-ui-doc-focus-frame) (lsp-ui-doc-glance)))

(defun lsp-custom-bindings ()
  (evil-local-set-key 'normal (kbd "K") 'lsp-describe-thing-at-point)
  (evil-local-set-key 'normal (kbd "g r") 'lsp-find-references)
  (evil-local-set-key 'normal (kbd "g I") 'lsp-find-implementation))

(defun lsp-format-on-save ()
  ;; (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(defun ds/lsp-ui-settings ()
  (setq-local lsp-ui-doc-position 'at-point
	      lsp-ui-peek-fontify 'always
	      lsp-ui-doc-show-with-cursor nil
	      lsp-ui-doc-show-with-mouse nil
	      lsp-ui-peek-always-show t
	      lsp-ui-sideline-show-diagnostics nil))

(defun ds/lsp-mode-setup ()
  (ds/lsp-ui-settings)
  (lsp-enable-which-key-integration)
  (lsp-custom-bindings)
  (lsp-format-on-save))

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
  (lsp-signature-function #'lsp-signature-posframe)
  (lsp-semantic-tokens-enable t)
  (lsp-inlay-hint-enable t)
  ;; (lsp-eldoc-enable-hover t)
  ;; (lsp-diagnostic-package :none)
  (lsp-completion-provider :none)
  (lsp-file-watch-threshold 1000)  ; pyright has more than 1000
  (lsp-enable-links t)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-segments '(file symbols))
  (gc-cons-threshold (* 100 1024 1024))
  (read-process-output-max (* 1024 1024))
  :custom-face
  (lsp-face-highlight-read ((t (:underline t :background nil :foreground nil))))
  (lsp-face-highlight-write ((t (:underline t :background nil :foreground nil))))
  (lsp-face-highlight-textual ((t (:underline t :background nil :foreground nil))))
  :hook
  ((ng2-mode
   ng2-ts-mode
   zig-mode
   c-ts-mode
   c++-ts-mode
   lua-ts-mode
   go-ts-mode
   js2-ts-mode
   java-ts-mode
   rust-ts-mode
   python-ts-mode
   haskell-ts-mode
   terraform-ts-mode
   typescript-ts-mode) . lsp-deferred)
  (lsp-mode . ds/lsp-mode-setup)
  (lsp-completion-mode . (lambda () (setq-local completion-category-defaults nil))))

(use-package lsp-java
  :after lsp-mode
  :init
  (setenv "JAVA_HOME" (string-trim-right (shell-command-to-string "asdf where java")))
  :config
  (setq lsp-java-vmargs '("-Xmx4G")))

(use-package lsp-haskell
  :after lsp-mode)

(use-package lsp-pyright
  :custom
  (lsp-pyright-langserver-command "basedpyright"))

(use-package blacken
  :hook (python-mode . blacken-mode))

(use-package lsp-ui
  :after lsp
  :config
  (setq lsp-ui-doc-max-width 150
	lsp-ui-doc-max-height 30)
  :custom-face
  (lsp-ui-peek-highlight ((t (:inherit nil :background nil :foreground nil :weight semi-bold :box (:line-width -1))))))

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
  :config
  (dap-auto-configure-mode)
  :custom
  (dap-auto-configure-features '(locals breakpoints expressions))
  :bind
  (("C-c d d" . dap-debug)
   ("C-c d l" . dap-debug-last)
   ("C-c d r" . dap-debug-recent))
  :hook (dap-mode . ds/dap-custom-bindings))

(use-package lsp-docker)

(provide 'init-lsp)
