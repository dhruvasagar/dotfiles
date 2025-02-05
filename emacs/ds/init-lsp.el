;;; init-lsp
(defun ds/lsp-describe-thing-at-point ()
  (interactive)
  (if (lsp-ui-doc--visible-p) (lsp-ui-doc-focus-frame) (lsp-ui-doc-glance)))

(defun lsp-custom-bindings ()
  (evil-local-set-key 'normal (kbd "K") 'lsp-describe-thing-at-point)
  (evil-local-set-key 'normal (kbd "g r") 'lsp-find-references)
  (evil-local-set-key 'normal (kbd "g I") 'lsp-find-implementation))

(defun lsp-format-on-save ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(defun ds/lsp-ui-settings ()
  (setq-local lsp-ui-doc-position 'at-point
	      lsp-ui-peek-fontify 'always
	      lsp-ui-doc-show-with-cursor nil
	      lsp-ui-doc-show-with-mouse nil
	      lsp-ui-peek-always-show t))

(defun ds/lsp-mode-setup ()
  (ds/lsp-ui-settings)
  (lsp-enable-which-key-integration)
  (lsp-custom-bindings))

;; (defun eglot-custom-bindings ()
;;   (evil-local-set-key 'normal (kbd "K") 'eldoc-box-eglot-help-at-point))

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
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-segments '(file symbols))
  (gc-cons-threshold (* 100 1024 1024))
  (read-process-output-max (* 1024 1024))
  :custom-face
  (lsp-face-highlight-read ((t (:underline t :background nil :foreground nil))))
  (lsp-face-highlight-write ((t (:underline t :background nil :foreground nil))))
  (lsp-face-highlight-textual ((t (:underline t :background nil :foreground nil))))
  :hook
  (ng2-mode . lsp)
  (ng2-ts-mode . lsp)
  (zig-mode . lsp)
  (c-ts-mode . lsp)
  (c++-ts-mode . lsp)
  (lua-ts-mode . lsp)
  (go-ts-mode . lsp)
  (js2-ts-mode . lsp)
  (java-ts-mode . lsp)
  (rust-ts-mode . lsp)
  (python-ts-mode . lsp)
  (haskell-ts-mode . lsp)
  (terraform-ts-mode . lsp)
  (typescript-ts-mode . lsp)
  (lsp-mode . ds/lsp-mode-setup)
  (lsp-completion-mode . (lambda () (setq-local completion-category-defaults nil))))

(use-package lsp-java
  :after lsp-mode
  :init
  (setenv "JAVA_HOME" (string-trim-right (shell-command-to-string "asdf where java"))))
(use-package lsp-haskell
  :after lsp-mode)

(use-package lsp-ui
  :after lsp
  :config
  (setq lsp-ui-doc-max-width 150
	lsp-ui-doc-max-height 30)
  :custom
  (lsp-ui-sideline t)
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


;; (defun ds/eglot-setup ()
;;   (eglot-custom-bindings)
;;   (setenv "JAVA_HOME" (string-trim-right (shell-command-to-string "asdf where java"))))

;; (require 'eglot)
;; (add-hook 'eglot-mode-hook #'ds/eglot-setup)
;; (add-hook 'java-ts-mode-hook #'eglot-ensure)
;; (add-hook 'rust-ts-mode-hook #'eglot-ensure)
;; (setq eglot-ignored-server-capabilities '(:hoverProvider))

;; (add-hook
;;  'eglot-managed-mode-hook
;;  (lambda ()
;;    ;; we want eglot to setup callbacks from eldoc, but we don't want eldoc
;;    ;; running after every command. As a workaround, we disable it after we just
;;    ;; enabled it. Now calling `M-x eldoc` will put the help we want in the eldoc
;;    ;; buffer. Alternatively we could tell eglot to stay out of eldoc, and add
;;    ;; the hooks manually, but that seems fragile to updates in eglot.
;;    (eldoc-mode -1)))

;; (use-package dape
;;   :preface
;;   ;; By default dape shares the same keybinding prefix as `gud'
;;   ;; If you do not want to use any prefix, set it to nil.
;;   (setq dape-key-prefix "\C-x\C-a")

;;   :hook
;;   ;; Save breakpoints on quit
;;   (kill-emacs . dape-breakpoint-save)
;;   ;; Load breakpoints on startup
;;    (after-init . dape-breakpoint-load)

;;   :config
;;   ;; Turn on global bindings for setting breakpoints with mouse
;;   (dape-breakpoint-global-mode)

;;   ;; Info buffers to the right
;;   (setq dape-buffer-window-arrangement 'right)

;;   ;; Info buffers like gud (gdb-mi)
;;   (setq dape-buffer-window-arrangement 'gud)
;;   (setq dape-info-hide-mode-line nil)

;;   ;; Pulse source line (performance hit)
;;   (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)

;;   ;; Showing inlay hints
;;   (setq dape-inlay-hints t)

;;   ;; Save buffers on startup, useful for interpreted languages
;;   (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

;;   ;; Kill compile buffer on build success
;;   ;; (add-hook 'dape-compile-hook 'kill-buffer)

;;   ;; Projectile users
;;   ;; (setq dape-cwd-fn 'projectile-project-root)
;;   )

;; Enable repeat mode for more ergonomic `dape' use
;; (use-package repeat
;;   :config
;;   (repeat-mode))

;; (use-package eldoc-box)

(provide 'init-lsp)
