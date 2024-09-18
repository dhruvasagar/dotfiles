(defun lsp-custom-bindings ()
  (define-key evil-normal-state-map (kbd "K") 'lsp-describe-thing-at-point)
  (define-key evil-normal-state-map (kbd "g d") 'lsp-find-definition)
  (define-key evil-normal-state-map (kbd "g r") 'lsp-find-references)
  (define-key evil-normal-state-map (kbd "g I") 'lsp-find-implementation))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :custom
  (lsp-auto-guess-root nil)
  (lsp-auto-select-workspace t)
  (lsp-keymap-prefix "M-m l")
  (lsp-modeline-diagnostics-enable nil)
  (lsp-keep-workspace-alive nil)
  (lsp-auto-execute-action nil)
  (lsp-before-save-edits nil)
  (lsp-eldoc-enable-hover nil)
  (lsp-diagnostic-package :none)
  (lsp-completion-provider :none)
  (lsp-file-watch-threshold 1500)  ; pyright has more than 1000
  (lsp-enable-links nil)
  (lsp-headerline-breadcrumb-enable nil)
  ;; Maybe set in future:
  ;;(lsp-enable-on-type-formatting nil)
  :custom-face
  (lsp-face-highlight-read ((t (:underline t :background nil :foreground nil))))
  (lsp-face-highlight-write ((t (:underline t :background nil :foreground nil))))
  (lsp-face-highlight-textual ((t (:underline t :background nil :foreground nil))))
  :hook
  (lsp-mode . (lambda () (lsp-enable-which-key-integration) (lsp-custom-bindings))))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :custom
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-sideline-delay 0.5)
  (lsp-ui-peek-always-show t)
  (lsp-ui-peek-fontify 'always)
  :custom-face
  (lsp-ui-peek-highlight ((t (:inherit nil :background nil :foreground nil :weight semi-bold :box (:line-width -1)))))
  :bind
  ( :map lsp-ui-mode-map
    ([remap xref-find-references] . lsp-ui-peek-find-references)
    ("C-M-l" . lsp-ui-peek-find-definitions)
    ("C-c C-d" . lsp-ui-doc-show))
  :config
  ;;;; LSP UI posframe ;;;;
  (defun lsp-ui-peek--peek-display (src1 src2)
    (-let* ((win-width (frame-width))
	    (lsp-ui-peek-list-width (/ (frame-width) 2))
	    (string (-some--> (-zip-fill "" src1 src2)
		      (--map (lsp-ui-peek--adjust win-width it) it)
		      (-map-indexed 'lsp-ui-peek--make-line it)
		      (-concat it (lsp-ui-peek--make-footer))))
	    )
      (setq lsp-ui-peek--buffer (get-buffer-create " *lsp-peek--buffer*"))
      (posframe-show lsp-ui-peek--buffer
		     :string (mapconcat 'identity string "")
		     :min-width (frame-width)
		     :poshandler 'posframe-poshandler-frame-center)))

  (defun lsp-ui-peek--peek-destroy ()
    (when (bufferp lsp-ui-peek--buffer)
      (posframe-delete lsp-ui-peek--buffer))
    (setq lsp-ui-peek--buffer nil
	  lsp-ui-peek--last-xref nil)
    (set-window-start (get-buffer-window) lsp-ui-peek--win-start))

  (advice-add 'lsp-ui-peek--peek-new :override 'lsp-ui-peek--peek-display)
  (advice-add 'lsp-ui-peek--peek-hide :override 'lsp-ui-peek--peek-destroy)
  ;;;; LSP UI posframe ;;;;
  )

(use-package lsp-pyright
  :after lsp-mode
  :ensure t
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright)
			 (lsp))))

(use-package lsp-java
  :after lsp-mode
  :ensure t
  :hook (java-mode . (lambda ()
			     (require 'lsp-java)
			     (lsp))))

(use-package helm-lsp
  :after lsp-mode)

(use-package lsp-treemacs)

(use-package dap-mode
  :after lsp-mode
  :config (dap-auto-configure-mode))

(provide 'init-lsp)
