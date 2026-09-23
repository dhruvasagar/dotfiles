(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(setenv "LSP_USE_PLISTS" "true")

;; Native compilation settings
(when (and (fboundp 'native-comp-available-p)
	   (native-comp-available-p))
  ;; Redirect eln-cache into var/ (must be in early-init.el)
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (expand-file-name "var/eln-cache/" user-emacs-directory)))
  (setq native-comp-async-report-warnings-errors nil
	package-native-compile t
	comp-deferred-compilation t
	jit-lock-defer-time 0
	native-comp-jit-compilation t))

;; UTF-8 everywhere
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Fix seq-empty-p: straight's seq-25.el corrupts the built-in definition.
;; Nuclear option: completely replace seq-empty-p with a robust version
;; that doesn't depend on cl-generic dispatch at all.
(defun ds/fix-seq-empty-p ()
  "Replace `seq-empty-p' with a version immune to cl-generic corruption."
  (when (fboundp 'seq-empty-p)
    ;; Remove any existing advice first
    (advice-remove 'seq-empty-p #'ds/seq-empty-p-robust)
    ;; Override seq-empty-p entirely with a plain function
    (defalias 'seq-empty-p
      (lambda (sequence)
        "Return non-nil if SEQUENCE is empty, nil otherwise.
Patched version that handles all types without cl-generic dispatch."
        (cond
         ((listp sequence) (null sequence))
         ((arrayp sequence) (= 0 (length sequence)))
         ((sequencep sequence) (= 0 (length sequence)))
         (t nil)))
      "Patched seq-empty-p: handles lists, vectors, strings, and non-sequence types.")))

;; Apply fix at every possible point during startup:
;; 1. Immediately if seq is already loaded
(ds/fix-seq-empty-p)
;; 2. After seq loads (in case it hasn't loaded yet or gets reloaded)
(with-eval-after-load 'seq (ds/fix-seq-empty-p))
;; 3. After seq-25 loads (straight's version loads this sub-module)
(with-eval-after-load 'seq-25 (ds/fix-seq-empty-p))
;; 4. After init completes (final safety net)
(add-hook 'after-init-hook #'ds/fix-seq-empty-p)

(provide 'early-init)
;; early-init.el ends here
