(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(setenv "LSP_USE_PLISTS" "true")

;; Native compilation settings
(when (and (fboundp 'native-comp-available-p)
	   (native-comp-available-p))
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

(provide 'early-init)
;; early-init.el ends here
