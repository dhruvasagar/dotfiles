(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
	"straight/repos/straight.el/bootstrap.el"
	(or (bound-and-true-p straight-base-dir)
	    user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Ensure straight.el is used for package management
(setq straight-use-package-by-default t)

(use-package use-package :config
  (setq use-package-always-ensure t) ;; Ensure packages are always installed
  (straight-use-package 'use-package))

(provide 'init-straight)
