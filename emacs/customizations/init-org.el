(add-to-list 'load-path (expand-file-name "~/git/org-mode/lisp"))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

(require 'org)
(require 'org-evil)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-isswitchb)

(setq org-agenda-files (quote ("~/Dropbox/Documents/dotoo-files")))

(provide 'init-org)
