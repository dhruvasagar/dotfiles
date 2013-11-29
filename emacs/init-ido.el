(ido-mode t)
(setq
 ido-everywhere t
 ido-auto-merge-work-directories-length 0
 ido-use-filename-at-point 'guess
 ido-use-url-at-point nil           ; don't use url at point (annoying)
 ido-case-fold  t                   ; be case-insensitive
 ido-max-prospects 10
 ido-max-window-height 1
 ffap-machine-p-known 'reject
 ido-ignore-files (append ido-ignore-files '("\\.orig\\'" "\\.dSYM/" "\\.o\\'" "\\.d\\'" "\\.beam\\'"
                                             "\\.hi\\'")))

(provide 'init-ido)
