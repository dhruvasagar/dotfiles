(require 'emms-setup)
(require 'emms-info-libtag)

(emms-devel)
(emms-default-players)

(setq
 emms-info-asynchronously t
 later-do-interval 0.0001
 emms-info-functions '(emms-info-libtag)
 emms-source-file-default-directory "~/Music/"
 emms-mode-line-format " %s "
 emms-show-format "NP: %s")

(add-hook 'emms-player-started-hook (lambda()
																			(emms-shuffle)
																			(emms-toggle-repeat-playlist)))

(global-set-key (kbd "C-c n") 'emms-next)
(global-set-key (kbd "C-c p") 'emms-previous)

(provide 'init-emms)
