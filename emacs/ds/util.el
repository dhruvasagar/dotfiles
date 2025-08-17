(defun random-alnum (&optional length)
  (let ((times (or length 5))
	(random ""))
    (setq-local random "")
    (dotimes (_ times)
      (setq random (concat random (let* ((alnum "abcdefghijklmnopqrstuvwxyz0123456789")
	     (i (% (abs (random)) (length alnum))))
		       (substring alnum i (1+ i))))))
    random))

(defun util/test-emacs ()
  "Test if emacs starts correctly."
  (interactive)
  (if (eq last-command this-command)
      (save-buffers-kill-terminal)
    (require 'async)
    (async-start
     (lambda () (shell-command-to-string
		 "emacs --batch --eval \"
(condition-case e
    (progn
      (load \\\"~/.emacs.d/init.el\\\")
      (message \\\"-OK-\\\"))
  (error
   (message \\\"ERROR!\\\")
   (signal (car e) (cdr e))))\""))
     `(lambda (output)
	(if (string-match "-OK-" output)
	    (when ,(called-interactively-p 'any)
	      (message "All is well"))
	  (switch-to-buffer-other-window "*startup error*")
	  (delete-region (point-min) (point-max))
	  (insert output)
	  (search-backward "ERROR!"))))))

(provide 'util)
