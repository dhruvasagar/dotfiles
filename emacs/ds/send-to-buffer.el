(defvar send-to-buffer-name nil
  "Name of buffer to send the range to")

(defvar send-to-buffer-mode-hook nil)

(defun send-to-buffer--prompt-target-buffer ()
  (interactive)
  (setq send-to-buffer-name (read-from-minibuffer "Buffer Name: ")))

(defun send-to-buffer-set-buffer-as-target ()
  "Set current buffer as target for send-to-buffer"
  (interactive)
  (setq send-to-buffer-name (buffer-name)))

(defun send-to-buffer (beg end)
  (interactive "r")
  (if (null send-to-buffer-name)
      (progn
	(send-to-buffer--prompt-target-buffer)
	(send-to-buffer beg end))
    (if (use-region-p)
	(process-send-region send-to-buffer-name beg end)
      (let ((current-line (thing-at-point 'paragraph t)))
	(with-temp-buffer
	  (insert current-line)
	  (process-send-region send-to-buffer-name (point-min) (point-max)))))))

(define-minor-mode send-to-buffer-mode
  "Minor mode for Send to Buffer"
  :lighter " SendToBuffer"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c >") 'send-to-buffer)
	    map)
  (when (featurep 'evil)
    (evil-define-key 'normal send-to-buffer-mode-map (kbd "g >") 'send-to-buffer))
  (run-hooks 'send-to-buffer-mode-hook))

(provide 'send-to-buffer)
