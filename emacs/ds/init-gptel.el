(use-package transient)

(use-package gptel
  :config
  (setq gptel-default-mode 'org-mode)
  (setq gptel-model "gpt-4o-mini")

  ;; gptel tools
  ;; File System Tools
  ;; 1. Read file
  (gptel-make-tool
   :function (lambda (filepath)
	       (with-temp-buffer
		 (insert-file-contents (expand-file-name filepath))
		 (buffer-string)))
   :name "read_file"
   :description "Read and display the contents of a file"
   :args (list '(:name "filepath"
		       :type string
		       :description "Path to the file to read. Supports relative paths and ~."))
   :category "filesystem")
  ;; 2. List directory
  (gptel-make-tool
   :function (lambda (directory)
	       (mapconcat #'identity
			  (directory-files directory)
			  "\n"))
   :name "list_directory"
   :description "List the contents of a given directory"
   :args (list '(:name "directory"
		       :type string
		       :description "The path to the directory to list"))
   :category "filesystem")
  ;; 3. Create directory
  (gptel-make-tool
   :function (lambda (parent name)
	       (condition-case nil
		   (progn
		     (make-directory (expand-file-name name parent) t)
		     (format "Directory %s created/verified in %s" name parent))
		 (error (format "Error creating directory %s in %s" name parent))))
   :name "make_directory"
   :description "Create a new directory with the given name in the specified parent directory"
   :args (list '(:name "parent"
		       :type string
		       :description "The parent directory where the new directory should be created, e.g. /tmp")
	       '(:name "name"
		       :type string
		       :description "The name of the new directory to create, e.g. testdir"))
   :category "filesystem")
  ;; 4. Create file
  (gptel-make-tool
   :function (lambda (path filename content)
	       (let ((full-path (expand-file-name filename path)))
		 (with-temp-buffer
		   (insert content)
		   (write-file full-path))
		 (format "Created file %s in %s" filename path)))
   :name "create_file"
   :description "Create a new file with the specified content"
   :args (list '(:name "path"
		       :type string
		       :description "The directory where to create the file")
	       '(:name "filename"
		       :type string
		       :description "The name of the file to create")
	       '(:name "content"
		       :type string
		       :description "The content to write to the file"))
   :category "filesystem")
  ;; 5. Edit file
  (defun ds/gptel--edit_file (file-path file-edits)
    "In FILE-PATH, apply FILE-EDITS with pattern matching and replacing."
    (if (and file-path (not (string= file-path "")) file-edits)
	(with-current-buffer (get-buffer-create "*edit-file*")
	  (insert-file-contents (expand-file-name file-path))
	  (let ((inhibit-read-only t)
		(case-fold-search nil)
		(file-name (expand-file-name file-path))
		(edit-success nil))
	    ;; apply changes
	    (dolist (file-edit (seq-into file-edits 'list))
	      (when-let ((line-number (plist-get file-edit :line_number))
			 (old-string (plist-get file-edit :old_string))
			 (new-string (plist-get file-edit :new_string))
			 (is-valid-old-string (not (string= old-string ""))))
		(goto-char (point-min))
		(forward-line (1- line-number))
		(when (search-forward old-string nil t)
		  (replace-match new-string t t)
		  (setq edit-success t))))
	    ;; return result to gptel
	    (if edit-success
		(progn
		  ;; show diffs
		  (ediff-buffers (find-file-noselect file-name) (current-buffer))
		  (format "Successfully edited %s" file-name))
	      (format "Failed to edited %s" file-name))))
      (format "Failed to edited %s" file-path)))

  (gptel-make-tool
   :function #'ds/gptel--edit_file
   :name "edit_file"
   :description "Edit file with a list of edits, each edit contains a line-number,
a old-string and a new-string, new-string will replace the old-string at the specified line."
   :args (list '(:name "file-path"
		       :type string
		       :description "The full path of the file to edit")
	       '(:name "file-edits"
		       :type array
		       :items (:type object
				     :properties
				     (:line_number
				      (:type integer :description "The line number of the file where edit starts.")
				      :old_string
				      (:type string :description "The old-string to be replaced.")
				      :new_string
				      (:type string :description "The new-string to replace old-string.")))
		       :description "The list of edits to apply on the file"))
   :category "filesystem")
  ;; 6. Run script
  (gptel-make-tool
   :function (lambda (script_program script_file script_args)
	       (with-temp-message "Executing command ..."
		 (shell-command-to-string
		  (concat script_program " "
			  (expand-file-name script_file) " "
			  script_args))))
   :name "run_script"
   :description "Run script"
   :args (list
	  '(:name "script_program"
		  :type string
		  :description "Program to run the the script.")
	  '(:name "script_file"
		  :type string
		  :description "Path to the script to run. Supports relative paths and ~.")
	  '(:name "script_args"
		  :type string
		  :description "Args for script to run."))
   :category "filesystem")
  ;; 7. Run command
  (gptel-make-tool
   :function (lambda (command)
	       (with-temp-message (format "Running command: %s" command)
		 (shell-command-to-string command)))
   :name "run_command"
   :description "Run a command."
   :args (list
	  '(:name "command"
		  :type "string"
		  :description "Command to run."))
   :category "command")
  ;; 8. Run async command
  (defun run_async_command (callback command)
    "Run COMMAND asynchronously and pass output to CALLBACK."
    (condition-case error
	(let ((buffer (generate-new-buffer " *async output*")))
	  (with-temp-message (format "Running async command: %s" command)
	    (async-shell-command command buffer nil))
	  (let ((proc (get-buffer-process buffer)))
	    (when proc
	      (set-process-sentinel
	     proc
	     (lambda (process _event)
	       (unless (process-live-p process)
		 (with-current-buffer (process-buffer process)
		   (let ((output (buffer-substring-no-properties (point-min) (point-max))))
		     (kill-buffer (current-buffer))
		     (funcall callback output)))))))))
      (t
       ;; Handle any kind of error
       (funcall callback (format "An error occurred: %s" error)))))

  (gptel-make-tool
   :function #'run_async_command
   :name "run_async_command"
   :description "Run an async command."
   :args (list
	  '(:name "command"
		  :type "string"
		  :description "Command to run."))
   :category "command"
   :async t
   :include t)

  ;; Web Tools
  ;; 1. Read URL
  (gptel-make-tool
   :function (lambda (url)
	       (with-current-buffer (url-retrieve-synchronously url)
		 (goto-char (point-min))
		 (forward-paragraph)
		 (let ((dom (libxml-parse-html-region (point) (point-max))))
		 (run-at-time 0 nil #'kill-buffer (current-buffer))
		 (with-temp-buffer
		   (shr-insert-document dom)
		   (buffer-substring-no-properties (point-min) (point-max))))))
   :name "read_url"
   :description "Fetch and read the contents of a URL"
   :args (list '(:name "url"
		       :type string
		       :description "The URL to read"))
   :category "web")

  ;; SQL Tools
  ;; 1. Select from DB
  (defun my/ejc-sql-eval-query (query &optional analyze-p connection-name)
    "Evaluate a SQL query using ejc-sql, ensuring a connection exists.
It takes a SQL query and the name of an existing ejc connection.
If no connection name is provided, it defaults to \"Default\".
If ANALYZE-P is non-nil, performs EXPLAIN ANALYZE on the query.
It will create a temporary buffer, connect to the database specified
by CONNECTION-NAME, evaluate the query, and return the result as a string.
It expects the connection CONNECTION-NAME to exist
using `ejc-connect'."
    (interactive)
    (let ((buffer (generate-new-buffer " *temp-ejc-sql-buffer*"))
	  (result "")
	  (actual-connection-name (or connection-name "Default"))
	  (max-wait-time 30) ; Maximum wait time in seconds
	  (wait-interval 0.1))
      (with-current-buffer buffer
	(ejc-connect actual-connection-name)
	(if analyze-p
	    (insert (concat "EXPLAIN (ANALYZE true, COSTS true, FORMAT json) " query ";"))
	  (insert (concat "SELECT json_agg(row_to_json (t)) FROM (" query ") t;")))
	(ejc-eval-user-sql-at-point))
      (let ((wait-time 0))
	(while (and (not (get-buffer "*ejc-sql-output*"))
		    (< wait-time max-wait-time))
	  (sit-for wait-interval)
	  (setq wait-time (+ wait-time wait-interval))))

      (when (get-buffer "*ejc-sql-output*")
	(with-current-buffer "*ejc-sql-output*"
	  (setq result (buffer-string)))
	(kill-buffer "*ejc-sql-output*"))

      (kill-buffer buffer)
      result))

  (gptel-make-tool
   :name "select_from_db"
   :function (lambda (query &optional analyze-p connection-name)
	       (my/ejc-sql-eval-query query analyze-p connection-name))
   :description "Evaluate a SQL query using ejc-sql, ensuring a connection exists.
It takes a SQL query and the name of an existing ejc connection.
If no connection name is provided, it defaults to \"Default\".
If ANALYZE-P is non-nil, performs EXPLAIN ANALYZE on the query.
It will create a temporary buffer, connect to the database specified
by CONNECTION-NAME, evaluate the query, and return the result as a string."
   :args (list '(:name "query"
		       :type string
		       :description "The SQL query to evaluate")
	       '(:name "analyze_p"
		       :type string
		       :optional t
		       :description "If ANALYZE-P is non-nil, performs EXPLAIN ANALYZE on the query instead of executing it directly, returning query execution plan and statistics.")
	       '(:name "connection_name"
		       :type string
		       :optional t
		       :description "The name of the ejc-sql connection to use. This connection must already exist. Defaults to 'Default' if not provided."))
   :category "sql")

  ;; Buffer Tools
  ;; 1. Read Buffer
  (gptel-make-tool
   :function (lambda (buffer)
	       (unless (buffer-live-p (get-buffer buffer))
		 (error "Error: buffer %s is not live." buffer))
	       (with-current-buffer buffer
		 (buffer-substring-no-properties (point-min) (point-max))))
   :name "read_buffer"
   :description "Return the contents of an Emacs buffer"
   :args (list '(:name "buffer"
		       :type string
		       :description "The name of the buffer whose contents are to be retrieved"))
   :category "emacs")
  ;; 2. Append to Buffer
  (gptel-make-tool
   :function (lambda (buffer text)
	       (with-current-buffer (get-buffer-create buffer)
		 (save-excursion
		 (goto-char (point-max))
		 (insert text)))
	       (format "Appended text to buffer %s" buffer))
   :name "append_to_buffer"
   :description "Append text to an Emacs buffer. If the buffer does not exist, it will be created."
   :args (list '(:name "buffer"
		       :type string
		       :description "The name of the buffer to append text to.")
	       '(:name "text"
		       :type string
		       :description "The text to append to the buffer."))
   :category "emacs")
  ;; 3. Edit Buffer
  (defun codel-edit-buffer (buffer-name old-string new-string)
    "In BUFFER-NAME, replace OLD-STRING with NEW-STRING."
    (with-current-buffer buffer-name
      (let ((case-fold-search nil))  ;; Case-sensitive search
	(save-excursion
	  (goto-char (point-min))
	  (let ((count 0))
	    (while (search-forward old-string nil t)
	      (setq count (1+ count)))
	    (if (= count 0)
		(format "Error: Could not find text to replace in buffer %s" buffer-name)
	      (if (> count 1)
		  (format "Error: Found %d matches for the text to replace in buffer %s" count buffer-name)
		(goto-char (point-min))
		(search-forward old-string)
		(replace-match new-string t t)
		(format "Successfully edited buffer %s" buffer-name))))))))

  (gptel-make-tool
   :name "EditBuffer"
   :function #'codel-edit-buffer
   :description "Edits Emacs buffers"
   :args '((:name "buffer_name"
		  :type string
		  :description "Name of the buffer to modify"
		  :required t)
	   (:name "old_string"
		  :type string
		  :description "Text to replace (must match exactly)"
		  :required t)
	   (:name "new_string"
		  :type string
		  :description "Text to replace old_string with"
		  :required t))
   :category "edit")
  ;; 4. Replace Buffer
  (defun codel-replace-buffer (buffer-name content)
    "Completely replace contents of BUFFER-NAME with CONTENT."
    (with-current-buffer buffer-name
      (erase-buffer)
      (insert content)
      (format "Buffer replaced: %s" buffer-name)))

  (gptel-make-tool
   :name "ReplaceBuffer"
   :function #'codel-replace-buffer
   :description "Completely overwrites buffer contents"
   :args '((:name "buffer_name"
		  :type string
		  :description "Name of the buffer to overwrite"
		  :required t)
	   (:name "content"
		  :type string
		  :description "Content to write to the buffer"
		  :required t))
   :category "edit")

  :bind
  ("C-c RET" . gptel-send))

(use-package gptel-aibo
  :after (gptel flycheck)
  :ensure (:host github :repo "dolmens/gptel-aibo"))

(provide 'init-gptel)
