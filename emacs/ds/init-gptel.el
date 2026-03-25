(use-package transient)

(use-package gptel
  :config
  (setq gptel-log-level 'info
	gptel-default-mode 'org-mode
	gptel-track-media t
	gptel-cache '(message system tool)
	gptel-use-tools t
	gptel-model "ChatGPT:gpt-4o-mini")

  ;; gptel tools
  ;; emacs
  (gptel-make-tool
   :function (lambda (buffer)
	       (with-temp-message (format "Reading buffer: %s" buffer)
		 (condition-case err
		     (if (buffer-live-p (get-buffer buffer))
			 (with-current-buffer buffer
			   (buffer-substring-no-properties (point-min) (point-max)))
		       (format "Error: buffer %s is not live." buffer))
		   (error (format "Error reading buffer %s: %s"
				  buffer (error-message-string err))))))
   :name "read_buffer"
   :description "Return the contents of an Emacs buffer"
   :args (list '(:name "buffer"
		       :type string
		       :description "The name of the buffer whose contents are to be retrieved"))
   :category "emacs"
   :include t)

  (gptel-make-tool
   :function (lambda (buffer text)
	       (with-temp-message (format "Appending to buffer: %s" buffer)
		 (condition-case err
		     (if (buffer-live-p (get-buffer buffer))
			 (with-current-buffer buffer
			   (goto-char (point-max))
			   (insert text)
			   (format "Successfully appended text to buffer %s." buffer))
		       (format "Error: buffer %s is not live or does not exist." buffer))
		   (error (format "Error appending to buffer %s: %s"
				  buffer (error-message-string err))))))
   :name "append_to_buffer"
   :description "Append the given text to the end of an Emacs buffer. Returns a success or error message."
   :args (list
	  '(:name "buffer"
		  :type string
		  :description "The name of the buffer to append to.")
	  '(:name "text"
		  :type string
		  :description "The text to append to the buffer."))
   :category "emacs"
   :include t)

  (defun gptel-read-documentation (symbol)
    "Read the documentation for SYMBOL, which can be a function or variable."
    (with-temp-message (format "Reading documentation for: %s" symbol)
      (condition-case err
	  (let ((sym (intern symbol)))
	    (cond
	     ((fboundp sym)
	      (documentation sym))
	     ((boundp sym)
	      (documentation-property sym 'variable-documentation))
	     (t
	      (format "No documentation found for %s" symbol))))
	(error (format "Error reading documentation for %s: %s"
		       symbol (error-message-string err))))))

  (gptel-make-tool
   :name "read_documentation"
   :function #'gptel-read-documentation
   :description "Read the documentation for a given function or variable"
   :args (list '(:name "name"
		       :type string
		       :description "The name of the function or variable whose documentation is to be retrieved"))
   :category "emacs"
   :include t)


  (gptel-make-tool
   :function (lambda (text)
	       (with-temp-message (format "Sending message: %s" text)
		 (message "%s" text)
		 (format "Message sent: %s" text)))
   :name "echo_message"
   :description "Send a message to the *Messages* buffer"
   :args (list '(:name "text"
		       :type string
		       :description "The text to send to the messages buffer"))
   :category "emacs"
   :include t)

  (gptel-make-tool
   :function (lambda (buffer_name content)
	       (with-temp-message (format "Replacing buffer contents: `%s`" buffer_name)
		 (if (get-buffer buffer_name)
		     (with-current-buffer buffer_name
		       (erase-buffer)
		       (insert content)
		       (format "Buffer contents replaced: %s" buffer_name))
		   (format "Error: Buffer '%s' not found" buffer_name))))
   :name "replace_buffer"
   :description "Completely overwrites buffer contents with the provided content."
   :args (list
	  '(:name "buffer_name"
		  :type string
		  :description "The name of the buffer whose contents will be replaced.")
	  '(:name "content"
		  :type string
		  :description "The new content to write to the buffer, replacing all existing content."))
   :category "emacs"
   :include t)

  ;; filesystem
  (gptel-make-tool
   :function (lambda (path filename content)
	       (with-temp-message (format "Creating file: %s in %s" filename path)
		 (condition-case err
		     (let ((full-path (expand-file-name filename path)))
		       (with-temp-buffer
			 (insert content)
			 (write-file full-path))
		       (format "Created file %s in %s" filename path))
		   (error (format "Error creating file %s in %s: %s"
				  filename path (error-message-string err))))))
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
   :category "filesystem"
   :include t)



  (gptel-make-tool
   :function (lambda (parent name)
	       (with-temp-message (format "Creating directory: %s in %s" name parent)
		 (condition-case err
		     (progn
		       (make-directory (expand-file-name name parent) t)
		       (format "Directory %s created/verified in %s" name parent))
		   (error (format "Error creating directory %s in %s: %s"
				  name parent (error-message-string err))))))
   :name "make_directory"
   :description "Create a new directory with the given name in the specified parent directory"
   :args (list '(:name "parent"
		       :type string
		       :description "The parent directory where the new directory should be created, e.g. /tmp")
	       '(:name "name"
		       :type string
		       :description "The name of the new directory to create, e.g. testdir"))
   :category "filesystem"
   :include t)

  (gptel-make-tool
   :function (lambda (file_path new_content)
	       (with-temp-message (format "Replacing content in file: `%s`" file_path)
		 (let ((full-path (expand-file-name file_path)))
		   (with-temp-file full-path
		     (insert new_content))
		   (format "Successfully replaced content in %s" full-path))))
   :name "replace_file_contents"
   :description "Replaces the entire content of a file. Use this tool ONLY as a last resort if both 'edit_file' and 'apply_diff' fail. It is highly token-inefficient as it requires sending the full file content. WARNING: This operation completely overwrites the target file."
   :args (list
	  '(:name "file_path"
		  :type string
		  :description "The path to the file that needs to be replaced.")
	  '(:name "new_content"
		  :type string
		  :description "The new content for the file."))
   :category "filesystem"
   :include t)

  (gptel-make-tool
   :function (lambda (command &optional working_dir)
	       (with-temp-message (format "Executing command: `%s`" command)
		 (let ((default-directory (if (and working_dir (not (string= working_dir "")))
					      (expand-file-name working_dir)
					    default-directory)))
		   (shell-command-to-string command))))
   :name "run_command"
   :description (concat
		 "Executes a shell command and returns the output as a string. IMPORTANT: This tool allows execution of arbitrary code."
		 "Installed commandline tools: coreutils, git, patch, findutils, the-silver-searcher curl"
		 "NOTE: You can use a combination of `find` and `the-silver-searcher` to find your way around a codebase")
   :args (list
	  '(:name "command"
		  :type string
		  :description "The complete shell command to execute.")
	  '(:name "working_dir"
		  :type string
		  :description "Optional: The directory in which to run the command. Defaults to the current directory if not specified."))
   :category "command"
   :include t)

  (gptel-make-tool
   :function (lambda (query)
	       (with-temp-message (format "Searching for: `%s`" query)
		 (let ((url (format "https://%s:%s@search.twohundredok.com/search?q=%s&format=json"
				    munen-gptel--twohundredok-user
				    munen-gptel--twohundredok-password
				    (url-hexify-string query))))
		   (with-temp-buffer
		     (url-insert-file-contents url)
		     (let ((json-response (json-read)))
		       (mapconcat (lambda (result)
				    (format "%s - %s\n%s" (cdr (assoc 'title result)) (cdr (assoc 'url result)) (cdr (assoc 'content result))))
				  (cdr (assoc 'results json-response))
				  "\n\n"))))))
   :name "search_web"
   :description "Searches the web using SearXNG metasearch engine and returns formatted results including titles, URLs, and content excerpts."
   :args (list
	  '(:name "query"
		  :type string
		  :description "The search query to execute against the search engine."))
   :category "web"
   :include t)

  (defun munen-gptel--trafilatura-fetch-url (url)
    "Fetch content from URL using trafilatura and return it as a string."
    (with-temp-message (format "Fetching content from: %s" url)
      (with-temp-buffer
	(call-process "trafilatura" nil t nil "--output-format=markdown" "--with-metadata" "-u" url)
	(buffer-string))))

  (gptel-make-tool
   :name "TrafilaturaFetch"
   :function #'munen-gptel--trafilatura-fetch-url
   :description "Fetch content from a URL using trafilatura, which extracts main content and metadata while removing boilerplate, navigation and ads."
   :args '((:name "url"
		  :type string
		  :description "URL to fetch content from"))
   :category "web"
   :include t)

  ;; read_file
  (gptel-make-tool
   :function (lambda (filepath)
	       (let ((ignore-patterns (when (boundp 'gptel-read-file-ignore-patterns)
					gptel-read-file-ignore-patterns))
		     (expanded-path (expand-file-name filepath)))
		 (if (and ignore-patterns
			  (cl-some (lambda (pattern)
				     (string-match-p pattern expanded-path))
				   ignore-patterns))
		     (format "Access denied: File %s matches ignore patterns" filepath)
		   (with-temp-message (format "Reading file: %s" filepath)
		     (condition-case err
			 (with-temp-buffer
			   (insert-file-contents expanded-path)
			   (buffer-string))
		       (error (format "Error reading file: %s - %s" filepath (error-message-string err))))))))
   :name "read_file"
   :description "Read and display the contents of a file. Note: If a file is already included in the current gptel context (conversation), there is no need to read it again as the context is always current."
   :args (list '(:name "filepath"
		       :type string
		       :description "Path to the file to read. Supports relative paths and ~. Only use this tool for files not already in the conversation context."))
   :category "filesystem"
   :include t)

  ;; A list of patterns that prevent `read_file` tool from accessing certain files
  (setq gptel-read-file-ignore-patterns
	'("*.secret"
	  "config/*"))

  ;; edit_file
  (defun munen-gptel--edit-file (file-path &optional file-edits)
    "Edit FILE-PATH by applying FILE-EDITS using fuzzy string matching (non-interactive).

    This function directly modifies the file on disk without user confirmation.
    Each edit in FILE-EDITS should specify:
    - :old_string - The string to find and replace (fuzzy matched)
    - :new_string - The replacement string

    EDITING RULES:
    - The old_string is matched using fuzzy search throughout the file
    - If multiple matches exist, only the first occurrence is replaced
    - Include enough context in old_string to uniquely identify the location
    - Whitespace differences are normalized during matching
    - Keep edits focused on the specific change requested

    Returns a success/failure message indicating whether edits were applied."
    (if (and file-path (not (string= file-path "")) file-edits)
	(with-current-buffer (get-buffer-create "*edit-file*")
	  (erase-buffer)
	  (insert-file-contents (expand-file-name file-path))
	  (let ((inhibit-read-only t)
		(target-file-name (expand-file-name file-path))
		(edit-success nil)
		(applied-edits 0)
		(total-edits (length file-edits)))
	    ;; Apply changes
	    (dolist (file-edit (seq-into file-edits 'list))
	      (when-let ((old-string (plist-get file-edit :old_string))
			 (new-string (plist-get file-edit :new_string))
			 (is-valid-old-string (not (string= old-string ""))))
		(goto-char (point-min))
		;; Try exact match first
		(if (search-forward old-string nil t)
		    (progn
		      (replace-match new-string t t)
		      (setq edit-success t)
		      (setq applied-edits (1+ applied-edits)))
		  ;; If exact match fails, try fuzzy match
		  (goto-char (point-min))
		  (when (munen-gptel--fuzzy-search old-string)
		    (replace-match new-string t t)
		    (setq edit-success t)
		    (setq applied-edits (1+ applied-edits))))))
	    ;; Return result
	    (if edit-success
		(progn
		  (write-file target-file-name nil)
		  (kill-buffer (current-buffer))
		  (format "Successfully edited and saved %s (%d/%d edits applied)"
			  target-file-name applied-edits total-edits))
	      (progn
		(kill-buffer (current-buffer))
		(format "Failed to apply edits to %s. No matching strings found." target-file-name)))))
      (format "Failed to edit %s (invalid path or no edits provided)." file-path)))

  (defun munen-gptel--normalize-whitespace (string)
    "Normalize whitespace in STRING for fuzzy matching.
    Converts multiple whitespace characters to single spaces and trims."
    (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " string)))

  (defun munen-gptel--fuzzy-search (target-string)
    "Search for TARGET-STRING using fuzzy matching.
    Returns t if found and positions point after the match, nil otherwise.
    Tries multiple matching strategies in order of preference."
    (let ((normalized-target (munen-gptel--normalize-whitespace target-string))
	  (case-fold-search nil))
      (or
       ;; Strategy 1: Normalized whitespace matching
       (progn
	 (goto-char (point-min))
	 (let ((found nil))
	   (while (and (not found) (not (eobp)))
	     (let* ((line-start (line-beginning-position))
		    (line-end (line-end-position))
		    (line-text (buffer-substring-no-properties line-start line-end))
		    (normalized-line (munen-gptel--normalize-whitespace line-text)))
	       (when (string-match-p (regexp-quote normalized-target) normalized-line)
		 ;; Found a match, now find the actual position in the original text
		 (goto-char line-start)
		 (when (re-search-forward
			(munen-gptel--create-fuzzy-regex target-string)
			line-end t)
		   (setq found t)))
	       (unless found (forward-line 1))))
	   found))
       ;; Strategy 2: Case-insensitive search
       (progn
	 (goto-char (point-min))
	 (let ((case-fold-search t))
	   (search-forward target-string nil t)))
       ;; Strategy 3: Regex-based flexible matching
       (progn
	 (goto-char (point-min))
	 (re-search-forward (munen-gptel--create-flexible-regex target-string) nil t)))))

  (defun munen-gptel--create-fuzzy-regex (string)
    "Create a regex pattern for fuzzy matching STRING.
    Allows flexible whitespace matching between words."
    (let ((escaped (regexp-quote string)))
      ;; Replace escaped whitespace with flexible whitespace pattern
      (replace-regexp-in-string
       "\\\\[ \t\n\r]+"
       "[ \t\n\r]+"
       escaped)))

  (defun munen-gptel--create-flexible-regex (string)
    "Create a very flexible regex pattern for STRING.
    Allows optional whitespace between characters and words."
    (let* ((chars (string-to-list string))
	   (pattern-parts '()))
      (dolist (char chars)
	(cond
	 ((memq char '(?\s ?\t ?\n ?\r))
	  ;; For whitespace, allow flexible matching
	  (push "[ \t\n\r]*" pattern-parts))
	 (t
	  ;; For regular characters, escape and add optional whitespace after
	  (push (concat (regexp-quote (char-to-string char)) "[ \t\n\r]*")
		pattern-parts))))
      (concat "\\(" (string-join (reverse pattern-parts) "") "\\)")))

  (gptel-make-tool
   :function #'munen-gptel--edit-file
   :name "edit_file"
   :description "Edits a file by applying fuzzy string matching changes. This is a primary method for file modification.

    If this tool fails, try 'apply_diff'. As a last resort, use 'replace_file_contents'.
    This tool modifies the file directly on disk without user confirmation.

    Each edit requires an old string to find (fuzzy matched) and a new string to replace it with."
   :args (list '(:name "file-path"
		       :type string
		       :description "The full path of the file to edit.")
	       '(:name "file-edits"
		       :type array
		       :items (:type object
				     :properties
				     (:old_string
				      (:type string :description "The exact string to be replaced (will be fuzzy matched if exact match fails).")
				      :new_string
				      (:type string :description "The new string to replace old_string.")))
		       :description "A list of edits to apply to the file. Each edit must contain old_string and new_string."))
   :category "filesystem"
   :include t)

  ;; edit_file_interactively
  (defun munen-gptel--edit-file-interactive (file-path file-edits)
    "Edit FILE-PATH by applying FILE-EDITS with interactive review using ediff.

    This function applies the specified edits to the file and then opens an ediff
    session to review changes before saving. Each edit in FILE-EDITS should specify:
    - :line_number - The 1-based line number where the edit occurs
    - :old_string - The exact string to find and replace
    - :new_string - The replacement string

    EDITING RULES:
    - The old_string must EXACTLY MATCH the existing file content at the specified line
    - Include enough context in old_string to uniquely identify the location
    - Keep edits concise and focused on the specific change requested
    - Do not include long runs of unchanged lines

    After applying edits, opens ediff to compare original vs modified versions,
    allowing user to review and selectively apply changes before saving.
    Returns a success/failure message indicating whether edits were applied."
    (if (and file-path (not (string= file-path "")) file-edits)
	(with-current-buffer (get-buffer-create "*edit-file*")
	  (erase-buffer)
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
   :function #'munen-gptel--edit-file-interactive
   :name "edit_file_interactive"
   :description "Edit a file interactively by applying a list of edits with review via ediff.

    This tool applies the specified edits and opens an ediff session for review.
    Each edit specifies a line number, old string to find, and new string replacement.

    After applying edits, ediff opens to compare original vs modified versions,
    allowing interactive review and selective application of changes before saving.
    This provides a safe way to review changes before committing them to disk."
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
   :category "emacs")

  ;; apply_diff
  (gptel-make-tool
   :name "apply_diff"
   :description (concat
		 "Applies a diff (patch) to a file. This is a primary method for file modification. "
		 "If this tool fails, try 'edit_file'. As a last resort, use 'replace_file_contents'. "
		 "The diff must be in the unified format ('diff -u'). "
		 "Ensure file paths in the diff (e.g., '--- a/file', '+++ b/file') match the 'file_path' argument and 'patch_options'. "
		 "Common 'patch_options' include: '' (for exact/relative paths), "
		 "'-p0' (if diff paths are full), '-p1' (to strip one leading directory). "
		 "Default options are '-N' (ignore already applied patches).")
   :args (list
	  '(:name "file_path"
		  :type string
		  :description "The path to the file that needs to be patched.")
	  '(:name "diff_content"
		  :type string
		  :description "The diff content in unified format (e.g., from 'diff -u').")
	  '(:name "patch_options"
		  :type string
		  :optional t
		  :description "Optional: Additional options for the 'patch' command (e.g., '-p1', '-p0', '-R'). Defaults to '-N'. Prepend other options if needed, e.g., '-p1 -N'.")
	  '(:name "working_dir"
		  :type string
		  :optional t
		  :description "Optional: The directory in which to interpret file_path and run patch. Defaults to the current buffer's directory if not specified."))
   :category "filesystem"
   :function
   (lambda (file_path diff_content &optional patch_options working_dir)
     (let ((original-default-directory default-directory)
	   (user-patch-options (if (and patch_options (not (string-empty-p patch_options)))
				   (split-string patch_options " " t)
				 nil))
	   ;; Combine user options with -N, ensuring -N is there.
	   ;; If user provides -N or --forward, use their version. Otherwise, add -N.
	   (base-options '("-N"))
	   (effective-patch-options '()))

       (if user-patch-options
	   (if (or (member "-N" user-patch-options) (member "--forward" user-patch-options))
	       (setq effective-patch-options user-patch-options)
	     (setq effective-patch-options (append user-patch-options base-options)))
	 (setq effective-patch-options base-options))

       (let* ((out-buf-name (generate-new-buffer-name "*patch-stdout*"))
	      (err-buf-name (generate-new-buffer-name "*patch-stderr*"))
	      (target-file nil)
	      (exit-status -1) ; Initialize to a known non-zero value
	      (result-output "")
	      (result-error ""))
	 (unwind-protect
	     (progn
	       (when (and working_dir (not (string-empty-p working_dir)))
		 (setq default-directory (expand-file-name working_dir)))

	       (setq target-file (expand-file-name file_path))

	       (unless (file-exists-p target-file)
		 ;; Use error to signal failure, which gptel should catch.
		 (error "File to patch does not exist: %s" target-file))

	       (with-temp-message (format "Applying diff to: `%s` with options: %s" target-file effective-patch-options)
		 (with-temp-buffer
		   (insert diff_content)
		   (unless (eq (char-before (point-max)) ?\n)
		     (goto-char (point-max))
		     (insert "\n"))

		   ;; Pass buffer *names* to call-process-region
		   (setq exit-status (apply #'call-process-region
					    (point-min) (point-max)
					    "patch"       ; Command
					    nil           ; delete region (no)
					    (list out-buf-name err-buf-name) ; stdout/stderr buffer names
					    nil           ; display (no)
					    (append effective-patch-options (list target-file))))))

	       ;; Retrieve content from buffers using their names
	       (let ((stdout-buf (get-buffer out-buf-name))
		     (stderr-buf (get-buffer err-buf-name)))
		 (when stdout-buf
		   (with-current-buffer stdout-buf
		     (setq result-output (buffer-string))))
		 (when stderr-buf
		   (with-current-buffer stderr-buf
		     (setq result-error (buffer-string)))))

	       (if (= exit-status 0)
		   (format "Diff successfully applied to %s.\nPatch command options: %s\nPatch STDOUT:\n%s\nPatch STDERR:\n%s"
			   target-file effective-patch-options result-output result-error)
		 ;; Signal an Elisp error, which gptel will catch and display.
		 ;; The arguments to 'error' become the error message.
		 (error "Failed to apply diff to %s (exit status %s).\nPatch command options: %s\nPatch STDOUT:\n%s\nPatch STDERR:\n%s"
			target-file exit-status effective-patch-options result-output result-error)))
	   ;; Cleanup clause of unwind-protect
	   (setq default-directory original-default-directory)
	   (let ((stdout-buf-obj (get-buffer out-buf-name))
		 (stderr-buf-obj (get-buffer err-buf-name)))
	     (when (buffer-live-p stdout-buf-obj) (kill-buffer stdout-buf-obj))
	     (when (buffer-live-p stderr-buf-obj) (kill-buffer stderr-buf-obj)))))))
   :include t)

  ;; list_directory
  (defun gptel--normalize-max-depth (max-depth)
    "Convert MAX-DEPTH to a number, handling strings, numbers, or nil.
    Returns 3 as default if MAX-DEPTH is nil or invalid."
    (cond
     ;; Already a number
     ((numberp max-depth) max-depth)
     ;; String that can be converted to number
     ((and (stringp max-depth)
	   (not (string-empty-p max-depth))
	   (string-match-p "^[0-9]+$" max-depth))
      (string-to-number max-depth))
     ;; Default case (nil, empty string, or invalid input)
     (t 3)))

  (defun gptel--parse-gitignore (gitignore-file)
    "Parse a .gitignore file and return a list of patterns."
    (when (file-exists-p gitignore-file)
      (with-temp-buffer
	(insert-file-contents gitignore-file)
	(let ((patterns '()))
	  (goto-char (point-min))
	  (while (not (eobp))
	    (let ((line (string-trim (buffer-substring-no-properties
				      (line-beginning-position)
				      (line-end-position)))))
	      (unless (or (string-empty-p line) (string-prefix-p "#" line))
		(push line patterns)))
	    (forward-line 1))
	  (nreverse patterns)))))

  (defun gptel--should-ignore-p (file-path gitignore-patterns)
    "Check if FILE-PATH should be ignored based on GITIGNORE-PATTERNS."
    (let ((relative-path (file-name-nondirectory file-path)))
      (cl-some (lambda (pattern)
		 (cond
		  ;; Directory pattern (ends with /)
		  ((string-suffix-p "/" pattern)
		   (and (file-directory-p file-path)
			(string-match-p (concat "^" (regexp-quote (string-remove-suffix "/" pattern)) "$")
					relative-path)))
		  ;; Exact match
		  ((not (string-match-p "[*?]" pattern))
		   (string= relative-path pattern))
		  ;; Wildcard pattern
		  (t
		   (string-match-p (concat "^" (replace-regexp-in-string "\\*" ".*" (regexp-quote pattern)) "$")
				   relative-path))))
	       gitignore-patterns)))

  (defun gptel--collect-gitignore-patterns (directory)
    "Collect all .gitignore patterns from DIRECTORY and parent directories."
    (let ((patterns '())
	  (current-dir (expand-file-name directory)))
      (while (and current-dir (not (string= current-dir "/")))
	(let ((gitignore-file (expand-file-name ".gitignore" current-dir)))
	  (when (file-exists-p gitignore-file)
	    (setq patterns (append (gptel--parse-gitignore gitignore-file) patterns))))
	(let ((parent (file-name-directory (directory-file-name current-dir))))
	  (setq current-dir (if (string= parent current-dir) nil parent))))
      ;; Add common ignore patterns
      (append patterns '(".git" ".DS_Store" "node_modules" "__pycache__" "*.pyc"))))

  (defun gptel--directory-tree (directory max-depth show-hidden)
    "Generate a tree representation of DIRECTORY."
    (let ((expanded-dir (expand-file-name directory)))
      (concat (abbreviate-file-name expanded-dir) "\n"
	      (gptel--directory-tree-recursive expanded-dir max-depth 0 show-hidden ""))))

  (defun gptel--directory-tree-recursive (directory max-depth current-depth show-hidden prefix)
    "Internal recursive function for generating directory tree."
    (if (>= current-depth max-depth)
	""
      (let* ((expanded-dir (expand-file-name directory))
	     (gitignore-patterns (gptel--collect-gitignore-patterns expanded-dir))
	     (entries (condition-case nil
			  (directory-files expanded-dir t "^[^.]" t)
			(error nil)))
	     (filtered-entries '())
	     (result ""))

	;; Add hidden files if requested
	(when show-hidden
	  (setq entries (append entries
				(directory-files expanded-dir t "^\\.[^.]" t))))

	;; Filter out ignored files
	(dolist (entry entries)
	  (unless (gptel--should-ignore-p entry gitignore-patterns)
	    (push entry filtered-entries)))

	(setq filtered-entries (sort filtered-entries #'string<))

	;; Generate tree output
	(let ((total (length filtered-entries)))
	  (dotimes (i total)
	    (let* ((entry (nth i filtered-entries))
		   (basename (file-name-nondirectory entry))
		   (is-last (= i (1- total)))
		   (is-dir (file-directory-p entry))
		   (connector (if is-last "└── " "├── "))
		   (new-prefix (concat prefix (if is-last "    " "│   "))))

	      (setq result (concat result prefix connector basename
				   (if is-dir "/" "") "\n"))

	      ;; Recurse into directories
	      (when (and is-dir (< (1+ current-depth) max-depth))
		(setq result (concat result
				     (gptel--directory-tree-recursive entry max-depth
								      (1+ current-depth) show-hidden
								      new-prefix)))))))
	result)))

  (gptel-make-tool
   :function (lambda (directory &optional max-depth show-hidden)
	       (with-temp-message (format "Listing directory tree: %s" directory)
		 (condition-case err
		     (let ((max-depth (gptel--normalize-max-depth max-depth))
			   (show-hidden (and show-hidden (not (string= show-hidden "")))))
		       (gptel--directory-tree directory max-depth show-hidden))
		   (error (format "Error listing directory: %s - %s" directory (error-message-string err))))))
   :name "list_directory"
   :description "List the contents of a directory in a tree format, respecting .gitignore files"
   :args (list '(:name "directory"
		       :type string
		       :description "The path to the directory to list")
	       '(:name "max-depth"
		       :type string
		       :description "Optional: Maximum depth to traverse (default: 3)")
	       '(:name "show-hidden"
		       :type string
		       :description "Optional: Show hidden files/directories (default: false)"))
   :category "filesystem"
   :include t)

  ;; file_lint_with_flycheck
  (gptel-make-tool
   :name "file_lint_with_flycheck"
   :description (concat
		 "Lints the specified file using Flycheck in Emacs, returning any errors or warnings "
		 "(or a 'no errors found' message).\n\n"
		 "**LLM Workflow for Code Modification:**\n"
		 "1.  **Baseline Check:** Run this tool on the file *before* generating a patch or "
		 "other code modifications. This helps understand the existing lint status and "
		 "avoid re-introducing pre-existing issues or being blamed for them.\n"
		 "2.  **Verification Check:** After your changes have been applied to the file "
		 "(e.g., via a patch), run this tool again.\n"
		 "3.  **Self-Correction:** Compare the lint output from step 2 (after your changes) "
		 "with the baseline from step 1 (before your changes). If your modifications "
		 "introduced *new* lint errors, you are expected to refactor your code to fix "
		 "these new errors. Re-run this lint tool to confirm your fixes before "
		 "considering the task complete. Focus on fixing errors introduced by your changes.")
   :args (list '(:name "filename"
		       :type "string"
		       :description "The path (relative or absolute) to the file to be checked."))
   :category "emacs"
   :include t
   :function
   (lambda (filename)
     (unless (require 'flycheck nil t)
       (error "Flycheck package is not available."))
     (unless (stringp filename)
       (error "Filename argument must be a string."))
     (let ((original-filename filename))
       (condition-case err
	   (let* ((absolute-filename (expand-file-name filename))
		  (buffer-object (get-file-buffer absolute-filename))
		  (temp-buffer-created (not buffer-object))
		  (buffer (or buffer-object
			      (progn
				(unless (file-exists-p absolute-filename)
				  (error "File not found: %s (expanded from %s)" absolute-filename original-filename))
				(find-file-noselect absolute-filename))))
		  (flycheck-was-on-in-buffer nil)
		  (errors-string "Error: Failed to collect Flycheck results.")) ; Default error

	     (unless (buffer-live-p buffer)
	       (error "Could not open or find buffer for file: %s" absolute-filename))

	     (with-temp-message (format "Linting %s with Flycheck..." absolute-filename)
	       (unwind-protect
		   (progn ;; Main work block
		     (with-current-buffer buffer
		       (setq flycheck-was-on-in-buffer flycheck-mode)
		       (unless flycheck-mode
			 (flycheck-mode 1)
			 (unless flycheck-mode
			   (error "Failed to enable Flycheck mode in buffer %s." (buffer-name))))

		       (flycheck-buffer) ;; Request a syntax check

		       (let ((timeout 15.0)
			     (start-time (float-time)))
			 (while (and (flycheck-running-p)
				     (< (- (float-time) start-time) timeout))
			   (sit-for 0.1 t))) ;; Wait, process events

		       ;; Check reason for loop termination & collect errors
		       (if (flycheck-running-p)
			   (error "Flycheck timed out after %.0f seconds for %s" timeout absolute-filename)
			 ;; Flycheck is no longer running, collect errors
			 (progn
			   ;; flycheck-current-errors is a VARIABLE
			   (let ((current-errors flycheck-current-errors))
			     (if current-errors
				 (setq errors-string
				       (format "Flycheck results for %s:\n%s"
					       absolute-filename
					       (mapconcat
						(lambda (err-obj) ;; err-obj is a flycheck-error struct
						  (format "- %S at L%s%s (%s): %s"
							  (flycheck-error-level err-obj)
							  (flycheck-error-line err-obj)
							  (if-let ((col (flycheck-error-column err-obj))) ; CORRECTED
							      (format ":C%s" col) "")
							  (flycheck-error-checker err-obj)
							  (flycheck-error-message err-obj)))
						current-errors "\n")))
			       (setq errors-string (format "No Flycheck errors found in %s." absolute-filename)))))))
		     ;; errors-string is now set based on Flycheck output
		     ) ;; End of with-current-buffer
		 ;; Cleanup block for unwind-protect
		 (progn
		   (when (buffer-live-p buffer)
		     (with-current-buffer buffer
		       (when (and flycheck-mode (not flycheck-was-on-in-buffer))
			 (flycheck-mode 0))))
		   (when (and temp-buffer-created (buffer-live-p buffer))
		     (kill-buffer buffer)))))
	     errors-string)
	 (error (format "Error linting file %s: %s"
			original-filename (error-message-string err)))))))

  ;; run_clojure_in_repl
  (gptel-make-tool
   :function (lambda (code host port working_dir)
	       (let* ((eff-host (if (or (null host) (string= host "")) "localhost" host))
		      (eff-port (if (or (null port) (and (integerp port) (= port 0))) 7888 port))
		      (eff-working-dir (let ((base-dir (or default-directory ".")))
					 (if (or (null working_dir) (string= working_dir ""))
					     base-dir
					   (expand-file-name working_dir base-dir))))
		      (shell-command (format "echo \"%s\" | lein repl :connect %s:%s"
					     code eff-host eff-port)))
		 (with-temp-message (format "Executing: %s" shell-command)
		   (let ((default-directory eff-working-dir)) ; Temporarily set default-directory
		     (shell-command-to-string shell-command)))))
   :name "run_clojure_in_repl"
   :description (concat
		 "Executes arbitrary Clojure code in a connected Leiningen nREPL session and returns the output. "
		 "This tool constructs and executes a shell command like 'echo \"<your_code_arg>\" | lein repl :connect host:port'.\n\n"
		 "IMPORTANT LLM USAGE INSTRUCTIONS FOR THE 'code' ARGUMENT:\n"
		 "1. The 'code' argument string you provide will be wrapped *directly* in double quotes by the 'echo' command "
		 "(e.g., the tool effectively does: echo \"<your_code_arg>\").\n"
		 "2. Because of this, if your Clojure code contains string literals that use double quotes, YOU MUST ESCAPE THOSE "
		 "INTERNAL DOUBLE QUOTES as '\\\\\"' within the 'code' argument string you provide. "
		 "   Example: To run the Clojure `(println \"Hello \"World\"!\")`, you must provide the 'code' argument as "
		 "   \'\'\'(println \\\\\"Hello \\\\\\\\\\\\\\\"World\\\\\\\\\\\\\\\"!\")\'\'\' (note the escaped internal quotes for the 'code' arg string).\n"
		 "3. Single quotes (e.g., `(quote foo)` or `'foo`) and reader macros (like `#'foo/bar`) within your Clojure code "
		 "   generally do NOT need special escaping for this tool *beyond what's needed by Clojure itself*, as they will be "
		 "   protected by the outer double quotes of the 'echo' command.\n"
		 "4. As per standard tool usage, if you wrap your 'code' argument in \'\'\'...\'\'\' for the tool call and the Clojure "
		 "   code *itself* contains a literal \'\'\' sequence, you must escape that as '\\'\\'\\''.\n\n"
		 "This tool allows execution of arbitrary Clojure code within the REPL. Useful for evaluation, running specific test functions with fixtures, or any other REPL interaction. "
		 "Requires a Leiningen nREPL server to be running and connectable.")
   :args (list
	  '(:name "code"
		  :type string
		  :description "The Clojure code string to execute. CRITICAL: See main tool description for how to format and escape this string, especially internal double quotes.")
	  '(:name "host"
		  :type string
		  :description "Optional: The hostname of the nREPL server. Defaults to 'localhost' if empty string or not provided.")
	  '(:name "port"
		  :type integer
		  :description "Optional: The port number of the nREPL server. Defaults to 7888 if 0, empty or not provided.")
	  '(:name "working_dir"
		  :type string
		  :description "Optional: The directory in which to run the 'lein repl :connect' command. Defaults to the current Emacs default-directory if empty string or not specified."))
   :category "clojure"
   :include t)

  :bind
  ("C-c RET" . gptel-send)
  ("C-c C-<return> g" . gptel)
  ("C-c C-<return> m" . gptel-menu)
  ("C-c C-<return> a" . gptel-add)
  ("C-c C-<return> A" . gptel-abort)
  :hook
  (gptel-mode . visual-line-mode)
  (gptel-post-stream . gptel-auto-scroll))

(use-package gptel-aibo
  :after (gptel flycheck)
  :straight (:host github :repo "dolmens/gptel-aibo"))

(use-package mcp
  :after gptel
  :config
  (require 'gptel-integrations)
  (require 'mcp-hub)
  :hook
  (after-init . mcp-hub-start-all-server))

(use-package gptel-agent
  :straight (:host github :repo "karthink/gptel-agent" :files ("*.el" "agents"))
  :config
  (gptel-agent-update))

(provide 'init-gptel)
