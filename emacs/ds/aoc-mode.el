(defvar aoc-year 2024
  "The year for which Advent of Code is relevant.")

(defvar aoc-template-dir (expand-file-name "~/dotfiles/templates/")
  "Directory where language templates are stored.")

(defvar aoc-session-cookie nil
  "Session cookie for Advent of Code. Set this once to avoid repeated prompting.")

(defvar aoc-dir (expand-file-name "~/src/dhruvasagar/comp/")
  "Directory where aoc code lives")

(defvar aoc-mode-hook nil)

(define-minor-mode aoc-mode
  "Minor mode for Advent Of Code."
  :lighter " AoC"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c C-a d") 'aoc-download-input)
	    (define-key map (kbd "C-c C-a r") 'aoc-run-input)
	    (define-key map (kbd "C-c C-a t") 'aoc-run-test-input)
	    (define-key map (kbd "C-c C-a g") 'aoc-generate-template)
	    map)
  (when (featurep 'evil)
    ;; Evil Normal Mode bindings
    (evil-define-key 'normal aoc-mode-map (kbd "SPC a d") 'aoc-download-input)
    (evil-define-key 'normal aoc-mode-map (kbd "SPC a r") 'aoc-run-input)
    (evil-define-key 'normal aoc-mode-map (kbd "SPC a t") 'aoc-run-test-input)
    (evil-define-key 'normal aoc-mode-map (kbd "SPC a g") 'aoc-generate-template))
  (run-hooks 'aoc-mode-hook))

(defun aoc-set-session-cookie ()
  "Set the Advent of Code session cookie."
  (interactive)
  (setq aoc-session-cookie (read-string "Enter your session cookie: "))
  (message "Session cookie set."))

(defun aoc-download-input ()
  "Download the input for the day as extracted from the file name."
  (interactive)
  (let* ((file-name (buffer-file-name))
	 (day (if (string-match "\\(\\d+\\)" file-name)
		  (string-to-number (match-string 1 file-name))
		(error "File name must contain a day number.")))
	 (url (format "https://adventofcode.com/%d/day/%d/input" aoc-year day))
	 (output (shell-command-to-string
		  (format "curl -s -H 'Cookie: session=%s' %s" aoc-session-cookie url))))
    (if (string-match-p "not found" output)
	(message "No input found for day %d." day)
      (with-temp-buffer
	(insert output)
	(write-file "input"))
      (message "Input downloaded for day %d." day))))

(defun aoc-compile-command (input-file)
  (let* ((file-name (buffer-file-name))
	 (day (if (string-match "\\(\\d+\\)" file-name)
		  (string-to-number (match-string 1 file-name))
		(error "File name must contain a day number.")))
	 (command (cond
		   ((string-match "\\.py$" file-name) (format "python3 %s < %s" file-name input-file))
		   ((string-match "\\.rb$" file-name) (format "ruby %s < %s" file-name input-file))
		   ((string-match "\\.js$" file-name) (format "node %s < %s" file-name input-file))
		   ((string-match "\\.go$" file-name) (format "go run %s < %s" file-name input-file))
		   ((string-match "\\.zig$" file-name) (format "zig run %s < %s" file-name input-file))
		   ((string-match "\\.rs$" file-name) (format "cargo build && target/debug/%s < %s" file-name input-file))
		   ((string-match "\\.c$" file-name) (format "gcc %s -o bin/day%02d-c && bin/day%02d-c < %s" file-name day day input-file))
		   ((string-match "\\.hs$" file-name) (format "ghc %s -o bin/day%02d-hs && bin/day%02d-hs < %s" file-name day day input-file))
		   ((string-match "\\.cpp$" file-name) (format "g++ -std=c++17 %s -o bin/day%02d-cpp && bin/day%02d-cpp < %s" file-name day day input-file))
		   ((string-match "\\.java$" file-name) (format "javac -d bin %s && bin/%s < %s" file-name file-name input-file))
		   (t (error "Unsupported file type: %s" file-name)))))
    (set (make-local-variable 'compile-command) command)
    (compile compile-command)))

(defun aoc-run-input ()
  "Run the current code file with 'input' as the input file extracted from the file name."
  (interactive)
  (aoc-compile-command "input"))

(defun aoc-run-test-input ()
  "Run the current code file with 'tinput' as the input file extracted from the file name."
  (interactive)
  (aoc-compile-command "tinput"))

(defun aoc-language-extension (language)
  "Return the file extension corresponding to the given LANGUAGE."
  (interactive "sEnter Language (python, ruby, js, golang, haskell, rust, zig, cpp): ")
  (let ((extension (cond
		    ((string= language "python") "py")
		    ((string= language "ruby") "rb")
		    ((string= language "js") "js")
		    ((string= language "golang") "go")
		    ((string= language "haskell") "hs")
		    ((string= language "rust") "rs")
		    ((string= language "zig") "zig")
		    ((string= language "cpp") "cpp")
		    (t (error "Unsupported language: %s" language)))))
    (message "The file extension for %s is %s." language extension)
    extension))

(defun aoc-generate-template (language)
  "Generate a template for the specified LANGUAGE from the template directory."
  (interactive "sEnter Language (python, ruby, or js): ")
  (let* ((day (format "%02d" (read-number "Enter Day (1-25): ")))
	 (extn (aoc-language-extension language))
	 (template-file (concat (file-name-as-directory aoc-template-dir)
				(format "%s/file.%s" extn extn)))
	 (template (if (file-exists-p template-file)
		       (with-temp-buffer
			 (insert-file-contents template-file)
			 (buffer-string))
		     (error "Template file %s does not exist." template-file))))
    (with-temp-buffer
      (insert (format template day))
      (write-file (format "day%d.%s" day extn))
      (message "Template generated for day %d in %s." day language))))

(defun aoc-mode-enable-if-in-aoc-dir ()
  "Enable my-minor-mode if the file is in the specific directory."
  (when (and (buffer-file-name)
	     (string-prefix-p aoc-dir (buffer-file-name)))
    (aoc-mode)))

(provide 'aoc-mode)
