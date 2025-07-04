(add-to-list 'load-path "~/.emacs.d/ds")

(defconst CACHE-DIR (expand-file-name "cache/" user-emacs-directory))

;; NOTE: I use F1 as C-h (paging & help).
(bind-keys*
 :prefix-map fk/menu-map
 :prefix "M-m"
 ("M-m" . which-key-show-major-mode)
 ("M-h" . help-command)
 ("M-u" . universal-argument)
 :map fk/menu-map :prefix-map buffers         :prefix "b"
 :map fk/menu-map :prefix-map comments        :prefix "c"
 :map fk/menu-map :prefix-map django          :prefix "d"
 :map fk/menu-map :prefix-map errors          :prefix "e"
 :map fk/menu-map :prefix-map files           :prefix "f"
 :map fk/menu-map :prefix-map org             :prefix "o"
 :map fk/menu-map :prefix-map text            :prefix "t"
 :map fk/menu-map :prefix-map version-control :prefix "v"
 :map fk/menu-map :prefix-map windows         :prefix "w")

;; (setq debug-on-error t)
;; (view-echo-area-messages)
(setq use-package-verbose t)
(setq evil-want-keybinding nil)

(require 'ds)

(add-hook 'prog-mode-hook 'aoc-mode-enable-if-in-aoc-dir)

(setq-default
 ring-bell-function 'ignore                    ; prevent beep sound.
 inhibit-startup-screen t                      ; TODO: maybe better on early-init or performance?
 initial-major-mode 'fundamental-mode          ; TODO: maybe better on early-init or performance?
 initial-scratch-message nil                   ; TODO: maybe better on early-init?
 create-lockfiles nil                          ; .#locked-file-name
 confirm-kill-processes nil                    ; exit emacs without asking to kill processes
 backup-inhibited t
 make-backup-files nil
 backup-by-copying t                           ; prevent linked files
 require-final-newline t                       ; always end files with newline
 delete-old-versions t                         ; don't ask to delete old backup files
 revert-without-query '(".*")                  ; `revert-buffer' without confirmation
 uniquify-buffer-name-style 'forward           ; non-unique buffer name display: unique-part/non-unique-filename
 fast-but-imprecise-scrolling t                ; supposed to make scrolling faster on hold
 window-resize-pixelwise t                     ; correctly resize windows by pixels (e.g. in split-window functions)
 native-comp-async-report-warnings-errors nil  ; disable annoying native-comp warnings
 ad-redefinition-action 'accept                ; disable annoying "ad-handle-definition: ‘some-function’ got redefined" warnings
 use-short-answers t                           ; e.g. `y-or-n-p' instead of `yes-or-no-p'
 auto-save-list-file-prefix nil
 help-enable-symbol-autoload t)                ; perform autoload if docs are missing from autoload objects.

(global-auto-revert-mode)

(save-place-mode)

(global-so-long-mode)

(bind-key* "M-r" 'repeat)

(defun fk/add-local-hook (hook function)
  "Add buffer-local hook."
  (add-hook hook function :local t))

(defun fk/async-process (command &optional name filter)
  "Start an async process by running the COMMAND string with bash. Return the
process object for it.

NAME is name for the process. Default is \"async-process\".

FILTER is function that runs after the process is finished, its args should be
\"(process output)\". Default is just messages the output."
  (make-process
   :command `("bash" "-c" ,command)
   :name (if name name
           "async-process")
   :filter (if filter filter
             (lambda (process output) (message (s-trim output))))))

;; Examples:
;;
;; (fk/async-process "ls")
;;
;; (fk/async-process "ls" "my ls process"
;;                   (lambda (process output) (message "Output:\n\n%s" output)))
;;
;; (fk/async-process "unknown command")

;; Make sure to focus when a new emacsclient frame created.
(add-hook 'server-after-make-frame-hook (lambda () (select-frame-set-input-focus (selected-frame))))

(defalias 'narrow-quit 'widen)  ; I forget `widen' everytime

;; TODO: lset would be useful too
(defmacro l (func &rest args)
  "Shorter lambda."
  `(lambda nil (apply ,func '(,@args))))

(defmacro li (func &rest args)
  "Shorter lambda, interactive."
  `(lambda nil (interactive) (apply ,func '(,@args))))

;; Examples:
;; (global-set-key (kbd "C-V") (lambda () (interactive) (next-line 10))) <-- Classical
;; (global-set-key (kbd "C-V") (li 'next-line 10)) <-- With li macro

(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none
      mac-pass-command-to-system nil)

(setq-default
 truncate-lines t
 frame-resize-pixelwise t             ; maximized emacs may not fit screen without this
 frame-title-format '("Emacs | %b"))  ; Emacs | buffer-name
