(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-tree)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (setq evil-want-integration t)
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "-") 'dired-jump)
  (define-key evil-normal-state-map (kbd "C-l") 'redraw-display)
  (define-key evil-insert-state-map (kbd "C-c C-u") 'evil-delete-back-to-indentation))

(use-package evil-collection
  :after evil
  :init
  (evil-collection-init))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-rails)

(use-package evil-commentary
  :config
  (evil-commentary-mode))

(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))

(use-package evil-exchange
  :config
  (evil-exchange-cx-install))

(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode t))

(use-package calendar
  :commands calendar
  :custom
  (calendar-week-start-day 1)  ; start at Monday
  :hook
  (calendar-mode . delete-other-windows))

(use-package nord-theme
  :config
  (load-theme 'nord t))

(use-package which-key
  :config
  (which-key-mode))

(use-package helpful
  :custom
  ;; Use helpful in `helm-apropos'
  (helm-describe-function-function 'helpful-function)
  (Helm-describe-variable-function 'helpful-variable)
  :bind
  (([remap describe-function] . helpful-callable)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-key] . helpful-key)
   :map emacs-lisp-mode-map
   ("C-c C-d" . helpful-at-point)))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom    
  (doom-modeline-height 25)
  (doom-modeline-bar-width 1)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-buffer-encoding t)
  (doom-modeline-indent-info nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-vcs-max-length 20)
  (doom-modeline-env-version t)
  (doom-modeline-irc-stylize 'identity)
  (doom-modeline-github-timer nil)
  (doom-modeline-gnus-timer nil))

(use-package anzu
  :hook
  (dashboard-after-initialize . global-anzu-mode))

(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-auto-enabled nil)
  (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  (set-face-background 'highlight-indent-guides-even-face "dimgray")
  (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  :hook
  (prog-mode . highlight-indent-guides-mode))

(use-package dashboard
  :init
  (setq dashboard-center-content t)
  (setq dashboard-startup-banner "~/Downloads/128px-Neovim-mark.svg.png")
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title " Remember (Neo)VIM Is Always Better  ")
  (setq dashboard-set-heading-icon t)
  :config
  (dashboard-setup-startup-hook))

(use-package tree-sitter
  :commands fk/tree-sitter-hl-mode
  :config
  (defun fk/tree-sitter-hl-mode ()
    "Require `tree-sitter-langs' + Activate `tree-sitter-hl-mode'."
    (interactive)
    (require 'tree-sitter-langs)
    (call-interactively 'tree-sitter-hl-mode))

  (with-eval-after-load 'expand-region
    (defun tree-sitter-mark-bigger-node ()
      (interactive)
      (let* ((p (point))
             (m (or (mark) p))
             (beg (min p m))
             (end (max p m))
             (root (ts-root-node tree-sitter-tree))
             (node (ts-get-descendant-for-position-range root beg end))
             (node-beg (ts-node-start-position node))
             (node-end (ts-node-end-position node)))
        ;; Node fits the region exactly. Try its parent node instead.
        (when (and (= beg node-beg) (= end node-end))
          (when-let ((node (ts-get-parent node)))
            (setq node-beg (ts-node-start-position node)
                  node-end (ts-node-end-position node))))
        (set-mark node-end)
        (goto-char node-beg)))

    (setq er/try-expand-list (append er/try-expand-list
                                     '(tree-sitter-mark-bigger-node)))))

(use-package tree-sitter-langs
  :defer t
  :config
  ;; Custom patterns to make it look like in old versions:
  ;; See: https://github.com/ubolonton/emacs-tree-sitter/issues/153
  (tree-sitter-hl-add-patterns 'python
    [(assignment left: (identifier) @variable)])

  (tree-sitter-hl-add-patterns 'python
    [(decorator (call (identifier) @function.special))]))

(use-package goggles
  :commands goggles-mode
  :custom
  (goggles-pulse-delay 0.1))

(use-package hideshow
  :defer nil
  :custom
  (hs-isearch-open t)
  :bind
  ( :map hs-minor-mode-map
    ("TAB" . fk/hs-smart-tab)
    ("<tab>" . fk/hs-smart-tab)
    ("<backtab>" . hs-toggle-hiding))
  :config
  (defun fk/hs-smart-tab ()
    "Pretend like `hs-toggle-hiding' if point is on a hiding block."
    (interactive)
    (if (save-excursion
          (move-beginning-of-line 1)
          (hs-looking-at-block-start-p))
        (hs-show-block)
      (indent-for-tab-command)))

  (defun fk/hide-second-level-blocks ()
    "Hide second level blocks (mostly class methods in python) in
current buffer."
    (interactive)
    (hs-minor-mode)
    (save-excursion
      (goto-char (point-min))
      (hs-hide-level 2))))

(use-package helm
  :config (helm-mode)
  :custom
  (helm-M-x-always-save-history t)
  (helm-M-x-reverse-history t)
  (helm-display-function 'pop-to-buffer)
  (savehist-additional-variables '(extended-command-history))
  (history-delete-duplicates t)
  (helm-command-prefix-key nil)
  ;; Just move the selected text to the top of kill-ring, do not insert the text
  (helm-kill-ring-actions '(("Copy marked" . (lambda (_str) (kill-new _str)))
                            ("Delete marked" . helm-kill-ring-action-delete)))
  :custom-face
  (helm-non-file-buffer ((t (:inherit font-lock-comment-face))))
  (helm-ff-file-extension ((t (:inherit default))))
  (helm-buffer-file ((t (:inherit default))))
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x C-b" . helm-buffers-list)
   ("C-x b" . helm-buffers-list)
   ("C-x C-r" . helm-recentf)
   ("C-x C-i" . fk/helm-imenu)
   ("C-x C-j" . fk/helm-imenu)
   ("M-y" . fk/yank-pop-or-helm-show-kill-ring)
   :map helm-map
   ("TAB" . helm-execute-persistent-action)
   ("<tab>" . helm-execute-persistent-action)
   ("C-z" . helm-select-action)
   ("C-w" . backward-kill-word)  ; Fix C-w
   :map files
   ("f" . helm-find-files)
   ("r" . helm-recentf)
   ("b" . helm-bookmarks)
   :map buffers
   ("b" . helm-buffers-list)
   :map help-map
   ("a" . helm-apropos))
  :hook
  (dashboard-after-initialize . helm-mode)
  (helm-mode . savehist-mode)
  :config
  (with-eval-after-load 'helm-buffers
    (dolist (regexp '("\\*epc con" "\\*helm" "\\*straight" "\\*Flymake"
                      "\\*eldoc" "\\*Compile-Log" "\\*xref" "\\*company"
                      "\\*aw-posframe" "\\*Warnings" "\\*Backtrace" "\\*helpful"
                      "\\*Messages" "\\*dashboard"))
      (add-to-list 'helm-boring-buffer-regexp-list regexp))
    (bind-keys
     :map helm-buffer-map
     ("M-d" . helm-buffer-run-kill-buffers)
     ("C-M-d" . helm-buffer-run-kill-persistent)))

  ;; "Waiting for process to die...done" fix.
  ;; Source: https://github.com/bbatsov/helm-projectile/issues/136#issuecomment-688444955
  (defun fk/helm--collect-matches (orig-fun src-list &rest args)
    (let ((matches
           (cl-loop for src in src-list
                    collect (helm-compute-matches src))))
      (unless (eq matches t) matches)))

  (advice-add 'helm--collect-matches :around 'fk/helm--collect-matches)

  (require 'helm-imenu)  ; Fixes buggy helm-imenu at first usage

  (defun fk/helm-imenu ()
    "helm-imenu without initializion (preselect)."
    (interactive)
    (unless helm-source-imenu
      (setq helm-source-imenu
            (helm-make-source "Imenu" 'helm-imenu-source
              :fuzzy-match helm-imenu-fuzzy-match)))
    (let* ((imenu-auto-rescan t)
           (helm-highlight-matches-around-point-max-lines 'never))
      (helm :sources 'helm-source-imenu
            :default ""
            :preselect ""
            :buffer "*helm imenu*")))

  ;; (add-hook 'imenu-after-jump-hook (lambda ()
  ;;                                    (when (derived-mode-p 'outline-mode)
  ;;                                      (show-subtree))))

  (defun fk/yank-pop-or-helm-show-kill-ring ()
    "If called after a yank, call `yank-pop'. Otherwise, call
`helm-show-kill-ring'."
    (interactive)
    (if (eq last-command 'yank)
        (if (eq major-mode 'vterm-mode)
            (vterm-yank-pop)
          (yank-pop))
      (helm-show-kill-ring))))

(use-package yasnippet
 ;; Expand snippets with `C-j', not with `TAB'. Use `TAB' to always
 ;; jump to next field, even when company window is active. If there
 ;; is need to complete company's selection, use `C-s'
 ;; (`company-complete-selection').
 :custom
 (yas-indent-line nil)
 (yas-inhibit-overlay-modification-protection t)
 :custom-face
 (yas-field-highlight-face ((t (:inherit region))))
 :bind*
 (("C-j" . yas-expand)
  :map yas-minor-mode-map
  ("TAB" . nil)
  ("<tab>" . nil)
  :map yas-keymap
  ("TAB" . (lambda () (interactive) (company-abort) (yas-next-field)))
  ("<tab>" . (lambda () (interactive) (company-abort) (yas-next-field))))
 :hook
 (dashboard-after-initialize . yas-global-mode)
 (snippet-mode . (lambda () (setq-local require-final-newline nil))))

(use-package hydra
  :defer t
  :init
  (setq hydra-hint-display-type 'posframe))

(use-package company
  :config
  (global-company-mode t)
  (setq-default
   company-idle-delay 0.05
   company-require-match nil
   company-minimum-prefix-length 0
   ;; get only preview
   ;; company-frontends '(company-preview-frontend)
   ;; also get a drop down
   company-frontends '(company-pseudo-tooltip-frontend company-preview-frontend)
   ))

(use-package mwim
 :bind
 ("C-a" . mwim-beginning-of-code-or-line)
 ("C-e" . mwim-end-of-line-or-code)
 ;; NOTE: Functions below are built-in but I think they fit in this context
 ("M-a" . fk/backward-sexp)
 ("M-e" . fk/forward-sexp)
 :config
 (defun fk/forward-sexp (&optional N)
   "Call `forward-sexp', fallback `forward-char' on error."
   (interactive)
   (condition-case nil
       (forward-sexp N)
     (error (forward-char N))))

 (defun fk/backward-sexp ()
   "`fk/forward-sexp' with negative argument."
   (interactive)
   (fk/forward-sexp -1)))

(use-package projectile
  :custom
  (projectile-auto-discover nil)
  (projectile-project-search-path (directory-files "~/src" t "[^.]"))
  ;; Open magit when switching project
  (projectile-switch-project-action
   (lambda ()
     (let ((magit-display-buffer-function
            'magit-display-buffer-same-window-except-diff-v1))
       (magit))))
  ;; Ignore emacs project (source codes)
  (projectile-ignored-projects '("~/emacs/"))
  ;; Do not include straight repos (emacs packages) and emacs directory itself
  ;; to project list
  (projectile-ignored-project-function
   (lambda (project-root)
     (or (string-prefix-p (expand-file-name user-emacs-directory) project-root)
         (string-prefix-p "/usr/lib/node_modules/" project-root))))
  (projectile-kill-buffers-filter 'kill-only-files)
  :hook
  (dashboard-after-initialize . projectile-mode))

(use-package helm-projectile
  :custom
  (helm-projectile-sources-list '(helm-source-projectile-buffers-list
                                  helm-source-projectile-recentf-list
                                  helm-source-projectile-files-list
                                  helm-source-projectile-projects))
  :bind
  ("C-x f" . helm-projectile)
  :hook
  (projectile-mode . helm-projectile-on)
  :config
  (defun fk/projectile-recentf-files-first-five (original-function)
    "Return a list of five recently visited files in a project."
    (let ((files (funcall original-function)))
      (if (> (length files) 5)
          (seq-subseq files 0 5)
        files)))
  (advice-add 'projectile-recentf-files :around 'fk/projectile-recentf-files-first-five))

(use-package helm-rg
  :init
  ;; Load this macro even if helm-rg is not loaded yet
  (defmacro fk/helm-rg-define-search-command (name keymap kbd &optional glob query)
    "Define search commands and keybindings with predefined glob and query. Usage:
(fk/helm-rg-define-search-command
 \"my-search\" global-map \"C-M-s\" \"*.el\" \"foo\")"
    `(progn
       (defun ,(intern (concat "fk/" name)) ()
         (interactive)
         (require 'helm-rg)
         (fk/helm-rg-dwim-with-glob (or ,glob "") ,query))
       (define-key ,keymap (kbd ,kbd) ',(intern (concat "fk/" name)))))

  (defmacro fk/helm-rg-define-search-commands (&rest args)
    "Define multiple search command at once. Usage:
(fk/helm-rg-define-search-commands
 (\"my-search\" global-map \"C-M-s\" \"*.el\" \"foo\")
 (\"my-other-search\" global-map \"C-M-S\" \"*.el\" \"bar\"))"
    `(progn ,@(cl-loop for expr in args
                       collect `(fk/helm-rg-define-search-command ,@expr))))
  :custom
  (helm-rg-default-extra-args '("--max-columns" "400"
                                "-g" "!{*.min.css,*.min.js,*.svg,*.po}"
                                "-g" "!migrations/"))
  :custom-face
  (helm-rg-file-match-face ((t (:foreground nil :inherit font-lock-type-face :weight bold :underline nil :slant italic))))
  (helm-rg-line-number-match-face ((t (:foreground nil :underline nil :inherit line-number))))
  :bind
  (("C-M-s" . fk/helm-rg-dwim)
   :map helm-rg-map
   ("C-c C-e" . fk/helm-rg-switch-helm-ag)
   ("C-c C-d" . fk/helm-rg-switch-deadgrep))
  :config
  (defun fk/helm-rg-dwim (&optional query)
    "Smarter version of helm-rg.
- Search in project if in a project else search in default (current) directory.
- Start search with selected text if region is active or empty string.
- Escape special characters when searching with selected text."
    (interactive)
    (let ((helm-rg-default-directory (or (projectile-project-root) default-directory))
          (query (or (fk/get-selected-text) query)))
      (cl-letf (((symbol-function 'helm-rg--get-thing-at-pt) (lambda () query)))
        (call-interactively 'helm-rg))))

  (defun fk/helm-rg-dwim-with-glob (glob &optional query)
    (interactive)
    (let ((helm-rg-default-glob-string glob))
      (fk/helm-rg-dwim query)))


  ;;;; Input normalization

  (defvar fk/helm-rg-fuzzy-max-words 6)  ; rg returns "Arguments list too long" after this point

  (defun fk/helm-input-to-ripgrep-regexp (func input)
    "Make `helm-rg' input ripgrep compatible. Escape special
characters and disable fuzzy matching if input has more than
`fk/helm-rg-fuzzy-max-words' words."
    (let* ((processed-input (fk/convert-string-to-rg-compatible input))
           (word-count (with-temp-buffer
                         (insert processed-input)
                         (count-words (point-min) (point-max)))))
      (if (> word-count fk/helm-rg-fuzzy-max-words)
          (string-replace " " ".*" processed-input)  ; simpler fuzzy
        (apply func (list processed-input)))))

  (advice-add 'helm-rg--helm-pattern-to-ripgrep-regexp :around 'fk/helm-input-to-ripgrep-regexp)


  ;;;; Appearance

  ;; Use a simpler header in the helm buffer.
  (fset 'helm-rg--header-name
        (lambda (_)
          (format "Search at %s\nArgs: %s" helm-rg--current-dir (string-join helm-rg--extra-args " "))))

  ;; Create bigger window for helm-rg
  (advice-add 'helm-rg :around
              (lambda (orig-func &rest args)
                (let ((helm-posframe-min-height (round (* (frame-height) 0.66)))
                      (helm-candidate-number-limit 99999))  ; show all matching lines. TODO open a PR and make this default.
                  (apply orig-func args))))


  ;;;; Switch to another frontend functions

  (defun fk/helm-rg-switch-helm-ag ()
    "Switch to `helm-ag' to use its edit feature."
    (interactive)
    (helm-rg--run-after-exit
     (require 'helm-ag)
     (fk/helm-ag-dwim helm-pattern))
    (minibuffer-keyboard-quit))

  (defun fk/helm-rg-switch-deadgrep ()
    "Switch to `deadgrep' to use its seperated buffer and `before n line' / `after n line' features."
    (interactive)
    (helm-rg--run-after-exit
     (require 'deadgrep)
     (deadgrep helm-pattern))
    (minibuffer-keyboard-quit)))

(use-package undo-tree
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-enable-undo-in-region t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  ;; :bind
  ;; (("C-u" . undo-tree-undo)
  ;;  ("C-S-u" . undo-tree-redo))
  :hook
  (dashboard-after-initialize . global-undo-tree-mode))

(use-package whitespace-cleanup-mode
  :custom
  (show-trailing-whitespace t)  ; not from whitespace-cleanup-mode.el
  :hook
  (dashboard-after-initialize . global-whitespace-cleanup-mode)
  (after-change-major-mode . (lambda ()
                              (unless (buffer-file-name)
                                (setq-local show-trailing-whitespace nil)))))

(use-package expand-region
  :custom
  (expand-region-fast-keys-enabled nil)
  (expand-region-subword-enabled t)
  :bind*
  ("C-t" . er/expand-region))

(use-package flyspell-popup
  :after flyspell
  :custom
  (flyspell-popup-correct-delay 1)
  :config
  (flyspell-popup-auto-correct-mode))

(use-package flycheck
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-disabled-checkers '(python-pycompile python-mypy python-pylint python-pyright))
  :bind
  ( :map errors
    ("n" . flycheck-next-error)
    ("p" . flycheck-previous-error)
    ("l" . flycheck-list-errors)
    ("v" . flycheck-verify-setup))
)

;; (use-package eglot
;;   :commands eglot
;;   :init
;;   (setq eglot-stay-out-of '(flymake))
;;   :custom
;;   (eglot-ignored-server-capabilites '(:documentHighlightProvider))
;;   (eglot-autoshutdown t)
;;   :hook
;;   ;; (eglot-managed-mode . eldoc-box-hover-mode)
;;   (eglot-managed-mode . fk/company-enable-snippets)
;;   :config
;;   (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
;;   (with-eval-after-load 'eglot
;;     (load-library "project")))

(use-package eldoc-box
  :commands (eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
  :custom
  (eldoc-box-clear-with-C-g t))

(use-package symbol-overlay
  :commands (symbol-overlay-mode symbol-overlay-put fk/highlight-occurrences)
  :bind
  ( :map symbol-overlay-mode-map
    ("C-c C-n" . symbol-overlay-jump-next)
    ("C-c C-p" . symbol-overlay-jump-prev))
  :hook
  (emacs-lisp-mode . symbol-overlay-mode)
  (python-mode . symbol-overlay-mode)
  :config
  (defun fk/highlight-occurrences ()
    "Put highlight to the occurrences of the symbol at point or the
string in the region. Uses `hi-lock' to highlight,
`symbol-overlay' to generate a random face. To remove highlights,
use `hi-lock-unface-buffer' or disable `hi-lock-mode'."
    ;; TODO: `hl-line' breaks background color
    (interactive)
    (let ((str (fk/get-selected-text))
          (face (nth (random (length symbol-overlay-faces)) symbol-overlay-faces)))
      (if str
          (highlight-regexp (regexp-quote str) face)
        (hi-lock-face-symbol-at-point))))

  (defalias 'fk/highlight-remove (lambda () (interactive) (hi-lock-unface-buffer t)))
  (defalias 'fk/highlight-remove-one-by-one 'hi-lock-unface-buffer))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package python
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "-i --simple-prompt")
  (python-indent-guess-indent-offset-verbose nil)
  :bind
  ( :map python-mode-map
    ("C-c r" . python-indent-shift-right)
    ("C-c l" . python-indent-shift-left))
  :hook
  ;; With pyls:
  ;; pip install python-language-server flake8 pyls-black(optional) pyls-isort(optional)
  ;; With pyright
  ;; sudo npm install -g pyright && pip install flake8 black(optional) django-stubs(optional)
  ;; NOTE: these hooks runs in reverse order
  (python-mode . (lambda () (setq-local company-prescient-sort-length-enable nil)))
  (python-mode . (lambda () (unless (and buffer-file-name (file-in-directory-p buffer-file-name "~/.virtualenvs/"))
                              (flycheck-mode))))
  (python-mode . lsp)
  ;; importmagic runs ~100mb ipython process per python file, and it does not
  ;; always find imports, 60%-70% maybe. I stop using this, but still want to keep.
  ;; (python-mode . importmagic-mode)
  (python-mode . fk/activate-pyvenv)
  (python-mode . (lambda ()
                   (when (and (buffer-file-name)
                              (string=
                               (car (last (f-split (f-parent (buffer-file-name)))))
                               "tests"))
                     (fk/hide-second-level-blocks))))
  (python-mode . fk/tree-sitter-hl-mode)
  (python-mode . (lambda () (setq-local fill-column 88)))
  :config
  (defvar python-walrus-operator-regexp ":=")

  ;; Make walrus operator ":=" more visible
  (font-lock-add-keywords
   'python-mode
   `((,python-walrus-operator-regexp 0 'escape-glyph t))
   'set))

(use-package pyvenv
  :after projectile
  :config
  ;; I show this in `fk/minibuffer-modeline-update' manually.
  (setq pyvenv-mode-line-indicator nil)

  (defun fk/get-venv-name ()
    "Get venv name of current python project."
    (when-let* ((root-dir (projectile-project-root))
                (venv-file (concat root-dir ".venv"))
                (venv-exists (file-exists-p venv-file))
                (venv-name (with-temp-buffer
                             (insert-file-contents venv-file)
                             (nth 0 (split-string (buffer-string))))))
      venv-name))

  (defun fk/activate-pyvenv ()
    "Activate python environment according to the `project-root/.venv' file."
    (interactive)
    (when-let ((venv-name (fk/get-venv-name)))
      (pyvenv-mode)
      (pyvenv-workon venv-name)))

  (defun fk/open-venv-dir ()
    "Open the directory of installed libraries in `dired'."
    (interactive)
    (when-let* ((venv-name (fk/get-venv-name))
                (venv-dir (expand-file-name venv-name "~/.virtualenvs")))
      (dired (car (directory-files-recursively venv-dir "site-packages" t)))))

  ;; python-mode hook is not enough when more than one project's files are open.
  ;; It just re-activate pyvenv when a new file is opened, it should re-activate
  ;; on buffer or perspective switching too. NOTE: restarting lsp server is
  ;; heavy, so it should be done manually if needed.
  (add-hook 'window-configuration-change-hook 'fk/activate-pyvenv))

(use-package web-mode
  :custom
  (css-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-enable-auto-indentation nil)
  (web-mode-enable-auto-pairing nil)
  (web-mode-engines-alist '(("django" . "\\.html\\'")))
  :custom-face
  (web-mode-block-string-face ((t (:inherit font-lock-string-face))))
  (web-mode-html-attr-value-face ((t (:inherit font-lock-string-face :foreground nil))))
  (web-mode-current-element-highlight-face ((t (:inherit highlight))))
  :mode ;; Copied from spacemacs
  (("\\.phtml\\'"      . web-mode)
   ("\\.tpl\\.php\\'"  . web-mode)
   ("\\.twig\\'"       . web-mode)
   ("\\.xml\\'"        . web-mode)
   ("\\.html\\'"       . web-mode)
   ("\\.htm\\'"        . web-mode)
   ("\\.[gj]sp\\'"     . web-mode)
   ("\\.as[cp]x?\\'"   . web-mode)
   ("\\.eex\\'"        . web-mode)
   ("\\.erb\\'"        . web-mode)
   ("\\.mustache\\'"   . web-mode)
   ("\\.handlebars\\'" . web-mode)
   ("\\.hbs\\'"        . web-mode)
   ("\\.eco\\'"        . web-mode)
   ("\\.ejs\\'"        . web-mode)
   ("\\.svelte\\'"     . web-mode)
   ("\\.djhtml\\'"     . web-mode)
   ("\\.mjml\\'"       . web-mode))
  :hook
  (web-mode . web-mode-toggle-current-element-highlight))

(use-package emmet-mode
  :custom
  (emmet-move-cursor-between-quotes t)
  :custom-face
  (emmet-preview-input ((t (:inherit lazy-highlight))))
  :bind
  ( :map emmet-mode-keymap
    ([remap yas-expand] . emmet-expand-line)
    ("M-n"  . emmet-next-edit-point)
    ("M-p"  . emmet-prev-edit-point)
    ("C-c p" . emmet-preview-mode))
  :hook
  ;;(rjsx-mode . (lambda () (setq emmet-expand-jsx-className? t)))
  (web-mode . emmet-mode)
  (css-mode . emmet-mode))

(use-package helm-emmet
  :after helm emmet)

(use-package json-mode
  :mode ("\\.json\\'" . json-mode))
(use-package json-navigator
  :commands json-navigator-navigate-region)

(use-package js2-mode
  :mode "\\.js\\'"
  :custom
  (js-indent-level 2)
  :hook
  (js2-mode . flycheck-mode)
  ;;(js2-mode . (lambda () (require 'tree-sitter-langs) (tree-sitter-hl-mode)))
  (js2-mode . lsp-deferred))

(use-package go-mode
  ;; install go & go-tools, for arch based linux:
  ;; sudo pacman -S go go-tools
  :mode "\\.go\\'"
  :custom
  (gofmt-command "goimports")
  :hook
  (go-mode . flycheck-mode)
  (go-mode . lsp-deferred)
  (go-mode . (lambda () (require 'tree-sitter-langs) (tree-sitter-hl-mode)))
  (go-mode . (lambda () (fk/add-local-hook 'before-save-hook 'gofmt))))

(use-package cc-mode
  :bind
  ( :map c-mode-base-map
    ("C-c C-c" . fk/c-run))
  :hook
  (c-mode . lsp-deferred)
  (c++-mode . lsp-deferred))

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package magit
  :commands magit
  :custom
  (magit-section-initial-visibility-alist '((stashes . show)
                                            (unpushed . show)
                                            (pullreqs . show)
                                            (issues . show)))
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  :bind*
  ( :map version-control
    ("v" . magit-status)
    ("s" . magit-status)
    :map magit-mode-map
    ("o" . (lambda () (interactive)
             (call-interactively 'magit-diff-visit-file-other-window)
             (recenter-top-bottom)))
    ("C-c C-f" . magit-find-file))
  :hook
  (git-commit-setup . git-commit-turn-on-flyspell)
  (magit-mode . hack-dir-local-variables-non-file-buffer))

(use-package magit-todos
  :commands magit-todos-list
  :custom
  (magit-todos-exclude-globs '("*jquery*.js" "*min.js" "*min.css" "*.mjml"))
  (magit-todos-max-items 40)  ; It's actually 20 but needed to set x2 for some reason
  :bind*
  ( :map version-control
    ("T" . magit-todos-list))
  :hook (magit-mode . magit-todos-mode))

(use-package git-link
  :commands git-link
  :custom
  (git-link-use-commit t)
  :bind
  ( :map version-control
    ("l" . git-link)))

(use-package git-timemachine
  :commands git-timemachine
  :bind
  ( :map version-control
    ("t" . git-timemachine))
  :hook
  (git-timemachine-mode . fk/tree-sitter-hl-mode))

(use-package forge
  :after magit
  :config
  (advice-add 'magit-pull-from-upstream :after
              (lambda (&rest _) (call-interactively 'forge-pull)))
  (advice-add 'magit-fetch-all :after
              (lambda (&rest _) (call-interactively 'forge-pull)))

  (defun fk/forge-create-pullreq--read-args ()
    (let* ((source  (magit-completing-read
                     "Source branch"
                     (magit-list-remote-branch-names)
                     ;; `magit-get-current-branch' as initial input
                     nil t (concat "origin/" (magit-get-current-branch)) 'magit-revision-history
                     (or (and-let* ((d (magit-branch-at-point)))
                           (if (magit-remote-branch-p d)
                               d
                             (magit-get-push-branch d t)))
                         (and-let* ((d (magit-get-current-branch)))
                           (if (magit-remote-branch-p d)
                               d
                             (magit-get-push-branch d t))))))
           (repo    (forge-get-repository t))
           (remote  (oref repo remote))
           (targets (delete source (magit-list-remote-branch-names remote)))
           (target  (magit-completing-read
                     ;; TODO: show history at first
                     "Target branch" targets nil t nil 'magit-revision-history
                     (let* ((d (cdr (magit-split-branch-name source)))
                            (d (and (magit-branch-p d) d))
                            (d (and d (magit-get-upstream-branch d)))
                            (d (and d (if (magit-remote-branch-p d)
                                          d
                                        (magit-get-upstream-branch d))))
                            (d (or d (concat remote "/"
                                             (or (oref repo default-branch)
                                                 "master")))))
                       (car (member d targets))))))
      (list source target)))

  (defun fk/forge-prepare-topic (source target)
    "Prepare topic for `forge-create-pullreq'."
    (if-let* ((target-name (upcase (string-remove-prefix "origin/" target)))
              (source-name (string-remove-prefix "origin/" source))
              (match (string-match "^[a-z]+-[0-9]+" source-name))
              (match-string (match-string-no-properties 0 source-name))
              (issue-id (when match (upcase match-string)))
              ;; Branch may not have issue-id
              (issue-id-string (if issue-id (format "%s | " issue-id) ""))
              (topic (string-remove-prefix (concat match-string "-") source-name))
              (capitalized-topic (upcase-initials (string-replace "-" " " topic))))
        (format "# %s | %s%s" target-name issue-id-string capitalized-topic)
      (format "# %s | " target-name )))  ; TODO: Get the last commit or at least branch name

  (defun fk/forge--prepare-post-buffer (filename &optional header source target)
    (let ((file (magit-git-dir
                 (convert-standard-filename
                  (concat "magit/posts/" filename)))))
      (make-directory (file-name-directory file) t)
      (let ((prevbuf (current-buffer))
            (resume (and (file-exists-p file)
                         (> (file-attribute-size (file-attributes file)) 0)))
            (buf (find-file-noselect file)))
        (with-current-buffer buf
          (forge-post-mode)
          (when header
            (magit-set-header-line-format header))
          (setq forge--pre-post-buffer prevbuf)
          (when resume
            (forge--display-post-buffer buf)
            (when (magit-read-char-case "A draft already exists.  " nil
                    (?r "[r]esume editing existing draft")
                    (?d "[d]iscard draft and start over" t))
              (erase-buffer)
              (setq resume nil)))
          (when (and (not resume) (string-prefix-p "new" filename))
            (let-alist (forge--topic-template
                        (forge-get-repository t)
                        (if source 'forge-pullreq 'forge-issue))
              (cond
               (.url
                (browse-url .url)
                (forge-post-cancel)
                (setq buf nil)
                (message "Using browser to visit %s instead of opening an issue"
                         .url))
               (.name
                ;; A Github issue with yaml frontmatter.
                (save-excursion (insert .text))
                (unless (re-search-forward "^title: " nil t)
                  (when (re-search-forward "^---" nil t 2)
                    (beginning-of-line)
                    (insert "title: \n")
                    (backward-char))))
               (t
                ;; Custom part:
                (insert (fk/forge-prepare-topic source target)))))))
        buf)))

  ;; I just added `magit-get-current-branch' as initial input
  (advice-add 'forge-create-pullreq--read-args :override 'fk/forge-create-pullreq--read-args)
  ;; I just added a custom dynamic topic template: `fk/forge-prepare-topic'
  (advice-add 'forge--prepare-post-buffer :override 'fk/forge--prepare-post-buffer))

;; (use-package diff-hl
;;  :custom
;;  (diff-hl-global-modes '(not org-mode))
;;  (diff-hl-ask-before-revert-hunk nil)
;;  :custom-face
;;  (diff-hl-insert ((t (:background "#224022"))))
;;  (diff-hl-change ((t (:background "#492949" :foreground "mediumpurple1"))))
;;  (diff-hl-delete ((t (:background "#492929" :foreground "orangered2"))))
;;  :bind
;;  (("M-n" . diff-hl-next-hunk)
;;   ("M-p" . diff-hl-previous-hunk)
;;   :map version-control
;;   ("n" . diff-hl-next-hunk)
;;   ("p" . diff-hl-previous-hunk)
;;   ("r" . diff-hl-revert-hunk))
;;  :hook
;;  (dashboard-after-initialize . global-diff-hl-mode)
;;  (diff-hl-mode . diff-hl-flydiff-mode)
;;  (magit-pre-refresh . diff-hl-magit-pre-refresh)
;;  (magit-post-refresh . diff-hl-magit-post-refresh))


(use-package vc-msg
  :commands vc-msg-show
  :bind
  ( :map version-control
    ("b" . vc-msg-show)))

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :custom
  (restclient-log-request nil)
  ;;:config
  ;;(setcdr (assoc "application/json" restclient-content-type-modes) 'json-mode)
)

(use-package slack
  :commands slack-start
  :custom
  (slack-buffer-function 'switch-to-buffer)
  (slack-buffer-emojify t)
  (slack-prefer-current-team t)
  (slack-alert-icon (fk/expand-static-file-name "slack/icon.png"))
  :custom-face
  (slack-preview-face ((t (:inherit (fixed-pitch shadow org-block) :extend nil))))
  :hook
  (slack-message-buffer-mode . (lambda () (setq-local truncate-lines nil)))
  (slack-message-buffer-mode . (lambda () (setq-local olivetti-body-width 80)))
  :config
  (slack-register-team
   :name "hipo"
   :default t
   :token (auth-source-pick-first-password :host "slack")
   :full-and-display-names t)

  (defun fk/alert-with-sound (orig-func &rest args)
    "Play sound with alert."
    (apply orig-func args)
    (when (eq (plist-get (cdr args) :category) 'slack)
      (let* ((sound-file (fk/expand-static-file-name "slack/sound.mp3"))
             (command (concat "ffplay -volume 20 -nodisp -nostats -hide_banner " sound-file)))
        (when (file-exists-p sound-file)
          (fk/async-process command)))))

  (advice-add 'alert :around 'fk/alert-with-sound))

(use-package emojify
  :commands emojify-mode)

(use-package alert
  :commands alert
  :custom
  (alert-default-style 'libnotify))

(use-package docker
  :commands docker)

(use-package markdown-mode
  :mode "\\.md\\'"
  :custom (markdown-header-scaling t)
  :bind
  ( :map markdown-mode-map
    ("M-n" . markdown-next-visible-heading)
    ("M-p" . markdown-previous-visible-heading)
    ("C-M-j" . markdown-follow-thing-at-point))
  :hook
  (markdown-mode . emojify-mode))

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package docker-compose-mode
  :mode "docker-compose\\'")

(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :hook
  (yaml-mode . highlight-indent-guides-mode)
  (yaml-mode . display-line-numbers-mode))

(use-package pip-requirements
  :mode (("\\.pip\\'" . pip-requirements-mode)
         ("requirements[^z-a]*\\.txt\\'" . pip-requirements-mode)
         ("requirements\\.in" . pip-requirements-mode))
  :config
  ;; Assign a non nil value to `pip-packages' to prevent fetching pip packages.
  (setq pip-packages '("ipython")))

(use-package git-modes
  :mode (("/.gitignore\\'" . gitignore-mode)
         ("/.dockerignore\\'" . gitignore-mode)))

(use-package terraform-mode
  :mode "\\.tf\\'"
  :hook
  (terraform-mode . lsp))

(use-package rubik
  :commands rubik)

(use-package tree-sitter
  :ensure t)

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(use-package nerd-icons
  :ensure t
  :custom
  (nerd-icons-font-family "FiraCode Nerd Font Mono"))

(use-package d2-mode
  :ensure t)

(use-package ledger-mode
  :ensure t)

(use-package rust-mode
  :ensure t
  :hook (rust-mode . lsp))

(use-package plantuml-mode
  :ensure t)

(provide 'init-packages)
