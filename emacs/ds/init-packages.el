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
  :bind
  (:map evil-normal-state-map
	("-" . dired-jump)
	("C-l" . redraw-display)
	("C-c C-g" . evil-show-file-info)
	("C-c C-u" . evil-delete-back-to-indentation)))
  ;; (define-key evil-normal-state-map (kbd "-") 'dired-jump)
  ;; (define-key evil-normal-state-map (kbd "C-l") 'redraw-display)
  ;; (define-key evil-insert-state-map (kbd "C-c C-u") 'evil-delete-back-to-indentation))

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
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-indent-info nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-vcs-max-length 20)
  (doom-modeline-env-version nil)
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
  (global-company-mode t))

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

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
)

(use-package consult-web
	:straight (consult-web :type git :host github :repo "armindarvish/consult-web" :files (:defaults "sources/*.el"))
        :after consult)

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult-gh
  :straight (consult-gh :type git :host github :repo "armindarvish/consult-gh")
  :after consult)

;; Enable vertico
(use-package vertico
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package eat
  :straight
  (:type git
	 :host codeberg
	 :repo "akib/emacs-eat"
	 :files ("*.el" ("term" "term/*.el") "*.texi"
		 "*.ti" ("terminfo/e" "terminfo/e/*")
		 ("terminfo/65" "terminfo/65/*")
		 ("integration" "integration/*")
		 (:exclude ".dir-locals.el" "*-tests.el"))))

(require 'project)
(setq project-switch-commands '((project-find-file "Find file" "f")
				(project-find-dir "Find dir" "d")
				(project-dired "Dired" "D")
				(consult-ripgrep "ripgrep" "g")
				(magit-project-status "Magit" "m")))
(define-key project-prefix-map (kbd "e") 'eat-project)

(provide 'init-packages)
