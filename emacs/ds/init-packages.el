(use-package no-littering
  :demand t
  :config
  (setq backup-directory-alist `(("." . "~/.emacs-saves")))
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq backup-by-copying t))

(use-package indent-bars
  :hook (prog-mode . indent-bars-mode)
  :init
  (setq
   indent-bars-prefer-character t
   indent-bars-color '(highlight :face-bg t :blend 0.2)
   indent-bars-pattern "."
   indent-bars-color-by-depth nil
   indent-bars-highlight-current-depth '(:color "yellow" :blend 0.4 :width 0.2)
   indent-bars-pad-frac 0.3
   indent-bars-ts-highlight-current-depth '(no-inherit) ; equivalent to nil
   indent-bars-ts-color-by-depth '(no-inherit)
   indent-bars-ts-color '(inherit fringe :face-bg t :blend 0.2))
  :custom
  (indent-bars-no-descend-lists t) ; no extra bars in continued func arg lists
  (indent-bars-treesit-support t))

(use-package direnv
  :config
  (direnv-mode))

(use-package esup)

(use-package hl-todo
  :init
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#E5C07B")
          ("FIXME"  . "#E06C75")
          ("DEBUG"  . "#C678DD")
          ("REFACTOR"  . "#C678DD")
          ("GOTCHA" . "#FF4500")
          ("NOTE"   . "#98C379")
          ("QUESTION"   . "#98C379")
          ("STUB"   . "#61AFEF")))
  :config
  (global-hl-todo-mode 1))

(use-package scroll-on-jump
  :custom
  (scroll-on-jump-smooth t)
  (scroll-on-jump-duration 0.1337)
  :config
  (scroll-on-jump-advice-add beginning-of-buffer)
  (scroll-on-jump-advice-add end-of-buffer)
  (scroll-on-jump-advice-add flyspell-goto-next-error)
  (when (featurep 'smartparens)
    (define-key smartparens-mode-map
		(kbd "C-M-f") (scroll-on-jump-interactive 'sp-forward-sexp))
    (define-key smartparens-mode-map
		(kbd "C-M-b") (scroll-on-jump-interactive 'sp-backward-sexp)))
  ;; (scroll-on-jump-with-scroll-advice-add scroll-up-command)
  (scroll-on-jump-with-scroll-advice-add View-scroll-half-page-backward)
  (scroll-on-jump-with-scroll-advice-add View-scroll-half-page-backward)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-down)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-up)
  ;; (scroll-on-jump-with-scroll-advice-add ccm-scroll-down)
  ;; (scroll-on-jump-with-scroll-advice-add ccm-scroll-up)
  (scroll-on-jump-with-scroll-advice-add isearch-update)
  (scroll-on-jump-with-scroll-advice-add recenter-top-bottom))

(use-package display-fill-column-indicator
  :defer t
  :config
  (setq display-fill-column-indicator-column 80))

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "OPENAI_API_KEY" "ANTHROPIC_API_KEY"))
  :config
  (exec-path-from-shell-initialize))

(use-package calendar
  :commands calendar
  :custom
  (calendar-week-start-day 1)  ; start at Monday
  :hook
  (calendar-mode . delete-other-windows))

(use-package which-key
  :config
  (which-key-mode))

(use-package keycast
  :hook (after-init . keycast-mode)
  :config
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line (fix for use with doom-modeline)."
    :global t
    (if keycast-mode
	(add-hook 'pre-command-hook 'keycast--update t)
      (remove-hook 'pre-command-hook 'keycast--update)))

  (add-to-list 'global-mode-string '("" keycast-mode-line)))

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

(use-package evil-textobj-tree-sitter
  :straight (:type git
		   :host github
		   :repo "meain/evil-textobj-tree-sitter"
		   :files (:defaults "queries" "treesit-queries")))

(use-package hideshow
  :hook
  (prog-mode . hs-minor-mode))

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
  (:map evil-insert-state-map
	("C-c C-s" . yas-expand)
	:map yas-minor-mode-map
	("TAB" . nil)
	("<tab>" . nil)
	:map yas-keymap
	("TAB" . (lambda () (interactive) (yas-next-field)))
	("<tab>" . (lambda () (interactive) (yas-next-field))))
  :hook
  (dashboard-after-initialize . yas-global-mode)
  (snippet-mode . (lambda () (setq-local require-final-newline nil))))

(use-package yasnippet-snippets)

;; Enable Corfu completion UI
;; See the Corfu README for more configuration tips.
(use-package corfu
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode 1)
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-cycle nil)
  (corfu-preselect t)
  (corfu-quit-no-match 'separator)
  (corfu-quit-at-boundary 'separator)
  (corfu-preview-current 'insert)
  :bind
  (:map corfu-popupinfo-map
	("M-d" . corfu-popupinfo-documentation)
	("M-l" . corfu-popupinfo-location)))

(use-package corfu-terminal
  :after corfu
  :straight (:type git :host codeberg :repo "akib/emacs-corfu-terminal")
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.8)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Add extensions
(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind
  ( :map evil-insert-state-map
    ("C-c p" . cape-prefix-map))
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-abbrev)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-dict)
  ;; (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-emoji)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-line)
  (add-hook 'completion-at-point-functions #'cape-rfc1345)
  (add-hook 'completion-at-point-functions #'cape-history))

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

(use-package flycheck
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled))
  :bind
  ( :map errors
    ("n" . flycheck-next-error)
    ("p" . flycheck-previous-error)
    ("l" . flycheck-list-errors)
    ("v" . flycheck-verify-setup)))

(use-package flycheck-posframe
  :after flycheck
  :config (flycheck-posframe-configure-pretty-defaults)
  :hook (flycheck-mode . flycheck-posframe-mode))

;; (use-package eldoc-box
;;   :commands (eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
;;   :custom
;;   (eldoc-box-clear-with-C-g t))

(use-package symbol-overlay
  :commands (symbol-overlay-mode symbol-overlay-put fk/highlight-occurrences)
  :bind
  ( :map symbol-overlay-mode-map
    ("C-c s f" . symbol-overlay-jump-first)
    ("C-c s n" . symbol-overlay-jump-next)
    ("C-c s p" . symbol-overlay-jump-prev)
    ("C-c s l" . symbol-overlay-jump-last))
  :hook
  (emacs-lisp-mode . symbol-overlay-mode)
  (python-mode . symbol-overlay-mode)
  (java-mode . symbol-overlay-mode)
  :config
  (defun fk/get-selected-text ()
    "Return selected text if region is active, else nil."
    (when (region-active-p)
      (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
	(deactivate-mark) text)))
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

(use-package colorful-mode
  :straight (:type git :host github :repo "DevelopmentCool2449/colorful-mode"))

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
  (("\\.html\\'"       . web-mode)
   ("\\.phtml\\'"      . web-mode)
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

(use-package transient
  :init
  (setq transient-show-popup 2))

(use-package magit
  :commands magit
  :init
  (setq magit-define-global-key-bindings 'recommended
	magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1
	magit-commit-diff-inhibit-same-window t)
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

(use-package magit-delta
  :hook (magit-mode . magit-delta-mode))

(use-package magit-todos
  :commands magit-todos-list
  :custom
  (magit-todos-exclude-globs '("*jquery*.js" "*min.js" "*min.css" "*.mjml"))
  (magit-todos-max-items 40)  ; It's actually 20 but needed to set x2 for some reason
  :bind*
  ( :map version-control
    ("T" . magit-todos-list))
  :hook (magit-mode . magit-todos-mode))

;; Using straight.el
(use-package magit-prime
  :straight (:type git :host github :repo "Azkae/magit-prime")
  :config
  (add-hook 'magit-pre-refresh-hook 'magit-prime-refresh-cache))

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
  (:map version-control
	("t" . git-timemachine)))

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

(use-package vc-msg
  :commands vc-msg-show
  :bind
  ( :map version-control
    ("b" . vc-msg-show)))

(use-package verb
  :after org
  :general
  (:states 'normal :keymaps 'org-mode-map "C-c C-r" verb-command-map))

(use-package emojify
  :commands emojify-mode)

(use-package alert
  :commands alert
  :custom
  (alert-default-style 'libnotify))

(use-package treesit-auto
  :custom
  (treesit-auto-install t)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package combobulate
  :after treesit
  :straight (:host github :repo "mickeynp/combobulate")
  :preface
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  (setq combobulate-key-prefix "C-c o")
  :hook
  ((python-ts-mode . combobulate-mode)
   (js-ts-mode . combobulate-mode)
   (html-ts-mode . combobulate-mode)
   (css-ts-mode . combobulate-mode)
   (yaml-ts-mode . combobulate-mode)
   (typescript-ts-mode . combobulate-mode)
   (json-ts-mode . combobulate-mode)
   (tsx-ts-mode . combobulate-mode)
   (java-ts-mode . combobulate-mode)
   (rust-mode . combobulate-mode)))

(use-package nerd-icons
  :custom
  (nerd-icons-font-family "FiraCode Nerd Font Mono"))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

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

(use-package wgrep
  :bind ( :map grep-mode-map
          ("e" . wgrep-change-to-wgrep-mode)
          ("C-x C-q" . wgrep-change-to-wgrep-mode)
          ("C-c C-c" . wgrep-finish-edit)))

(use-package consult-gh
  :straight (:type git :host github :repo "armindarvish/consult-gh")
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
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
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
  :custom
  (completion-styles '(basic orderless partial-completion flex))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vterm
  :bind (:map project-prefix-map
              ("t" . project-vterm))
  :preface
  (defun project-vterm ()
    (interactive)
    (defvar vterm-buffer-name)
    (let* ((default-directory (project-root     (project-current t)))
           (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
           (vterm-buffer (get-buffer vterm-buffer-name)))
      (if (and vterm-buffer (not current-prefix-arg))
          (pop-to-buffer vterm-buffer  (bound-and-true-p display-comint-buffer-action))
        (vterm))))
  :init
  (add-to-list 'project-switch-commands     '(project-vterm "Vterm") t)
  (add-to-list 'project-kill-buffer-conditions  '(major-mode . vterm-mode))
  :config
  (setq vterm-copy-exclude-prompt t
        vterm-max-scrollback 5000))

(use-package multi-vterm
  :hook
  (vterm-mode-hook . (lambda ()
		       (setq-local evil-insert-state-cursor 'box)
		       (evil-insert-state)))
  :config
  (evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-vterm)
  (evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-vterm-next)
  (evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-vterm-prev)
  (evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume)
  :bind
  (:map vterm-mode-map
	("RET" . vterm-send-return)))

(use-package ansi-colorful
  :straight (:host github :repo "jcs-elpa/ansi-colorful")
  :hook
  (compilation-filter . ansi-colorful-mode)
  (sh-mode . ansi-colorful-mode)
  (logview-mode . ansi-colorful-mode))

(use-package devdocs
  :bind
  ("C-h D" . devdocs-lookup))

(use-package auctex
  :hook
  (LaTeX-mode . (lambda ()
		  (interactive)
		  (turn-on-prettify-symbols-mode)
		  (turn-on-flyspell))))

(use-package pdf-tools)

(use-package define-word)

(use-package ement
  :straight (:type git :host github :repo "alphapapa/ement.el"))

(use-package emms)

(use-package elfeed
  :init
  (setq elfeed-feeds
	'(("https://dev.to/feed" programming)
	  ("https://hnrss.org/frontpage" hackernews)
	  ("https://hackaday.com/blog/feed/" hackday)
	  ("https://opensource.com/feed" opensource)
	  ("https://www.techrepublic.com/rss-feeds/topic/open-source" techrepublic opensource)
	  ("https://blog.codinghorror.com/rss/" codinghorror)
	  ("https://martinfowler.com/feed.atom" martin fowler)
	  ("https://feeds.hanselman.com/ScottHanselman" scotthanselman)
	  ("https://medium.com/netflix-techblog?source=rss" netflix)
	  ("https://www.reddit.com/r/linux.rss" reddit linux)
	  ("https://www.reddit.com/r/vim.rss" reddit vim)
	  ("https://www.reddit.com/r/neovim.rss" reddit neovim)
	  ("https://karthinks.com/software/index.xml" karthink emacs)
	  ("https://www.reddit.com/r/emacs.rss" reddit emacs)
	  ("https://rss.arxiv.org/rss/cs" arxiv research))))

(use-package ace-window
  :bind
  ("M-o" . ace-window)
  ("M-C-o" . ace-delete-window))

(use-package zoom-window
  :config
  (setq zoom-window-mode-line-color "DarkGreen")
  :general (:states 'normal "C-w z" 'zoom-window-zoom))

(use-package easy-hugo
  :init
  (setq easy-hugo-basedir "~/src/dhruvasagar/website/"
	easy-hugo-postdir "content/posts/"
        easy-hugo-url "https://dhruvasagar.dev"
        easy-hugo-sshdomain "blog"
        easy-hugo-root "/home/dhruvasagar/website/"
        easy-hugo-previewtime "300")
  :config
  (easy-hugo-enable-menu)
  :bind
  ("C-c M-h" . easy-hugo-menu))

(use-package ledger-mode
  :mode
  (("\\.ledger\\'" . ledger-mode)
   ("\\.dat\\'" . ledger-mode))
  :interpreter "ledger"
  :custom-face (ledger-font-payee-uncleared-face ((t (:inherit error :underline t))))
  :bind (:map ledger-mode-map
	      ("C-c r" . ledger-report)
	      )
  :init
  (setenv "LEDGER_FILE" (expand-file-name (concat org-directory "/comptes.ledger")))
  (setq ledger-mode-should-check-version nil
	ledger-report-links-in-register nil
	ledger-binary-path "ledger")

  (setq ledger-use-iso-dates t)
  (setq ledger-highlight-xact-under-point t)
  (setq ledger-post-account-alignment-column 2)
  (setq ledger-post-amount-alignment-column  52)
  (setq ledger-post-auto-adjust-amounts t)
  (setq ledger-reports
        '(("bal" "%(binary) -y '%a %e %b %Y' -f %(ledger-file) cleared")
          ("net worth" "ledger -f %(ledger-file) -V balance assets liabilities --real")
          ("month budget" "ledger -y '%a %e %b %Y' -f %(ledger-file) --budget -M -p \"this month\" register Expenses Liabilities Assets:PEL Assets:LivretA")
          ("month unbudgeted" "ledger -y '%a %e %b %Y' -f %(ledger-file) --unbudgeted -M -p \"this month\"  register Expenses Liabilities Income")
          ("by Payee" "ledger -y '%a %e %b %Y' -f %(ledger-file) register -P")
          ("uncleared" "ledger -y '%a %e %b %Y' -f %(ledger-file) register -U")
          ("monthly expenses"
           "%(binary) -f %(ledger-file) balance expenses --average --monthly")
          ;; ("monthly expenses" "ledger -y '%b %Y' -f %(ledger-file) -MAn reg Expenses")
          ("month expenses weekly" "ledger -y '%a %e %b %Y' -f %(ledger-file) -Wn reg Expenses -p \"this month\"")
          ("month expenses daily" "ledger -y '%a %e %b %Y' -f %(ledger-file) -Dn reg Expenses -p \"this month\""))))

(use-package age
  :custom
  (age-default-identity "~/.ssh/age/key.txt")
  :config
  (age-file-enable))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package p-search :straight (:host github :repo "zkry/p-search"))

(use-package nov)

(use-package prescient)
(use-package corfu-prescient
  :after (prescient corfu)
  :config
  (corfu-prescient-mode 1)
  (prescient-persist-mode 1))
(use-package vertico-prescient
  :after (prescient vertico)
  :config
  (vertico-prescient-mode 1)
  (prescient-persist-mode 1))

(use-package quickrun)

(use-package persistent-scratch
  :init
  :defer 1
  :bind ("C-c n s" . persistent-scratch-quick-open)
  :config
  (eval-after-load '+popup
    '(set-popup-rule! "\\^*scratch:" :vslot -4 :autosave t :size 0.35 :select t :quit nil :ttl nil :modeline t))
  (setq persistent-scratch-save-file (concat CACHE-DIR ".persistent-scratch"))
  ;; (persistent-scratch-restore)
  ;; (persistent-scratch-setup-default)
  (persistent-scratch-autosave-mode)
  (defun persistent-scratch-buffer-identifier()
    (string-match "^*scratch:" (buffer-name)))
  (defun persistent-scratch-get-scratches()
    (let ((scratch-buffers)
          (save-data
           (read
            (with-temp-buffer
              (let ((coding-system-for-read 'utf-8-unix))
                (insert-file-contents persistent-scratch-save-file))
              (buffer-string)))))
      (dolist (saved-buffer save-data)
        (push (substring (aref saved-buffer 0) (length "*scratch:")) scratch-buffers))
      scratch-buffers))

  (defun persistent-scratch-restore-this(&optional file)
    (interactive)
    (let ((current-buf (buffer-name (current-buffer)))
          (save-data
           (read
            (with-temp-buffer
              (let ((coding-system-for-read 'utf-8-unix))
                (insert-file-contents (or file persistent-scratch-save-file)))
              (buffer-string)))))
      (dolist (saved-buffer save-data)
        (when (string= current-buf (aref saved-buffer 0))
          (with-current-buffer (get-buffer-create (aref saved-buffer 0))
            (erase-buffer)
            (insert (aref saved-buffer 1))
            (funcall (or (aref saved-buffer 3) #'ignore))
            (let ((point-and-mark (aref saved-buffer 2)))
              (when point-and-mark
                (goto-char (car point-and-mark))
                (set-mark (cdr point-and-mark))))
            (let ((narrowing (aref saved-buffer 4)))
              (when narrowing
                (narrow-to-region (car narrowing) (cdr narrowing))))
            ;; Handle version 2 fields if present.
            (when (>= (length saved-buffer) 6)
              (unless (aref saved-buffer 5)
                (deactivate-mark))))))))

  (defun persistent-scratch-quick-open()
    (interactive)
    (let* ((scratch-buffers (persistent-scratch-get-scratches))
           (chosen-scratch (concat "*scratch:"
                                   (completing-read
                                    "Choose a scratch: "
                                    scratch-buffers nil nil nil nil
                                    (random-alnum 4))))
           (buffer-exists-p (get-buffer chosen-scratch)))
      (pop-to-buffer chosen-scratch)
      (unless buffer-exists-p
        (persistent-scratch-restore-this))
      (persistent-scratch-mode)))
  (setq persistent-scratch-scratch-buffer-p-function 'persistent-scratch-buffer-identifier))

(use-package popper
  :bind (("C-\\"   . popper-toggle)
         ("M-\\"   . popper-cycle)
         ("C-M-\\" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
	  "\\*pytest*\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package dirvish
  :init
  (dirvish-override-dired-mode))

(use-package multi-line
  :bind
  (:map evil-normal-state-map
	("gS" . multi-line)
	("gJ" . multi-line-single-line)))

(use-package rfc-mode)

(use-package diff-hl
  :after magit
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package gcmh-mode
  :straight (:type git :host github :repo "emacsmirror/gcmh")
  :config
  (gcmh-mode 1))

;; (use-package flyover
;;   :straight (:type git :host github :repo "konrad1977/flyover")
;;   :init
;;   (setq flyover-use-theme-colors t)
;;   :hook (flycheck-mode . flyover-mode))

(use-package jira
  :straight (:host github :repo "unmonoqueteclea/jira.el")
  :config
  (setq jira-base-url "https://aicrete.atlassian.net")
  :bind
  (:map jira-issues-mode-map
	("C-c j" . jira-issues-menu)
	("C-c ?" . jira-issues-actions-menu)
	("C-c f" . jira-detail-find-issue-by-key)
	("C-c c" . (lambda ()
		     (interactive)
		     (jira-actions-copy-issues-id-to-clipboard (jira-utils-marked-item))))
	("C-c C" . jira-actions-change-issue-menu)
	("C-c I" . (lambda ()
		     (interactive)
		     (jira-details-show-issue (jira-utils-marked-item))))
	("C-c O" . (lambda ()
		     (interactive)
		     (jira-actions-open-issue (jira-utils-marked-item))))
	("C-c W" . jira-actions-add-worklog-menu)))

(require 'project)
(setq project-switch-commands '((project-find-file "Find file" "f")
				(project-find-dir "Find dir" "d")
				(project-dired "Dired" "D")
				(consult-ripgrep "ripgrep" "g")
				(magit-project-status "Magit" "m")
				(consult-todo-project "Find TODO" "t")))

(use-package project-x
  :after project
  :straight (:type git :host github :repo "karthink/project-x")
  :config
  (add-hook 'project-find-functions 'project-x-try-local 90)
  (add-hook 'kill-emacs-hook 'project-x--window-state-write)
  (setq project-x-save-interval 600     ;Save project state every 10 min
	project-x-local-identifier '("package.json" "mix.es" "cargo.toml" ".project" ".git"))
  (project-x-mode 1))

(provide 'init-packages)
