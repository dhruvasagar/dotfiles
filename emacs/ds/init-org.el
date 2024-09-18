(use-package org
             :ensure t
             :config
             (setq org-return-follows-link t)
             (setq org-directory "~/Dropbox/Documents/org-files")
             (setq org-default-notes-file "~/Dropbox/Documents/org-files/refile.org")
             (setq org-agenda-files (quote ("~/Dropbox/Documents/org-files")))
             (setq org-log-done (quote time))
             (setq org-log-into-drawer t)
             (setq org-log-state-notes-insert-after-drawers nil)
             (setq org-archive-mark-done nil)
             (setq org-archive-location "%s_archive::* Archived Tasks")
             (setq org-todo-keywords
                   (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                           (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))
             (setq org-todo-keyword-faces
                   (quote (("TODO" :foreground "red" :weight bold)
                           ("NEXT" :foreground "blue" :weight bold)
                           ("DONE" :foreground "forest green" :weight bold)
                           ("WAITING" :foreground "orange" :weight bold)
                           ("HOLD" :foreground "magenta" :weight bold)
                           ("CANCELLED" :foreground "forest green" :weight bold)
                           ("MEETING" :foreground "forest green" :weight bold)
                           ("PHONE" :foreground "forest green" :weight bold))))
             (setq org-use-fast-todo-selection t)
             (setq org-treat-S-cursor-todo-selection-as-state-change nil)
             (setq org-todo-state-tags-triggers
                   (quote (("CANCELLED" ("CANCELLED" . t))
                           ("WAITING" ("WAITING" . t))
                           ("HOLD" ("WAITING") ("HOLD" . t))
                           (done ("WAITING") ("HOLD"))
                           ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                           ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                           ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))
             (setq org-capture-templates
                   (quote (("t" "todo" entry (file "~/Dropbox/Documents/org-files/refile.org")
                            "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
                           ("r" "respond" entry (file "~/Dropbox/Documents/org-files/refile.org")
                            "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
                           ("n" "note" entry (file "~/Dropbox/Documents/org-files/refile.org")
                            "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                           ("j" "Journal" entry (file+datetree "~/Dropbox/Documents/org-files/diary.org")
                            "* %?\n%U\n" :clock-in t :clock-resume t)
                           ("w" "org-protocol" entry (file "~/Dropbox/Documents/org-files/refile.org")
                            "* TODO Review %c\n%U\n" :immediate-finish t)
                           ("m" "Meeting" entry (file "~/Dropbox/Documents/org-files/refile.org")
                            "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                           ("p" "Phone call" entry (file "~/Dropbox/Documents/org-files/refile.org")
                            "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
                           ("h" "Habit" entry (file "~/Dropbox/Documents/org-files/refile.org")
                            "* NEXT %?\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n%U\n%a\n"))))
             ; Targets include this file and any file contributing to the agenda - up to 9 levels deep
             (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                              (org-agenda-files :maxlevel . 9))))
             ; Use full outline paths for refile targets - we file directly with IDO
             (setq org-refile-use-outline-path t)
             ; Targets complete directly with IDO
             (setq org-outline-path-complete-in-steps nil)

             ; Allow refile to create parent tasks with confirmation
             (setq org-refile-allow-creating-parent-nodes (quote confirm))

             ; Use IDO for both buffer and file completion and ido-everywhere to t
             (setq org-completion-use-ido t)
             (setq ido-max-directory-size 100000)
             (ido-mode (quote both))
             ; Use the current window when visiting files and buffers with ido
             (setq ido-default-file-method 'selected-window)
             (setq ido-default-buffer-method 'selected-window)
             ; Use the current window for indirect buffer display
             (setq org-indirect-buffer-display 'current-window)

             ;;;; Refile settings
             ; Exclude DONE state tasks from refile targets
             (defun bh/verify-refile-target ()
               "Exclude todo keywords with a done state from refile targets"
               (not (member (nth 2 (org-heading-components)) org-done-keywords)))

             (setq org-refile-target-verify-function 'bh/verify-refile-target)
             ;; Do not dim blocked tasks
             (setq org-agenda-dim-blocked-tasks nil)

             ;; Compact the block agenda view
             (setq org-agenda-compact-blocks t)

             ;; Custom agenda command definitions
             (setq org-agenda-custom-commands
                   (quote (("N" "Notes" tags "NOTE"
                            ((org-agenda-overriding-header "Notes")
                             (org-tags-match-list-sublevels t)))
                           ("h" "Habits" tags-todo "STYLE=\"habit\""
                            ((org-agenda-overriding-header "Habits")
                             (org-agenda-sorting-strategy
                               '(todo-state-down effort-up category-keep))))
                           (" " "Agenda"
                            ((agenda "" nil)
                             (tags "REFILE"
                                   ((org-agenda-overriding-header "Tasks to Refile")
                                    (org-tags-match-list-sublevels nil)))
                             (tags-todo "-CANCELLED/!"
                                        ((org-agenda-overriding-header "Stuck Projects")
                                         (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                                         (org-agenda-sorting-strategy
                                           '(category-keep))))
                             (tags-todo "-HOLD-CANCELLED/!"
                                        ((org-agenda-overriding-header "Projects")
                                         (org-agenda-skip-function 'bh/skip-non-projects)
                                         (org-tags-match-list-sublevels 'indented)
                                         (org-agenda-sorting-strategy
                                           '(category-keep))))
                             (tags-todo "-CANCELLED/!NEXT"
                                        ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                               (if bh/hide-scheduled-and-waiting-next-tasks
                                                                                 ""
                                                                                 " (including WAITING and SCHEDULED tasks)")))
                                         (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                                         (org-tags-match-list-sublevels t)
                                         (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                                         (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                                         (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                                         (org-agenda-sorting-strategy
                                           '(todo-state-down effort-up category-keep))))
                             (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                                        ((org-agenda-overriding-header (concat "Project Subtasks"
                                                                               (if bh/hide-scheduled-and-waiting-next-tasks
                                                                                 ""
                                                                                 " (including WAITING and SCHEDULED tasks)")))
                                         (org-agenda-skip-function 'bh/skip-non-project-tasks)
                                         (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                                         (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                                         (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                                         (org-agenda-sorting-strategy
                                           '(category-keep))))
                             (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                                        ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                                               (if bh/hide-scheduled-and-waiting-next-tasks
                                                                                 ""
                                                                                 " (including WAITING and SCHEDULED tasks)")))
                                         (org-agenda-skip-function 'bh/skip-project-tasks)
                                         (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                                         (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                                         (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                                         (org-agenda-sorting-strategy
                                           '(category-keep))))
                             (tags-todo "-CANCELLED+WAITING|HOLD/!"
                                        ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                                               (if bh/hide-scheduled-and-waiting-next-tasks
                                                                                 ""
                                                                                 " (including WAITING and SCHEDULED tasks)")))
                                         (org-agenda-skip-function 'bh/skip-non-tasks)
                                         (org-tags-match-list-sublevels nil)
                                         (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                                         (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
                             (tags "-REFILE/"
                                   ((org-agenda-overriding-header "Tasks to Archive")
                                    (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                                    (org-tags-match-list-sublevels nil))))
                            nil))))

             (defun bh/org-auto-exclude-function (tag)
               "Automatic task exclusion in the agenda with / RET"
               (and (cond
                      ((string= tag "hold")
                       t)
                      ((string= tag "farm")
                       t))
                    (concat "-" tag)))
             (setq org-agenda-auto-exclude-function 'bh/org-auto-exclude-function)

             ;; Resume clocking task when emacs is restarted
             (org-clock-persistence-insinuate)
             ;;
             ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
             (setq org-clock-history-length 23)
             ;; Resume clocking task on clock-in if the clock is open
             (setq org-clock-in-resume t)
             ;; Change tasks to NEXT when clocking in
             (setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
             ;; Separate drawers for clocking and logs
             (setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
             ;; Save clock data and state changes and notes in the LOGBOOK drawer
             (setq org-clock-into-drawer t)
             ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
             (setq org-clock-out-remove-zero-time-clocks t)
             ;; Agenda clock report parameters
             (setq org-agenda-clockreport-parameter-plist
                   (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))
             ; Set default column view headings: Task Effort Clock_Summary
             (setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
             ; global Effort estimate values
             ; global STYLE property values for completion
             (setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                                 ("STYLE_ALL" . "habit"))))
             ;; Agenda log mode items to display (closed and state changes by default)
             (setq org-agenda-log-mode-items (quote (closed state)))

             ; Tags with fast selection keys
             (setq org-tag-alist (quote ((:startgroup)
                                         ("@errand" . ?e)
                                         ("@office" . ?o)
                                         ("@home" . ?H)
                                         ("@farm" . ?f)
                                         (:endgroup)
                                         ("WAITING" . ?w)
                                         ("HOLD" . ?h)
                                         ("PERSONAL" . ?P)
                                         ("WORK" . ?W)
                                         ("FARM" . ?F)
                                         ("ORG" . ?O)
                                         ("NORANG" . ?N)
                                         ("crypt" . ?E)
                                         ("NOTE" . ?n)
                                         ("CANCELLED" . ?c)
                                         ("FLAGGED" . ??))))

             ; Allow setting single tags without the menu
             (setq org-fast-tag-selection-single-key (quote expert))
             ; For tag searches ignore tasks with scheduled and deadline dates
             (setq org-agenda-tags-todo-honor-ignore-options t)

             ;; Clock out when moving task to a done state
             (setq org-clock-out-when-done t)
             ;; Save the running clock and all clock history when exiting Emacs, load it on startup
             (setq org-clock-persist t)
             ;; Do not prompt to resume an active clock
             (setq org-clock-persist-query-resume nil)
             ;; Enable auto clock resolution for finding open clocks
             (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
             ;; Include current clocking task in clock reports
             (setq org-clock-report-include-clocking-task t)

             (setq bh/keep-clock-running nil)

             (defun bh/is-project-p ()
               "Any task with a todo keyword subtask"
               (save-restriction
                 (widen)
                 (let ((has-subtask)
                       (subtree-end (save-excursion (org-end-of-subtree t)))
                       (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
                   (save-excursion
                     (forward-line 1)
                     (while (and (not has-subtask)
                                 (< (point) subtree-end)
                                 (re-search-forward "^\*+ " subtree-end t))
                            (when (member (org-get-todo-state) org-todo-keywords-1)
                              (setq has-subtask t))))
                   (and is-a-task has-subtask))))

             (defun bh/is-project-subtree-p ()
               "Any task with a todo keyword that is in a project subtree.
               Callers of this function already widen the buffer view."
               (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                                           (point))))
                 (save-excursion
                   (bh/find-project-task)
                   (if (equal (point) task)
                     nil
                     t))))

             (defun bh/is-task-p ()
               "Any task with a todo keyword and no subtask"
               (save-restriction
                 (widen)
                 (let ((has-subtask)
                       (subtree-end (save-excursion (org-end-of-subtree t)))
                       (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
                   (save-excursion
                     (forward-line 1)
                     (while (and (not has-subtask)
                                 (< (point) subtree-end)
                                 (re-search-forward "^\*+ " subtree-end t))
                            (when (member (org-get-todo-state) org-todo-keywords-1)
                              (setq has-subtask t))))
                   (and is-a-task (not has-subtask)))))

             (defun bh/is-subproject-p ()
               "Any task which is a subtask of another project"
               (let ((is-subproject)
                     (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
                 (save-excursion
                   (while (and (not is-subproject) (org-up-heading-safe))
                          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
                            (setq is-subproject t))))
                 (and is-a-task is-subproject)))

             (defun bh/list-sublevels-for-projects-indented ()
               "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
               This is normally used by skipping functions where this variable is already local to the agenda."
               (if (marker-buffer org-agenda-restrict-begin)
                 (setq org-tags-match-list-sublevels 'indented)
                 (setq org-tags-match-list-sublevels nil))
               nil)

             (defun bh/list-sublevels-for-projects ()
               "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
               This is normally used by skipping functions where this variable is already local to the agenda."
               (if (marker-buffer org-agenda-restrict-begin)
                 (setq org-tags-match-list-sublevels t)
                 (setq org-tags-match-list-sublevels nil))
               nil)

             (defvar bh/hide-scheduled-and-waiting-next-tasks t)

             (defun bh/toggle-next-task-display ()
               (interactive)
               (setq bh/hide-scheduled-and-waiting-next-tasks (not bh/hide-scheduled-and-waiting-next-tasks))
               (when  (equal major-mode 'org-agenda-mode)
                 (org-agenda-redo))
               (message "%s WAITING and SCHEDULED NEXT Tasks" (if bh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

             (defun bh/skip-stuck-projects ()
               "Skip trees that are not stuck projects"
               (save-restriction
                 (widen)
                 (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
                   (if (bh/is-project-p)
                     (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                            (has-next ))
                       (save-excursion
                         (forward-line 1)
                         (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                                (unless (member "WAITING" (org-get-tags-at))
                                  (setq has-next t))))
                       (if has-next
                         nil
                         next-headline)) ; a stuck project, has subtasks but no next task
                     nil))))

             (defun bh/skip-non-stuck-projects ()
               "Skip trees that are not stuck projects"
               ;; (bh/list-sublevels-for-projects-indented)
               (save-restriction
                 (widen)
                 (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
                   (if (bh/is-project-p)
                     (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                            (has-next ))
                       (save-excursion
                         (forward-line 1)
                         (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                                (unless (member "WAITING" (org-get-tags-at))
                                  (setq has-next t))))
                       (if has-next
                         next-headline
                         nil)) ; a stuck project, has subtasks but no next task
                     next-headline))))

             (defun bh/skip-non-projects ()
               "Skip trees that are not projects"
               ;; (bh/list-sublevels-for-projects-indented)
               (if (save-excursion (bh/skip-non-stuck-projects))
                 (save-restriction
                   (widen)
                   (let ((subtree-end (save-excursion (org-end-of-subtree t))))
                     (cond
                       ((bh/is-project-p)
                        nil)
                       ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
                        nil)
                       (t
                         subtree-end))))
                 (save-excursion (org-end-of-subtree t))))

             (defun bh/skip-non-tasks ()
               "Show non-project tasks.
               Skip project and sub-project tasks, habits, and project related tasks."
               (save-restriction
                 (widen)
                 (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
                   (cond
                     ((bh/is-task-p)
                      nil)
                     (t
                       next-headline)))))

             (defun bh/skip-project-trees-and-habits ()
               "Skip trees that are projects"
               (save-restriction
                 (widen)
                 (let ((subtree-end (save-excursion (org-end-of-subtree t))))
                   (cond
                     ((bh/is-project-p)
                      subtree-end)
                     ((org-is-habit-p)
                      subtree-end)
                     (t
                       nil)))))

             (defun bh/skip-projects-and-habits-and-single-tasks ()
               "Skip trees that are projects, tasks that are habits, single non-project tasks"
               (save-restriction
                 (widen)
                 (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
                   (cond
                     ((org-is-habit-p)
                      next-headline)
                     ((and bh/hide-scheduled-and-waiting-next-tasks
                           (member "WAITING" (org-get-tags-at)))
                      next-headline)
                     ((bh/is-project-p)
                      next-headline)
                     ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
                      next-headline)
                     (t
                       nil)))))

             (defun bh/skip-project-tasks-maybe ()
               "Show tasks related to the current restriction.
               When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
               When not restricted, skip project and sub-project tasks, habits, and project related tasks."
               (save-restriction
                 (widen)
                 (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                        (next-headline (save-excursion (or (outline-next-heading) (point-max))))
                        (limit-to-project (marker-buffer org-agenda-restrict-begin)))
                   (cond
                     ((bh/is-project-p)
                      next-headline)
                     ((org-is-habit-p)
                      subtree-end)
                     ((and (not limit-to-project)
                           (bh/is-project-subtree-p))
                      subtree-end)
                     ((and limit-to-project
                           (bh/is-project-subtree-p)
                           (member (org-get-todo-state) (list "NEXT")))
                      subtree-end)
                     (t
                       nil)))))

             (defun bh/skip-project-tasks ()
               "Show non-project tasks.
               Skip project and sub-project tasks, habits, and project related tasks."
               (save-restriction
                 (widen)
                 (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
                   (cond
                     ((bh/is-project-p)
                      subtree-end)
                     ((org-is-habit-p)
                      subtree-end)
                     ((bh/is-project-subtree-p)
                      subtree-end)
                     (t
                       nil)))))

             (defun bh/skip-non-project-tasks ()
               "Show project tasks.
               Skip project and sub-project tasks, habits, and loose non-project tasks."
               (save-restriction
                 (widen)
                 (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                        (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
                   (cond
                     ((bh/is-project-p)
                      next-headline)
                     ((org-is-habit-p)
                      subtree-end)
                     ((and (bh/is-project-subtree-p)
                           (member (org-get-todo-state) (list "NEXT")))
                      subtree-end)
                     ((not (bh/is-project-subtree-p))
                      subtree-end)
                     (t
                       nil)))))

             (defun bh/skip-projects-and-habits ()
               "Skip trees that are projects and tasks that are habits"
               (save-restriction
                 (widen)
                 (let ((subtree-end (save-excursion (org-end-of-subtree t))))
                   (cond
                     ((bh/is-project-p)
                      subtree-end)
                     ((org-is-habit-p)
                      subtree-end)
                     (t
                       nil)))))

             (defun bh/skip-non-subprojects ()
               "Skip trees that are not projects"
               (let ((next-headline (save-excursion (outline-next-heading))))
                 (if (bh/is-subproject-p)
                   nil
                   next-headline)))

             (defun bh/clock-in-to-next (kw)
               "Switch a task from TODO to NEXT when clocking in.
               Skips capture tasks, projects, and subprojects.
               Switch projects and subprojects from NEXT back to TODO"
               (when (not (and (boundp 'org-capture-mode) org-capture-mode))
                 (cond
                   ((and (member (org-get-todo-state) (list "TODO"))
                         (bh/is-task-p))
                    "NEXT")
                   ((and (member (org-get-todo-state) (list "NEXT"))
                         (bh/is-project-p))
                    "TODO"))))

             (defun bh/find-project-task ()
               "Move point to the parent (project) task if any"
               (save-restriction
                 (widen)
                 (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
                   (while (org-up-heading-safe)
                          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
                            (setq parent-task (point))))
                   (goto-char parent-task)
                   parent-task)))

             (defun bh/punch-in (arg)
               "Start continuous clocking and set the default task to the
               selected task.  If no task is selected set the Organization task
               as the default task."
               (interactive "p")
               (setq bh/keep-clock-running t)
               (if (equal major-mode 'org-agenda-mode)
                 ;;
                 ;; We're in the agenda
                 ;;
                 (let* ((marker (org-get-at-bol 'org-hd-marker))
                        (tags (org-with-point-at marker (org-get-tags-at))))
                   (if (and (eq arg 4) tags)
                     (org-agenda-clock-in '(16))
                     (bh/clock-in-organization-task-as-default)))
                 ;;
                 ;; We are not in the agenda
                 ;;
                 (save-restriction
                   (widen)
                   ; Find the tags on the current task
                   (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
                     (org-clock-in '(16))
                     (bh/clock-in-organization-task-as-default)))))

             (defun bh/punch-out ()
               (interactive)
               (setq bh/keep-clock-running nil)
               (when (org-clock-is-active)
                 (org-clock-out))
               (org-agenda-remove-restriction-lock))

             (defun bh/clock-in-default-task ()
               (save-excursion
                 (org-with-point-at org-clock-default-task
                                    (org-clock-in))))

             (defun bh/clock-in-parent-task ()
               "Move point to the parent (project) task if any and clock in"
               (let ((parent-task))
                 (save-excursion
                   (save-restriction
                     (widen)
                     (while (and (not parent-task) (org-up-heading-safe))
                            (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
                              (setq parent-task (point))))
                     (if parent-task
                       (org-with-point-at parent-task
                                          (org-clock-in))
                       (when bh/keep-clock-running
                         (bh/clock-in-default-task)))))))

             (defvar bh/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")

             (defun bh/clock-in-organization-task-as-default ()
               (interactive)
               (org-with-point-at (org-id-find bh/organization-task-id 'marker)
                                  (org-clock-in '(16))))

             (defun bh/clock-out-maybe ()
               (when (and bh/keep-clock-running
                          (not org-clock-clocking-in)
                          (marker-buffer org-clock-default-task)
                          (not org-clock-resolving-clocks-due-to-idleness))
                 (bh/clock-in-parent-task)))

             (add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)
             ;; Show all future entries for repeating tasks
             (setq org-agenda-repeating-timestamp-show-all t)

             ;; Show all agenda dates - even if they are empty
             (setq org-agenda-show-all-dates t)

             ;; Use sticky agenda's so they persist
             (setq org-agenda-sticky t)

             (setq org-show-following-heading t)
             (setq org-show-hierarchy-above t)
             (setq org-show-siblings (quote ((default))))
             (setq org-special-ctrl-a/e t)
             (setq org-special-ctrl-k t)
             (setq org-yank-adjusted-subtrees t)
             (setq org-id-method (quote uuidgen))

             ;; Sorting order for tasks on the agenda
             (setq org-agenda-sorting-strategy
                   (quote ((agenda habit-down time-up user-defined-up effort-up category-keep)
                           (todo category-up effort-up)
                           (tags category-up effort-up)
                           (search category-up))))

             ;; Start the weekly agenda on Monday
             (setq org-agenda-start-on-weekday 1)

             ;; Enable display of the time grid so we can see the marker for the current time
             (setq org-agenda-time-grid (quote ((daily today remove-match)
                                                (0900 1100 1300 1500 1700)
                                                "......" "----------------")))

             ;; Display tags farther right
             (setq org-agenda-tags-column -102)

             ;;
             ;; Agenda sorting functions
             ;;
             (setq org-agenda-cmp-user-defined 'bh/agenda-sort)

             (defun bh/agenda-sort (a b)
               "Sorting strategy for agenda items.
               Late deadlines first, then scheduled, then non-late deadlines"
               (let (result num-a num-b)
                 (cond
                   ; time specific items are already sorted first by org-agenda-sorting-strategy

                   ; non-deadline and non-scheduled items next
                   ((bh/agenda-sort-test 'bh/is-not-scheduled-or-deadline a b))

                   ; deadlines for today next
                   ((bh/agenda-sort-test 'bh/is-due-deadline a b))

                   ; late deadlines next
                   ((bh/agenda-sort-test-num 'bh/is-late-deadline '> a b))

                   ; scheduled items for today next
                   ((bh/agenda-sort-test 'bh/is-scheduled-today a b))

                   ; late scheduled items next
                   ((bh/agenda-sort-test-num 'bh/is-scheduled-late '> a b))

                   ; pending deadlines last
                   ((bh/agenda-sort-test-num 'bh/is-pending-deadline '< a b))

                   ; finally default to unsorted
                   (t (setq result nil)))
                 result))

             (defmacro bh/agenda-sort-test (fn a b)
               "Test for agenda sort"
               `(cond
                  ; if both match leave them unsorted
                  ((and (apply ,fn (list ,a))
                        (apply ,fn (list ,b)))
                   (setq result nil))
                  ; if a matches put a first
                  ((apply ,fn (list ,a))
                   (setq result -1))
                  ; otherwise if b matches put b first
                  ((apply ,fn (list ,b))
                   (setq result 1))
                  ; if none match leave them unsorted
                  (t nil)))

             (defmacro bh/agenda-sort-test-num (fn compfn a b)
               `(cond
                  ((apply ,fn (list ,a))
                   (setq num-a (string-to-number (match-string 1 ,a)))
                   (if (apply ,fn (list ,b))
                     (progn
                       (setq num-b (string-to-number (match-string 1 ,b)))
                       (setq result (if (apply ,compfn (list num-a num-b))
                                      -1
                                      1)))
                     (setq result -1)))
                  ((apply ,fn (list ,b))
                   (setq result 1))
                  (t nil)))

             (defun bh/is-not-scheduled-or-deadline (date-str)
               (and (not (bh/is-deadline date-str))
                    (not (bh/is-scheduled date-str))))

             (defun bh/is-due-deadline (date-str)
               (string-match "Deadline:" date-str))

             (defun bh/is-late-deadline (date-str)
               (string-match "\\([0-9]*\\) d\. ago:" date-str))

             (defun bh/is-pending-deadline (date-str)
               (string-match "In \\([^-]*\\)d\.:" date-str))

             (defun bh/is-deadline (date-str)
               (or (bh/is-due-deadline date-str)
                   (bh/is-late-deadline date-str)
                   (bh/is-pending-deadline date-str)))

             (defun bh/is-scheduled (date-str)
               (or (bh/is-scheduled-today date-str)
                   (bh/is-scheduled-late date-str)))

             (defun bh/is-scheduled-today (date-str)
               (string-match "Scheduled:" date-str))

             (defun bh/is-scheduled-late (date-str)
               (string-match "Sched\.\\(.*\\)x:" date-str))

             (setq org-deadline-warning-days 30)

             ; Enable habit tracking (and a bunch of other modules)
             (setq org-modules (quote (org-bbdb
                                        org-bibtex
                                        org-crypt
                                        org-gnus
                                        org-id
                                        org-info
                                        org-jsinfo
                                        org-habit
                                        org-inlinetask
                                        org-irc
                                        org-mew
                                        org-mhe
                                        org-protocol
                                        org-rmail
                                        org-vm
                                        org-wl
                                        org-w3m)))
             ; position the habit graph on the agenda to the right of the default
             (setq org-habit-graph-column 50)

             (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
             (defun bh/remove-empty-drawer-on-clock-out ()
               (interactive)
               (save-excursion
                 (beginning-of-line 0)
                 (org-remove-empty-drawer-at "LOGBOOK" (point))))
             (add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

             (setq org-ditaa-jar-path
                   (format "%s/libexec/ditaa-0.11.0-standalone.jar"
                           (string-trim-right (shell-command-to-string "brew --prefix ditaa"))))
             (setq org-plantuml-jar-path (format "%s/libexec/plantuml.jar" (string-trim-right (shell-command-to-string "brew --prefix plantuml"))))

             (add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)

             ; Make babel results blocks lowercase
             (setq org-babel-results-keyword "results")

             (defun bh/display-inline-images ()
               (condition-case nil
                               (org-display-inline-images)
                               (error nil)))

             (org-babel-do-load-languages
               (quote org-babel-load-languages)
               (quote ((emacs-lisp . t)
                       (dot . t)
                       (ditaa . t)
                       (R . t)
                       (python . t)
                       (ruby . t)
                       (gnuplot . t)
                       (clojure . t)
                       (shell . t)
                       (org . t)
                       (plantuml . t)
                       (latex . t))))

             ; Do not prompt to confirm evaluation
             ; This may be dangerous - make sure you understand the consequences
             ; of setting this -- see the docstring for details
             (setq org-confirm-babel-evaluate nil)

             ; Use fundamental mode when editing plantuml blocks with C-c '
             (add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))

             ; Erase all reminders and rebuilt reminders for today from the agenda
             (defun bh/org-agenda-to-appt ()
               (interactive)
               (setq appt-time-msg-list nil)
               (org-agenda-to-appt))

             ; Rebuild the reminders everytime the agenda is displayed
             (add-hook 'org-agenda-finalize-hook 'bh/org-agenda-to-appt 'append)

             ; This is at the end of my .emacs - so appointments are set up when Emacs starts
             (bh/org-agenda-to-appt)

             ; Activate appointments so we get notifications
             (appt-activate t)

             ; If we leave Emacs running overnight - reset the appointments one minute after midnight
             (run-at-time "24:01" nil 'bh/org-agenda-to-appt)

             ;; Enable abbrev-mode
             (add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))

             ;; Skeletons
             ;;
             ;; sblk - Generic block #+begin_FOO .. #+end_FOO
             (define-skeleton skel-org-block
                              "Insert an org block, querying for type."
                              "Type: "
                              "#+begin_" str "\n"
                              _ - \n
                              "#+end_" str "\n")

             (define-abbrev org-mode-abbrev-table "sblk" "" 'skel-org-block)

             ;; splantuml - PlantUML Source block
             (define-skeleton skel-org-block-plantuml
                              "Insert a org plantuml block, querying for filename."
                              "File (no extension): "
                              "#+begin_src plantuml :file " str ".png :cache yes\n"
                              _ - \n
                              "#+end_src\n")

             (define-abbrev org-mode-abbrev-table "splantuml" "" 'skel-org-block-plantuml)

             (define-skeleton skel-org-block-plantuml-activity
                              "Insert a org plantuml block, querying for filename."
                              "File (no extension): "
                              "#+begin_src plantuml :file " str "-act.png :cache yes :tangle " str "-act.txt\n"
                              (bh/plantuml-reset-counters)
                              "@startuml\n"
                              "skinparam activity {\n"
                              "BackgroundColor<<New>> Cyan\n"
                              "}\n\n"
                              "title " str " - \n"
                              "note left: " str "\n"
                              "(*) --> \"" str "\"\n"
                              "--> (*)\n"
                              _ - \n
                              "@enduml\n"
                              "#+end_src\n")

             (defvar bh/plantuml-if-count 0)

             (defun bh/plantuml-if ()
               (incf bh/plantuml-if-count)
               (number-to-string bh/plantuml-if-count))

             (defvar bh/plantuml-loop-count 0)

             (defun bh/plantuml-loop ()
               (incf bh/plantuml-loop-count)
               (number-to-string bh/plantuml-loop-count))

             (defun bh/plantuml-reset-counters ()
               (setq bh/plantuml-if-count 0
                     bh/plantuml-loop-count 0)
               "")

             (define-abbrev org-mode-abbrev-table "sact" "" 'skel-org-block-plantuml-activity)

             (define-skeleton skel-org-block-plantuml-activity-if
                              "Insert a org plantuml block activity if statement"
                              ""
                              "if \"\" then\n"
                              "  -> [condition] ==IF" (setq ifn (bh/plantuml-if)) "==\n"
                              "  --> ==IF" ifn "M1==\n"
                              "  -left-> ==IF" ifn "M2==\n"
                              "else\n"
                              "end if\n"
                              "--> ==IF" ifn "M2==")

             (define-abbrev org-mode-abbrev-table "sif" "" 'skel-org-block-plantuml-activity-if)

             (define-skeleton skel-org-block-plantuml-activity-for
                              "Insert a org plantuml block activity for statement"
                              "Loop for each: "
                              "--> ==LOOP" (setq loopn (bh/plantuml-loop)) "==\n"
                              "note left: Loop" loopn ": For each " str "\n"
                              "--> ==ENDLOOP" loopn "==\n"
                              "note left: Loop" loopn ": End for each " str "\n" )

             (define-abbrev org-mode-abbrev-table "sfor" "" 'skel-org-block-plantuml-activity-for)

             (define-skeleton skel-org-block-plantuml-sequence
                              "Insert a org plantuml activity diagram block, querying for filename."
                              "File appends (no extension): "
                              "#+begin_src plantuml :file " str "-seq.png :cache yes :tangle " str "-seq.txt\n"
                              "@startuml\n"
                              "title " str " - \n"
                              "actor CSR as \"Customer Service Representative\"\n"
                              "participant CSMO as \"CSM Online\"\n"
                              "participant CSMU as \"CSM Unix\"\n"
                              "participant NRIS\n"
                              "actor Customer"
                              _ - \n
                              "@enduml\n"
                              "#+end_src\n")

             (define-abbrev org-mode-abbrev-table "sseq" "" 'skel-org-block-plantuml-sequence)

             ;; sdot - Graphviz DOT block
             (define-skeleton skel-org-block-dot
                              "Insert a org graphviz dot block, querying for filename."
                              "File (no extension): "
                              "#+begin_src dot :file " str ".png :cache yes :cmdline -Kdot -Tpng\n"
                              "graph G {\n"
                              _ - \n
                              "}\n"
                              "#+end_src\n")

             (define-abbrev org-mode-abbrev-table "sdot" "" 'skel-org-block-dot)

             ;; sditaa - Ditaa source block
             (define-skeleton skel-org-block-ditaa
                              "Insert a org ditaa block, querying for filename."
                              "File (no extension): "
                              "#+begin_src ditaa :file " str ".png :cache yes\n"
                              _ - \n
                              "#+end_src\n")

             (define-abbrev org-mode-abbrev-table "sditaa" "" 'skel-org-block-ditaa)

             ;; selisp - Emacs Lisp source block
             (define-skeleton skel-org-block-elisp
                              "Insert a org emacs-lisp block"
                              ""
                              "#+begin_src emacs-lisp\n"
                              _ - \n
                              "#+end_src\n")

             (define-abbrev org-mode-abbrev-table "selisp" "" 'skel-org-block-elisp)

             (defun bh/org-todo (arg)
               (interactive "p")
               (if (equal arg 4)
                 (save-restriction
                   (bh/narrow-to-org-subtree)
                   (org-show-todo-tree nil))
                 (bh/narrow-to-org-subtree)
                 (org-show-todo-tree nil)))

             (defun bh/widen ()
               (interactive)
               (if (equal major-mode 'org-agenda-mode)
                 (progn
                   (org-agenda-remove-restriction-lock)
                   (when org-agenda-sticky
                     (org-agenda-redo)))
                 (widen)))

             (add-hook 'org-agenda-mode-hook
                       '(lambda () (org-defkey org-agenda-mode-map "W" (lambda () (interactive) (setq bh/hide-scheduled-and-waiting-next-tasks t) (bh/widen))))
                       'append)

             (defun bh/restrict-to-file-or-follow (arg)
               "Set agenda restriction to 'file or with argument invoke follow mode.
               I don't use follow mode very often but I restrict to file all the time
               so change the default 'F' binding in the agenda to allow both"
               (interactive "p")
               (if (equal arg 4)
                 (org-agenda-follow-mode)
                 (widen)
                 (bh/set-agenda-restriction-lock 4)
                 (org-agenda-redo)
                 (beginning-of-buffer)))

             (add-hook 'org-agenda-mode-hook
                       '(lambda () (org-defkey org-agenda-mode-map "F" 'bh/restrict-to-file-or-follow))
                       'append)

             (defun bh/narrow-to-org-subtree ()
               (widen)
               (org-narrow-to-subtree)
               (save-restriction
                 (org-agenda-set-restriction-lock)))

             (defun bh/narrow-to-subtree ()
               (interactive)
               (if (equal major-mode 'org-agenda-mode)
                 (progn
                   (org-with-point-at (org-get-at-bol 'org-hd-marker)
                                      (bh/narrow-to-org-subtree))
                   (when org-agenda-sticky
                     (org-agenda-redo)))
                 (bh/narrow-to-org-subtree)))

             (add-hook 'org-agenda-mode-hook
                       '(lambda () (org-defkey org-agenda-mode-map "N" 'bh/narrow-to-subtree))
                       'append)

             (defun bh/narrow-up-one-org-level ()
               (widen)
               (save-excursion
                 (outline-up-heading 1 'invisible-ok)
                 (bh/narrow-to-org-subtree)))

             (defun bh/get-pom-from-agenda-restriction-or-point ()
               (or (and (marker-position org-agenda-restrict-begin) org-agenda-restrict-begin)
                   (org-get-at-bol 'org-hd-marker)
                   (and (equal major-mode 'org-mode) (point))
                   org-clock-marker))

             (defun bh/narrow-up-one-level ()
               (interactive)
               (if (equal major-mode 'org-agenda-mode)
                 (progn
                   (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
                                      (bh/narrow-up-one-org-level))
                   (org-agenda-redo))
                 (bh/narrow-up-one-org-level)))

             (add-hook 'org-agenda-mode-hook
                       '(lambda () (org-defkey org-agenda-mode-map "U" 'bh/narrow-up-one-level))
                       'append)

             (defun bh/narrow-to-org-project ()
               (widen)
               (save-excursion
                 (bh/find-project-task)
                 (bh/narrow-to-org-subtree)))

             (defun bh/narrow-to-project ()
               (interactive)
               (if (equal major-mode 'org-agenda-mode)
                 (progn
                   (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
                                      (bh/narrow-to-org-project)
                                      (save-excursion
                                        (bh/find-project-task)
                                        (org-agenda-set-restriction-lock)))
                   (org-agenda-redo)
                   (beginning-of-buffer))
                 (bh/narrow-to-org-project)
                 (save-restriction
                   (org-agenda-set-restriction-lock))))

             (add-hook 'org-agenda-mode-hook
                       '(lambda () (org-defkey org-agenda-mode-map "P" 'bh/narrow-to-project))
                       'append)

             (defvar bh/project-list nil)

             (defun bh/view-next-project ()
               (interactive)
               (let (num-project-left current-project)
                 (unless (marker-position org-agenda-restrict-begin)
                   (goto-char (point-min))
                   ; Clear all of the existing markers on the list
                   (while bh/project-list
                          (set-marker (pop bh/project-list) nil))
                   (re-search-forward "Tasks to Refile")
                   (forward-visible-line 1))

                 ; Build a new project marker list
                 (unless bh/project-list
                   (while (< (point) (point-max))
                          (while (and (< (point) (point-max))
                                      (or (not (org-get-at-bol 'org-hd-marker))
                                          (org-with-point-at (org-get-at-bol 'org-hd-marker)
                                                             (or (not (bh/is-project-p))
                                                                 (bh/is-project-subtree-p)))))
                                 (forward-visible-line 1))
                          (when (< (point) (point-max))
                            (add-to-list 'bh/project-list (copy-marker (org-get-at-bol 'org-hd-marker)) 'append))
                          (forward-visible-line 1)))

                 ; Pop off the first marker on the list and display
                 (setq current-project (pop bh/project-list))
                 (when current-project
                   (org-with-point-at current-project
                                      (setq bh/hide-scheduled-and-waiting-next-tasks nil)
                                      (bh/narrow-to-project))
                   ; Remove the marker
                   (setq current-project nil)
                   (org-agenda-redo)
                   (beginning-of-buffer)
                   (setq num-projects-left (length bh/project-list))
                   (if (> num-projects-left 0)
                     (message "%s projects left to view" num-projects-left)
                     (beginning-of-buffer)
                     (setq bh/hide-scheduled-and-waiting-next-tasks t)
                     (error "All projects viewed.")))))

             (add-hook 'org-agenda-mode-hook
                       '(lambda () (org-defkey org-agenda-mode-map "V" 'bh/view-next-project))
                       'append)
             (setq org-use-speed-commands t)
             (setq org-speed-commands-user (quote (("0" . ignore)
                                                   ("1" . ignore)
                                                   ("2" . ignore)
                                                   ("3" . ignore)
                                                   ("4" . ignore)
                                                   ("5" . ignore)
                                                   ("6" . ignore)
                                                   ("7" . ignore)
                                                   ("8" . ignore)
                                                   ("9" . ignore)

                                                   ("a" . ignore)
                                                   ("d" . ignore)
                                                   ("h" . bh/hide-other)
                                                   ("i" progn
                                                    (forward-char 1)
                                                    (call-interactively 'org-insert-heading-respect-content))
                                                   ("k" . org-kill-note-or-show-branches)
                                                   ("l" . ignore)
                                                   ("m" . ignore)
                                                   ("q" . bh/show-org-agenda)
                                                   ("r" . ignore)
                                                   ("s" . org-save-all-org-buffers)
                                                   ("w" . org-refile)
                                                   ("x" . ignore)
                                                   ("y" . ignore)
                                                   ("z" . org-add-note)

                                                   ("A" . ignore)
                                                   ("B" . ignore)
                                                   ("E" . ignore)
                                                   ("F" . bh/restrict-to-file-or-follow)
                                                   ("G" . ignore)
                                                   ("H" . ignore)
                                                   ("J" . org-clock-goto)
                                                   ("K" . ignore)
                                                   ("L" . ignore)
                                                   ("M" . ignore)
                                                   ("N" . bh/narrow-to-org-subtree)
                                                   ("P" . bh/narrow-to-org-project)
                                                   ("Q" . ignore)
                                                   ("R" . ignore)
                                                   ("S" . ignore)
                                                   ("T" . bh/org-todo)
                                                   ("U" . bh/narrow-up-one-org-level)
                                                   ("V" . ignore)
                                                   ("W" . bh/widen)
                                                   ("X" . ignore)
                                                   ("Y" . ignore)
                                                   ("Z" . ignore))))

             (defun bh/show-org-agenda ()
               (interactive)
               (if org-agenda-sticky
                 (switch-to-buffer "*Org Agenda( )*")
                 (switch-to-buffer "*Org Agenda*"))
               (delete-other-windows))

             (setq org-list-demote-modify-bullet (quote (("+" . "-")
                                                         ("*" . "-")
                                                         ("1." . "-")
                                                         ("1)" . "-")
                                                         ("A)" . "-")
                                                         ("B)" . "-")
                                                         ("a)" . "-")
                                                         ("b)" . "-")
                                                         ("A." . "-")
                                                         ("B." . "-")
                                                         ("a." . "-")
                                                         ("b." . "-"))))
             (setq org-tags-match-list-sublevels t)
             (setq org-agenda-persistent-filter t)
             (setq org-agenda-skip-additional-timestamps-same-entry t)
             ; Overwrite the current window with the agenda
             (setq org-agenda-window-setup 'current-window)
             (setq org-clone-delete-id t)
             (setq org-cycle-include-plain-lists t)
             (setq org-src-fontify-natively t)
             (setq org-structure-template-alist
                   (quote (("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")
                           ("e" "#+begin_example\n?\n#+end_example" "<example>\n?\n</example>")
                           ("q" "#+begin_quote\n?\n#+end_quote" "<quote>\n?\n</quote>")
                           ("v" "#+begin_verse\n?\n#+end_verse" "<verse>\n?\n</verse>")
                           ("c" "#+begin_center\n?\n#+end_center" "<center>\n?\n</center>")
                           ("l" "#+begin_latex\n?\n#+end_latex" "<literal style=\"latex\">\n?\n</literal>")
                           ("L" "#+latex: " "<literal style=\"latex\">?</literal>")
                           ("h" "#+begin_html\n?\n#+end_html" "<literal style=\"html\">\n?\n</literal>")
                           ("H" "#+html: " "<literal style=\"html\">?</literal>")
                           ("a" "#+begin_ascii\n?\n#+end_ascii")
                           ("A" "#+ascii: ")
                           ("i" "#+index: ?" "#+index: ?")
                           ("I" "#+include %file ?" "<include file=%file markup=\"?\">"))))

             (defun bh/mark-next-parent-tasks-todo ()
               "Visit each parent task and change NEXT states to TODO"
               (let ((mystate (or (and (fboundp 'org-state)
                                       state)
                                  (nth 2 (org-heading-components)))))
                 (when mystate
                   (save-excursion
                     (while (org-up-heading-safe)
                            (when (member (nth 2 (org-heading-components)) (list "NEXT"))
                              (org-todo "TODO")))))))

             (add-hook 'org-after-todo-state-change-hook 'bh/mark-next-parent-tasks-todo 'append)
             (add-hook 'org-clock-in-hook 'bh/mark-next-parent-tasks-todo 'append)

             (setq org-startup-folded t)
             (setq org-alphabetical-lists t)

             ;; flyspell mode for spell checking everywhere
             (add-hook 'org-mode-hook 'turn-on-flyspell 'append)

             ;; Disable keys in org-mode
             ;;    C-c [
             ;;    C-c ]
             ;;    C-c ;
             ;;    C-c C-x C-q  cancelling the clock (we never want this)
             (add-hook 'org-mode-hook
                       '(lambda ()
                          ;; Undefine C-c [ and C-c ] since this breaks my
                          ;; org-agenda files when directories are include It
                          ;; expands the files in the directories individually
                          (org-defkey org-mode-map "\C-c[" 'undefined)
                          (org-defkey org-mode-map "\C-c]" 'undefined)
                          (org-defkey org-mode-map "\C-c;" 'undefined)
                          (org-defkey org-mode-map "\C-c\C-x\C-q" 'undefined))
                       'append)

             (add-hook 'org-mode-hook
                       (lambda ()
                         (local-set-key (kbd "C-c M-o") 'bh/mail-subtree))
                       'append)

             (defun bh/mail-subtree ()
               (interactive)
               (org-mark-subtree)
               (org-mime-subtree))

             (setq org-export-coding-system 'utf-8)
             (prefer-coding-system 'utf-8)
             (set-charset-priority 'unicode)
             (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

             (setq org-time-clocksum-format
                   '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

             (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

             :bind
             (("C-c l" . org-store-link)
              ("C-c a" . org-agenda)
              ("C-c c" . org-capture)))

(use-package org-bullets
             :hook (org-mode . org-bullets-mode))

(use-package org-roam
             :ensure t
             :custom
             (org-roam-directory "~/Dropbox/Documents/org-files/roam/")
             (org-roam-completion-everywhere t)
             :bind (("C-c n l" . org-roam-buffer-toggle)
                    ("C-c n f" . org-roam-node-find)
                    ("C-c n i" . org-roam-node-insert)
                    :map org
                    ("o" . org-roam-node-find)
                    ("C-M-i" . completion-at-point))
             :config
             (org-roam-setup)
             (org-roam-db-autosync-mode))

(use-package websocket
             :after org-roam)

(use-package org-roam-ui
             :after org-roam ;; or :after org
             ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
             ;;         a hookable mode anymore, you're advised to pick something yourself
             ;;         if you don't care about startup time, use
             ;;  :hook (after-init . org-roam-ui-mode)
             :config
             (setq org-roam-ui-sync-theme t
                   org-roam-ui-follow t
                   org-roam-ui-update-on-save t
                   org-roam-ui-open-on-start t))


(provide 'init-org)
