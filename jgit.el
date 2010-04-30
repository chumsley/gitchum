(defface git-chapter-header
    '((((class color) (background dark))
       (:foreground "yellow"))
      (((class color) (background light))
       (:foreground "black" :background "yellow" :bold t))
      (t (:bold t)))
  "Face used to highlight stage headers"
  :group 'git)

    (define-key map [?s] 'git-status)
    (define-key map [?\r] 'git-toggle-expanded)
;    (define-key map [(control ?c) (control ?r)] 'git-revert)
    (define-key map [?r] 'git-refine-hunk)
    (define-key map [?R] 'git-refine-all-hunks)
    (define-key map [?y] 'git-include-hunk)
    (define-key map [?n] 'git-exclude-hunk)
    (define-key map [?s] 'git-exclude-remaining-in-file)
    (define-key map [?f] 'git-include-remaining-in-file)
    (define-key map [?Y] 'git-include-all-hunks)
    (define-key map [?N] 'git-exclude-all-hunks)
    (define-key map [?d] 'git-exclude-remaining)
    (define-key map [?\ ] 'git-toggle-hunk-included)
           (setq delcheated t
                 newfile nil)
           (kill-this-line))

          ((looking-at "new file mode \\([0-7]+\\)")
           (setq latest-permissions (match-string 1))
           (setq newfile t
                 delcheated nil)
(defvar git-chapter-header-re "^\t:\\(Staged\\|Unstaged\\|Untracked\\):\n"
  "Regular expression for post-formatting status 'chapter' headers")

(defvar git-file-header-re "^[ld-][r-][w-][xs-][r-][w-][xs-][r-][w-][xt-] \\([^:]+\\):?  ?\\([^\r\n]*\\)[\r\n]"
(defun git-chapter-header-p ()
  "Non-nil if point is currently on a chapter header line"
  (save-excursion
    (goto-char (point-at-bol))
    (looking-at git-chapter-header-re)))

      (if (re-search-backward git-file-header-re nil t)
                (not (git-file-header-p))
                (not (git-chapter-header-p))
                (not (looking-at "^$"))))
    (when (= (point) (point-at-bol))
      (forward-line -1))
    (point-at-eol)))
                (not (git-hunk-header-p))
                (not (looking-at "^[^\\ +-]"))
                (not (looking-at "^$"))))
    (when (= (point) (point-at-bol))
      (forward-line -1))
    (point-at-eol)))
(defun git-expand-file ()
  "Expand the current file"
  (let ((inhibit-read-only t))
    (subst-char-in-region (git-file-beginning) (git-file-end) ?\^M ?\n)))

(defun git-collapse-file ()
  "Collapse the current file"
  (let ((inhibit-read-only t))
    (subst-char-in-region (git-file-beginning) (git-file-end) ?\n ?\^M)))

(defun git-file-expanded-p ()
  "Return non-NIL if the current file is expanded"
  (save-excursion
    (goto-char (git-file-beginning))
    (re-search-forward "\n" (git-file-end) t)))

(defun git-toggle-expanded ()
  (cond
    ((and (git-file-header-p)
          (git-file-expanded-p))
     (git-collapse-file))
    ((git-file-header-p)
     (git-expand-file))
    ((git-hunk-expanded-p)
     (git-collapse-hunk))
    (t (git-expand-hunk))))
  (unless (git-hunk-beginning)
    (error "No current hunk"))
  (git-command-window 'status same-window)
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (git-markup-status)
    (git-status-mode)
    (git-on-all-hunks 'git-collapse-file)
(defun git-current-chapter ()
  "Returns one of '(staged unstaged untracked) representing the current chapter for point."
  (save-excursion
    (if (git-chapter-header-p)
      (goto-char (point-at-bol))
      (or (re-search-backward git-chapter-header-re nil t)
          (error "No current chapter")))
    (looking-at git-chapter-header-re)
    (let ((h (match-string 1)))
      (cond
        ((string= h "Staged") 'staged)
        ((string= h "Unstaged") 'unstaged)
        ((string= h "Untracked") 'untracked)))))

(defun git-toggle-file-staged ()
  "If the file is currently staged, unstage it.  Otherwise, stage it."
  (interactive)
  (let ((chapter (git-current-chapter))
        (filename (or (git-current-filename) (error "Cannot find current filename"))))
    (ecase chapter
      (staged
       (message "git reset HEAD %s" filename)
       (message (shell-command-to-string (format "git reset HEAD %s" filename))))
      ((unstaged untracked)
       (message "git add %s" filename)
       (message (shell-command-to-string (format "git add %s" filename)))))
    (revert-buffer t t t)))

(defvar git-status-font-lock-keywords
  (append `((,git-chapter-header-re (0 'git-chapter-header)))
          git-whatsnew-font-lock-keywords))

(defvar git-status-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map git-hunk-display-map)
    (define-key map [?\ ] 'git-toggle-file-staged)
    map))

(defun git-status-mode ()
  (unless (eq major-mode 'git-status)
    (kill-all-local-variables))
  (setq font-lock-defaults '((git-status-font-lock-keywords) t))
  (setq major-mode 'git-status)
  (setq mode-name "git-status")
  (use-local-map git-status-map)
  (set (make-local-variable 'revert-buffer-function)
       (lambda (ignore-auto noconfirm)
         (git-status t)))
  (setq selective-display t)
  (font-lock-fontify-buffer))

(defun git-markup-status ()
  "Converts status output to something usable"
  (let ((lines-left 0)
        (branch nil)
        (chapter nil)
        (e (make-marker)))
    (flet ((kill-this-line ()
             (delete-region (point-at-bol)
                            (save-excursion (setq lines-left (forward-line 1)) (point)))))    
      (while (zerop lines-left)
        (setq lines-left nil)
        (cond
          ;; Look for the branch-description line
          ((looking-at "# On branch \\(.*\\)")
           (setq branch (match-string 1))
           (kill-this-line))

          ;; Watch for changes in chapter
          ((looking-at "# Changes to be committed:")
           (setq chapter 'staged)
           (kill-this-line)
           (insert "\n\t:Staged:\n\n"))
          ((looking-at "# Changed but not updated:")
           (setq chapter 'unstaged)
           (kill-this-line)
           (insert "\n\t:Unstaged:\n\n"))
          ((looking-at "# Untracked files:")
           (setq chapter 'untracked)
           (kill-this-line)
           (insert "\n\t:Untracked:\n\n"))
          
          ;; Add actual files
          ((looking-at "#\t\\(new file:\\|deleted:\\|modified:\\)?[ ]+\\(.*\\)$")
           (let ((filename (match-string 2))
                 (s nil))
             (kill-this-line)
             (setq s (point))
             (ecase chapter
               (staged (message "git diff --cached %s" filename)
                       (call-process "git" nil (current-buffer) nil "diff" "--cached" filename))
               (unstaged (message "git diff %s" filename)
                       (call-process "git" nil (current-buffer) nil "diff" filename)))
             (set-marker e (point))
             (save-restriction
               (narrow-to-region s e)
               (goto-char (point-min))
               (git-markup-hunks))
             (goto-char e)
             (message nil)))

          ((and (eq chapter 'untracked)
                (looking-at "#\t\\(.*\\)$"))
           (let ((filename (match-string 1))
                 (s nil))
             (kill-this-line)
             (setq s (point))
             (message "git diff %s" filename)
             (call-process "git" nil (current-buffer) nil "diff" "/dev/null" filename)
             (set-marker e (point))
             (save-restriction
               (narrow-to-region s e)
               (goto-char (point-min))
               (git-markup-hunks))
             (goto-char e)
             (message nil)))
          
          ;; Kill anything we don't recognize
          (t

           (kill-this-line)))
        (unless lines-left
          (setq lines-left (forward-line 1)))))))
               

           
  (git-command-window 'commit same-window)
  (insert (format (substitute-command-keys git-commit-buffer-instructions)
                  (git-current-branch)))
  (git-hunks '("diff" "--cached") nil
             (lambda (str)
               (save-excursion (insert str))
               (git-markup-hunks))))