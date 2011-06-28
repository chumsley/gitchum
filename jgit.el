;;; jgit.el --- xgit-like git integration for emacs

;; Copyright (C) 2010 James Wright

;; Author: James Wright <james@chumsley.org>
;; Created: 28 Apr 2010
;; Keywords: 

;; This file is not yet part of GNU Emacs.

;; jgit.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; jgit.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Still TODO
;; - macro to factor out common navigation code?  (just takes navigation-body and error)
;; - Cherrypicking from commit/status?
;; - empty new files and empty repos are not handled well

;;; Code:

;;;; =============================================== Faces ==============================================

(defface git-header
    '((((class color) (background light))
       (:background "grey85"))
      (t (:bold t)))
  "Common aspects of headers"
  :group 'git)

(defface git-chapter-header
    '((((class color) (background dark))
       (:foreground "yellow"))
      (((class color) (background light))
       (:foreground "black" :background "yellow" :bold t))
      (t (:bold t)))
  "Face used to highlight stage headers"
  :group 'git)

(defface git-file-header
    '((((class color) (background dark))
       (:foreground "yellow"))
      (((class color) (background light))
       (:foreground "black" :background "grey70"))
      (t (:bold t)))
  "Face used to highlight filename headers"
  :group 'git)

(defface git-file-header-filename
    '((((class color) (background dark))
       (:foreground "yellow" :bold t))
      (((class color) (background light))
       (:foreground "black" :bold t :background "grey70"))
      (t (:bold t)))
  "Face used to highlight filename portion of filename headers"
  :group 'git)

(defface git-file-header-mod
    '((((class color))
       (:inherit git-context))
      (t (:bold t)))
  "Face used to highlight 'mod' tag of filename headers"
  :group 'git)

(defface git-file-header-del
    '((((class color))
       (:inherit git-line-removed))
      (t (:bold t)))
  "Face used to highlight 'deleted' tag of filename headers"
  :group 'git)

(defface git-file-header-new
    '((((class color))
       (:inherit git-line-added))
      (t (:bold t)))
  "Face used to highlight 'deleted' tag of filename headers"
  :group 'git)

(defface git-hunk-header
    '((((class color) (background dark))
       (:background "gray90" :foreground "black" :inherit git-header))
      (((class color) (background light))
       (:background "gray90" :foreground "black" :inherit git-header))
      (t (:bold t)))
  "Face used for hunk header lines"
  :group 'git)

(defface git-line-added
    '((((class color) (background dark))
       (:foreground "blue"))
      (((class color) (background light))
       (:foreground "blue"))
      (t (:bold t)))
  "Face used for lines added"
  :group 'git)

(defface git-line-removed
    '((((class color) (background dark))
       (:foreground "red"))
      (((class color) (background light))
       (:foreground "red"))
      (t (:bold t)))
  "Face used for lines removed"
  :group 'git)

(defface git-excluded
    '((((class color))
       (:inherit 'shadow))
      (t (:dim t)))
  "Face used for excluded patches"
  :group 'git)

(defface git-context
    '((((class color)) (:inherit default)))
  "Face used for context lines in a patch"
  :group 'git)

(defface git-verbosity
    '((((class color)) (:inherit shadow)))
  "Face used for various other fluff in a patch display"
  :group 'git)

(defface git-branch
    '((t (:bold t :background "white")))
  "Face used to highlight the branch name"
  :group 'git)

(defface git-commit-msg
    '((t (:inherit default)))
  "Face used for the commit message"
  :group 'git)



;;;; ---------------------------- Other customizable settings ----------------------------

(defcustom git-command-prefix [(control x) ?g]
  "Prefix key sequence for git commands."
  :group 'git)


;;;; ============================================== Keymaps =============================================

;;;; ----------------------------------- global keymap -----------------------------------

(defvar git-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?a] 'git-add)
;    (define-key map [?b] 'git-blame)
;    (define-key map [?l] 'git-log)
;    (define-key map [?=] 'git-diff)
;    (define-key map [?-] 'git-ediff)
;    (define-key map [??] 'git-describe-bindings)
;    (define-key map [?d] 'git-describe-patch)
;    (define-key map [?f] 'git-filelog)
    (define-key map [?G] 'git-pull)
    (define-key map [?S] 'git-push)
    (define-key map [?i] 'git-init)
    (define-key map [?m] 'git-query-manifest)
    (define-key map [?s] 'git-status)
    (define-key map [?w] 'git-whatsnew)
    ;(define-key map [?c] 'git-commit)
    (define-key map [?x] 'git-remove)
    (define-key map [?C] 'git-cvs-commit)
    (define-key map [?U] 'git-cvs-update)
    map)
  "The prefix for git commands")

(if (not (keymapp (lookup-key global-map git-command-prefix)))
  (define-key global-map git-command-prefix git-prefix-map))

(defvar git-hunk-display-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\r] 'git-toggle-expanded)
    (define-key map [(control return)] 'git-find-in-other-window)
    (define-key map [?j] 'git-next-dwim)
    (define-key map [?k] 'git-prev-dwim)
    (define-key map [?J] 'git-next-file)
    (define-key map [?K] 'git-prev-file)
    (define-key map [?a] 'git-expand-all-hunks)
    (define-key map [?z] 'git-collapse-all-hunks)
    (define-key map [?q] 'darcs-quit-current)
    map)
  "Keymap for displaying lists of atomic hunks")

(defvar git-whatsnew-map
  (let ((map (make-sparse-keymap 'git-whatsnew-map)))
    (set-keymap-parent map git-hunk-display-map)
    (define-key map [(control ?c) (control ?s)] 'git-stage-from-whatsnew)
    (define-key map [(control ?c) (control ?c)] 'git-commit-from-whatsnew)
    (define-key map [(control ?c) (control ?r)] 'git-revert)
    (define-key map [(control ?x) ?#] 'git-commit-from-whatsnew)
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
    map)
  "Keymap for git-whatsnew-mode")

;;;; ============================================= Commands =============================================

(defun git-command-window (name same-window)
  "Switch to a readonly git command window with the
  default-directory set to the repo root.  Contents are erased,
  but local variables might be leftover from previous instances."
  (let ((repo-dir (let ((dir (file-name-directory (expand-file-name (or (buffer-file-name (current-buffer)) default-directory))))
                        (olddir "/"))
                    (while (and (not (equal dir olddir))
                                (not (file-directory-p (concat dir "/.git"))))
                      (setq olddir dir
                            dir (file-name-directory (directory-file-name dir))))
                    (and (not (equal dir olddir)) dir))))
    (if same-window
      (switch-to-buffer (format "*git %s: (%s)*" name repo-dir))
      (switch-to-buffer-other-window (format "*git %s: (%s)*" name repo-dir)))
    (toggle-read-only 1)
    (setq default-directory repo-dir)))

;;;; ------------------------------------ git-whatsnew -----------------------------------

(defvar git-display-state nil
  "Note: Distinct from git-responses; git-responses describes the
  state that should be passed to git, whereas git-display-state
  describes the way the hunks should look.")

(defvar git-current-target nil
  "The current target before refresh")

(defun git-whatsnew (&optional same-window state-assoc current-target)
  "Prints a list of all the changes in the current repo"
  (interactive)
  (git-command-window 'whatsnew same-window)
  (set (make-local-variable 'git-current-target) current-target)
  (set (make-local-variable 'git-display-state) state-assoc)
  (git-hunks '("add" "--patch") git-refined-hunks
             (lambda (raw-output)
               (let ((inhibit-read-only t)
                     (cooked (with-temp-buffer
                               (insert raw-output)
                               (goto-char (point-min))
                               (git-markup-hunks)
                               (buffer-substring (point-min) (point-max)))))
                 (erase-buffer)
                 (git-whatsnew-mode)
                 (save-excursion
                   (insert cooked))
                 (git-apply-plists git-display-state t)
                 (when git-current-target
                   (git-goto-target git-current-target))
                 (message nil)))))

(defun git-markup-hunks ()
  "Starting from point and moving down the rest of the buffer,
   convert raw output from `git-hunks' and edit it to the format
   that we present to the user"
  (let ((lines-left 0)
        (latest-index nil)
        (latest-permissions nil)
        (latest-filename nil)
        (delcheated nil)
        (newfile nil))
    (flet ((kill-this-line ()
             (delete-region (point-at-bol)
                            (save-excursion (setq lines-left (forward-line 1)) (point)))))    
      (while (zerop lines-left)
        (setq lines-left nil)
        (cond
          ;; Collapse the four lines of file heading into one
          ((looking-at "index \\([0-9a-f.]+\\) \\([0-7]+\\)")
           (setq latest-index (match-string 1))
           (setq latest-permissions (match-string 2))
           (setq delcheated nil
                 newfile nil)
           (kill-this-line))

          ((looking-at "deleted file mode \\([0-7]+\\)")
           (setq latest-permissions (match-string 1))
           (setq delcheated t
                 newfile nil)
           (kill-this-line))

          ((looking-at "new file mode \\([0-7]+\\)")
           (setq latest-permissions (match-string 1))
           (setq newfile t
                 delcheated nil)
           (kill-this-line))

          ((looking-at "--- \\(?:a/\\([^\r\n\t]*\\)\t?\n\\|/dev/null\\)")
           (let ((filename (match-string 1)))
             (setq latest-filename filename)
             (kill-this-line)))

          ((looking-at "\\+\\+\\+ \\(?:b/\\([^\r\n\t]*\\)\t?\n\\|/dev/null\\)")
           (let ((filename (match-string 1)))
             (setq latest-filename (or filename latest-filename))
             (kill-this-line)
             (insert (translate-permissions latest-permissions) " "
                     (cond
                       (delcheated "deleted:  ")
                       (newfile    "new file: ")
                       (t          "modified: "))
                     latest-filename "\n")))

          ;; Ensure that hunk headers don't have context lines at the end
          ((looking-at "@@ \\([^@]*\\) @@\\([^\r\n]*\\)")
           (unless (zerop (length (match-string 2)))
             (save-excursion
               (goto-char (match-beginning 2))
               (insert "\n"))))

          ;; Some strings pass through safely
          ((looking-at "No changes."))
          
          ;; All unrecognized non-blank lines get killed
          ((or (= (point-at-bol) (point-at-eol))
               (looking-at "[^ +-@\\]"))
           (kill-this-line)))
        (unless lines-left
          (setq lines-left (forward-line 1)))))))

(defun translate-permissions (permstring)
  (if (null permstring)
    "----------"
    (flet ((xlat-perms (pstr sticky sticky-char)
             (let* ((p (string-to-number pstr))
                    (ext (string-to-number (substring permstring 2 3)))
                    (a (if (zerop (logand p 4)) "-" "r"))
                    (b (if (zerop (logand p 2)) "-" "w"))
                    (c (cond
                         ((zerop (logand ext sticky))
                          (if (zerop (logand p 1)) "-" "x"))
                         (t sticky-char))))
               (concat a b c))))
      (concat (case (string-to-number (substring permstring 0 2))
                (10 "-")
                (12 "l")
                (otherwise "?"))
              (xlat-perms (substring permstring 3 4) 4 "s")
              (xlat-perms (substring permstring 4 5) 2 "s")
              (xlat-perms (substring permstring 5 6) 1 "t")))))

(defvar git-chapter-header-re "^\t:\\(Staged\\|Unstaged\\|Untracked\\):\n"
  "Regular expression for post-formatting status 'chapter' headers")

(defvar git-file-header-re "^[ld-][r-][w-][xs-][r-][w-][xs-][r-][w-][xt-] \\([^:]+\\):?  ?\\([^\r\n]*\\)[\r\n]"
  "Regular expression for post-formatting file headers.")

(defvar git-hunk-header-re "\\(^@@ -\\(?:[0-9]+\\)\\(?:,\\(?:[0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\(?:[0-9]+\\)\\)? @@\\).*$"
  "Regular expression for hunk headers.")

(defvar git-whatsnew-font-lock-keywords
  `(
    (,git-file-header-re
     (0 'git-file-header) (2 'git-file-header-filename prepend)
     (1
      (let ((s (match-string 1)))
        (cond ((string= s "modified") 'git-file-header-mod)
              ((string= s "deleted") 'git-file-header-del)
              ((string= s "new file") 'git-file-header-new)))
      prepend))
    
    (,git-hunk-header-re (1 'git-hunk-header))
    ("^[+>].*$"
     (0 'git-line-added))
    ("^[-<].*$"
     (0 'git-line-removed))
    ("^#.*$"
     (0 'font-lock-comment-face))
    ("^\\\\.*$"
     (0 'git-verbosity))
    ))

(defun git-whatsnew-mode ()
  (unless (eq major-mode 'git-whatsnew)
    ;; Don't kill locals if we're already in whatsnew-mode
    (kill-all-local-variables))
  (setq font-lock-defaults '((git-whatsnew-font-lock-keywords) t))
  (setq major-mode 'git-whatsnew)
  (setq mode-name "git-whatsnew")
  (use-local-map git-whatsnew-map)
  (set (make-local-variable 'revert-buffer-function)
       (lambda (ignore-auto noconfirm)
         (let ((target (git-current-target)))
           (git-whatsnew t (git-collect-hunk-plists) target))))
  (setq selective-display t)
  (turn-on-font-lock))

(defun git-chapter-header-p ()
  "Non-nil if point is currently on a chapter header line"
  (save-excursion
    (goto-char (point-at-bol))
    (looking-at git-chapter-header-re)))

(defun git-file-header-p ()
  "Non-nil if point is currently on a file header line"
  (save-excursion
    (goto-char (point-at-bol))
    (looking-at git-file-header-re)))

(defun git-hunk-header-p ()
  "Non-nil if point is currently on a hunk header line"
  (save-excursion
    (goto-char (point-at-bol))
    (looking-at git-hunk-header-re)))

(defun git-chapter-beginning ()
  "Returns the position of the beginning of the current chapter"
    (if (git-chapter-header-p)
      (point-at-bol)
      (save-excursion
        (if (re-search-backward git-chapter-header-re nil t)
          (point)
          (error "No current chapter")))))

(defun git-chapter-end ()
  "Returns the position of the end of the current chapter"
  (save-excursion
    (goto-char (git-chapter-beginning))
    (while (and (zerop (forward-line 1))
                (not (git-chapter-header-p))))
    (when (= (point) (point-at-bol))
      (forward-line -1))
    (point-at-eol)))

(defun git-file-beginning (&optional noerror)
  "Returns the position of the beginning of the current file"
  (if (git-file-header-p)
    (point-at-bol)
    (save-excursion
      (if (re-search-backward git-file-header-re nil t)
        (point)
        (unless noerror (error "No current file"))))))

(defun git-file-end ()
  "Returns the position of the end of the current file"
  (save-excursion
    (goto-char (git-file-beginning))
    (while (and (zerop (forward-line 1))
                (not (git-file-header-p))
                (not (git-chapter-header-p))
                (not (looking-at "^$"))))
    (when (= (point) (point-at-bol))
      (forward-line -1))
    (point-at-eol)))

(defun git-hunk-beginning (&optional noerror)
  "Returns the position of the beginning of the current hunk"
  (cond
    ((git-hunk-header-p)
     (point-at-bol))
    ((git-file-header-p)
     nil)
    (t (save-excursion
         (if (re-search-backward "^@@" nil t)
           (point)
           (unless noerror (error "No current hunk")))))))

(defun git-hunk-end ()
  "Returns the position of the end of the current hunk"
  (save-excursion
    (goto-char (git-hunk-beginning))
    (while (and (zerop (forward-line 1))
                (not (git-file-header-p))
                (not (git-hunk-header-p))
                (not (looking-at "^[^\\ +-]"))
                (not (looking-at "^$"))))
    (when (= (point) (point-at-bol))
      (forward-line -1))
    (point-at-eol)))

(defun git-next-hunk (&optional noerror)
  "Move point to the beginning of the next hunk"
  (interactive)
  (let ((p (save-excursion
             (goto-char (point-at-eol))
             (and (re-search-forward "^@@" nil t)
                  (match-beginning 0)))))
    (if (null p)
      (unless noerror (error "No more patches"))
      (goto-char p)
      (git-maybe-recenter))))

(defun git-next-file-or-hunk (&optional noerror)
  "Move point to the beginning of the next hunk or file header.
  Returns non-NIL only if move was successful (i.e., not at
  eob)."
  (let ((p (save-excursion
             (goto-char (point-at-eol))
             (and (re-search-forward (format "^@@\\|%s" git-file-header-re) nil t)
                  (match-beginning 0)))))
    (if (null p)
      (unless noerror (error "No more patches"))
      (goto-char p)
      (git-maybe-recenter))))

(defun git-prev-hunk (&optional noerror)
  "Move point to the beginning of the previous hunk"
  (interactive)
  (let ((p (save-excursion
             (goto-char (point-at-bol))
             (and (re-search-backward "^@@" nil t)
                  (match-beginning 0)))))
               
    (if (null p)
      (unless noerror (error "No more patches"))
      (goto-char p)
      (git-maybe-recenter))))

(defun git-next-file (&optional noerror)
  "Move point to the beginning of the next file"
  (interactive)
  (let ((p (save-excursion
             (goto-char (point-at-eol))
             (and (re-search-forward git-file-header-re nil t)
                  (match-beginning 0)))))
    (if (null p)
      (unless noerror (error "No more files"))
      (goto-char p)
      (git-maybe-recenter))))

(defun git-prev-file (&optional noerror)
  "Move point to the beginning of the previous file"
  (interactive)
  (let ((p (save-excursion
             (goto-char (point-at-bol))
             (and (re-search-backward git-file-header-re nil t)
                  (match-beginning 0)))))
    (if (null p)
      (unless noerror (error "No more files"))
      (goto-char p)
      (git-maybe-recenter))))

(defun git-next-dwim ()
  "Go to the next hunk if possible, or the next file otherwise."
  (interactive)
  (or (git-next-hunk t)
      (git-next-file t)
      (error "No more files or hunks")))

(defun git-prev-dwim ()
  "Go to the previous hunk if possible. or the previous file otherwise."
  (interactive)
  (or (git-prev-hunk t)
      (git-prev-file t)
      (error "No more files or hunks")))
  
(defun git-expand-file ()
  "Expand the current file"
  (let ((inhibit-read-only t))
    (subst-char-in-region (git-file-beginning) (git-file-end) ?\^M ?\n)))

(defun git-collapse-file ()
  "Collapse the current file"
  (let ((inhibit-read-only t))
    (subst-char-in-region (git-file-beginning) (git-file-end) ?\n ?\^M)))

(defun git-expand-hunk ()
  "Expand the current hunk"
  (let ((inhibit-read-only t))
    (subst-char-in-region (git-hunk-beginning) (git-hunk-end) ?\^M ?\n)))

(defun git-collapse-hunk ()
  "Collapse the current hunk"
  (let ((inhibit-read-only t))
    (subst-char-in-region (git-hunk-beginning) (git-hunk-end) ?\n ?\^M)))

(defun git-hunk-expanded-p ()
  "Return non-NIL if the current hunk is expanded"
  (save-excursion
    (goto-char (git-hunk-beginning))
    (re-search-forward "\n" (git-hunk-end) t)))

(defun git-file-expanded-p ()
  "Return non-NIL if the current file is expanded"
  (save-excursion
    (goto-char (git-file-beginning))
    (re-search-forward "\n" (git-file-end) t)))

(defun git-toggle-expanded ()
  "Expand or collapse the current hunk"
  (interactive)
  (cond
    ((and (git-file-header-p)
          (git-file-expanded-p))
     (git-collapse-file))
    ((git-file-header-p)
     (git-expand-file))
    ((git-hunk-expanded-p)
     (git-collapse-hunk))
    (t (git-expand-hunk))))

(defun git-current-target ()
  "Returns a list of the form (FILENAME &optional LINE-NUMBER RECENTER-TO)."
  (let ((filename (git-current-filename))
        (s (git-hunk-beginning t))
        (e (point))
        (target nil)
        (recenter-to (- (line-number) (line-number (window-start)))))
    (when s
      (save-excursion
        (goto-char s)
        (looking-at git-hunk-header-re)
        (setq target (string-to-number (match-string 2)))
        (while (and (zerop (forward-line 1))
                    (<= (point-at-eol) e))
          (unless (looking-at "^-")
            (setq target (+ target 1))))))
    (list filename
          (when s target)
          recenter-to)))

(defun git-goto-target (target)
  "Goes to the point that is nearest TARGET without being over"
  (let ((filename (first target))
        (line (second target))
        (recenter-to (third target))
        (e nil)
        (current-line)
        (moved-p))
        
    (goto-char (point-min))
    ;; Try to find the appropriate file
    (while (and (not (string= filename (git-current-filename)))
                (git-next-file t)))
    (if (not (string= filename (git-current-filename)))
      (goto-char (point-min))
      ;; Find appropriate hunk
      (setq e (git-file-end))
      (when line
        (while (and (setq moved-p (git-next-hunk t))
                    (<= (point) e)
                    (progn (looking-at git-hunk-header-re)
                           (<= (string-to-number (match-string 2)) line))))
        (when moved-p
          (git-prev-hunk))
        ;; Find appropriate line of the hunk
        (setq e (git-hunk-end))
        (setq current-line (progn (looking-at git-hunk-header-re)
                                  (string-to-number (match-string 2))))
        (while (and (<= current-line line)
                    (zerop (forward-line 1))
                    (<= (point-at-eol) e))
          (unless (looking-at "^-")
            (setq current-line (+ current-line 1))))
        (unless (= (+ line 1) current-line)
          (git-prev-hunk)))
      ;; Recenter to the same place
      (when recenter-to ;TEST
        (message "recentering to %d" recenter-to) 
        (recenter recenter-to)))))
      
          
      

(defun git-find-in-other-window ()
  "Find the current file; If point is at a hunk, go to the appropriate location within the file."
  (interactive)
  (let ((target (git-current-target)))
    (find-file-other-window (concat default-directory "/" (first target)))
    (when (second target)
      (goto-line (second target)))))

(defvar *git-current-filename* nil
  "Contains a record describing filename of the current enclosing
  file header.  Provided only for performance reasons; it is not
  guaranteed to be set.")

(defvar git-refined-hunks nil
  "Assoc list of hunk plists for hunks that need to be refined")

(defun git-refine-hunk ()
  "Split the current hunk, if possible"
  (interactive)
  (let ((plist (git-hunk-plist)))
    (make-local-variable 'git-refined-hunks)
    (push (cons (plist-get plist :key) (plist-put plist :response "s"))
          git-refined-hunks)
    (revert-buffer t t t)))

(defun git-refine-all-hunks ()
  "Split all splittable hunks"
  (interactive)
  (git-on-all-hunks 'git-refine-hunk))

(defun git-current-filename ()
  "Returns the name of the file enclosing point."
  (or *git-current-filename*
      (let ((fpos (git-file-beginning t)))
        (when fpos
          (save-excursion
            (goto-char fpos)
            (looking-at git-file-header-re)
            (match-string 2))))))

(defun git-on-all-hunks (thunk &optional include-files)
  "Call THUNK with point on every hunk header in the buffer.  If
  INCLUDE-FILES is non-NIL, also calls THUNK on every file
  header."
  (let ((*git-current-filename* nil))
    (save-excursion
      (goto-char (point-min))
      (when (git-file-header-p)
        (setq *git-current-filename* (match-string 2))
        (when include-files
          (save-excursion
            (funcall thunk))))
      (while (git-next-file-or-hunk t)
        (when (git-file-header-p)
          (setq *git-current-filename* (match-string 2)))
        (when (or include-files (git-hunk-header-p))
          (save-excursion
            (funcall thunk)))))))

(defun git-hunk-plist ()
  "Returns a plist describing the current hunk"
  (unless (git-hunk-beginning)
    (error "No current hunk"))
  (let ((filename (git-current-filename))
        (hunkid (save-excursion
                  (goto-char (git-hunk-beginning))
                  (looking-at "^@@ [^@]* @@")
                  (match-string 0)))
        (excluded-p (git-hunk-excluded-p)))
    (list :filename filename
          :hunkid hunkid
          :key (format "%s/%s" filename hunkid)
          :expanded-p (git-hunk-expanded-p)
          :excluded-p excluded-p
          :response (if excluded-p "n" "y"))))
    
(defun git-expand-all-hunks ()
  "Expand all hunks"
  (interactive)
  (git-on-all-hunks 'git-expand-hunk))

(defun git-collapse-all-hunks ()
  "Collapse all hunks"
  (interactive)
  (git-on-all-hunks 'git-collapse-hunk))

(defun git-hunk-excluded-p ()
  "Return exclusion overlay if the current hunk is excluded, or NIL otherwise."
  (overlay-at (point) 'git-hunk-excluded))

(defun git-exclude-hunk (&optional next-hunk)
  "Exclude the current hunk. Advances to the next hunk if called interactively or if NEXT-HUNK is non-NIL."
  (interactive '(t))
  (unless (git-hunk-excluded-p)
    (let ((ov (make-overlay (git-hunk-beginning) (git-hunk-end))))
      (overlay-put ov 'git-hunk-excluded t)
      (overlay-put ov 'face 'git-excluded)))
  (git-collapse-hunk)
  (when next-hunk
    (git-next-hunk)))

(defun git-include-hunk (&optional next-hunk)
  "Include the current hunk. Advances to the next hunk if called interactively or if NEXT-HUNK is non-NIL."
  (interactive '(t))
  (let ((ov (git-hunk-excluded-p)))
    (when ov
      (delete-overlay ov))
    (git-expand-hunk)
    (when next-hunk
      (git-next-hunk))))

(defun git-toggle-hunk-included ()
  "Include or exclude the current hunk"
  (interactive)
  (if (git-hunk-excluded-p)
    (git-include-hunk)
    (git-exclude-hunk)))

(defun git-include-all-hunks ()
  "Include all hunks"
  (interactive)
  (git-on-all-hunks 'git-include-hunk))

(defun git-exclude-all-hunks ()
  "Exclude all hunks"
  (interactive)
  (git-on-all-hunks 'git-exclude-hunk))

(defun git-exclude-remaining ()
  "Exclude current and all following hunks"
  (interactive)
  (when (git-hunk-beginning t)
    (git-exclude-hunk))
  (while (git-next-hunk t)
    (git-exclude-hunk)))

(defun git-exclude-remaining-in-file ()
  "Exclude the current hunk and all following hunks in the same file"
  (interactive)
  (let ((e (git-file-end)))
    (when (git-hunk-beginning t)
      (git-exclude-hunk))
    (while (and (git-next-hunk)
                (< (point) e))
      (git-exclude-hunk))))

(defun git-include-remaining-in-file ()
  "Include the current hunk and all following hunks in the same file"
  (interactive)
  (let ((e (git-file-end)))
    (when (git-hunk-beginning t)
      (git-include-hunk))
    (while (and (git-next-hunk)
                (< (point) e))
      (git-include-hunk))))

(defun git-collect-hunk-plists ()
  "Collect state plists from each hunk in the current buffer, and
  return an assoc list from hunk key to the associated state plist."
  (let ((state-assoc nil))
   (git-on-all-hunks
    (lambda ()
      (cond
        ((and (git-file-header-p) (git-file-expanded-p))
         (push (cons (format "%s/" (git-current-filename))
                     (list :key (format "%s/" (git-current-filename)) :expanded-p t))
               state-assoc))
        ((git-file-header-p)
         (push (cons (format "%s/" (git-current-filename))
                     (list :key (format "%s/" (git-current-filename)) :expanded-p nil))
               state-assoc)
         (save-excursion
           (save-restriction
             (narrow-to-region (git-file-beginning) (git-file-end))
             (git-expand-file)
             (git-collapse-all-hunks)
             (forward-line 1)
             (setq state-assoc (append (git-collect-hunk-plists) state-assoc))
             (git-collapse-file))))
        (t (let ((plist (git-hunk-plist)))
             (push (cons (plist-get plist :key) plist)
                   state-assoc)))))
    t)
   (append (nreverse state-assoc)
           git-refined-hunks)))

(defun git-apply-plists (state-assoc include-files)
  "Restore the state of files and hunks according to the members of STATE-ASSOC."
  (git-on-all-hunks
   (lambda ()
     (let* ((file-p (git-file-header-p))
            (key (if file-p
                   (format "%s/" (git-current-filename))
                   (plist-get (git-hunk-plist) :key)))
            (plist (cdr (assoc key state-assoc))))
       (when plist
         (if file-p
           (cond
             ((and (plist-get plist :expanded-p)
                   (not (git-file-expanded-p)))
              (git-expand-file))
             ((and (not (plist-get plist :expanded-p))
                   (git-file-expanded-p))
              ;; First recursively handle all the contained hunks before collapsing file
              (save-excursion
                (save-restriction
                  (narrow-to-region (git-file-beginning) (git-file-end))
                  (forward-line 1)
                  (git-apply-plists state-assoc nil)
                  (git-collapse-file)))))
           
           (cond
             ((and (not (plist-get plist :excluded-p))
                   (git-hunk-excluded-p))
              (git-include-hunk))
             ((and (plist-get plist :excluded-p)
                   (not (git-hunk-excluded-p)))
              (git-exclude-hunk)))

           (cond
             ((and (not (plist-get plist :expanded-p))
                   (git-hunk-expanded-p))
              (git-collapse-hunk))
             ((and (plist-get plist :expanded-p)
                   (not (git-hunk-expanded-p)))
              (git-expand-hunk)))))))
   include-files))

(defun git-stage-from-whatsnew ()
  "Stage the patches that are included in the current whatsnew buffer"
  (interactive)
  (let ((plist-assoc (git-collect-hunk-plists)))
    (git-hunks '("add" "--patch") plist-assoc
               (lambda (str)
                 (git-whatsnew t)
                 (message "Changes staged")))))
                 
(defun git-commit-from-whatsnew ()
  "Stage the included patches and then commit"
  (interactive)
  (let ((plist-assoc (git-collect-hunk-plists)))
    (git-hunks '("add" "--patch") plist-assoc
               (lambda (str)
                 (git-commit t)))))

;;;; ----------------------------------- git-cvs-commit ----------------------------------

(defun git-cvs-commit ()
  "Calls out to cvs_commit.sh to push the current git state to CVS"
  (interactive)
  (require 'compile)
  (let ((compilation-buffer-name-function (lambda (&rest dummy) "*git: cvs_commit*")))
    (compile "cvs_commit.sh" t)))

;;;; --------------------------------- git-query-manifest --------------------------------

(defun git-query-manifest (&optional same-window)
  "Shows the current state of the repository, according to the index."
  (interactive)
  (git-command-window 'manifest same-window)
  (let ((inhibit-read-only t)
        (lines-left 0))
    (erase-buffer)
    (call-process "git" nil (current-buffer) nil "ls-files" "--stage" "--full-name")
    (goto-char (point-min))
    (save-excursion
      (while (zerop lines-left)
        (when (looking-at "\\(.*\\) .*\t.*$")
          (insert (translate-permissions (match-string 1)))
          (delete-word)
          (goto-char (point-at-bol)))
        (setq lines-left (forward-line 1)))))
  (git-manifest-mode))

(defvar git-manifest-font-lock-keywords
  '((".*\t\\(.*\\)$" (1 'git-file-header-filename))))

(defvar git-manifest-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\r] 'git-goto-manifest-file)
    (define-key map [?q] 'darcs-quit-current)
    map))

(defun git-manifest-mode ()
  (kill-all-local-variables)
  (setq font-lock-defaults '((git-manifest-font-lock-keywords) t))
  (setq major-mode 'git-manifest)
  (setq mode-name "git-manifest")
  (use-local-map git-manifest-map)
  (set (make-local-variable 'revert-buffer-function)
       (lambda (ignore-auto noconfirm)
         (git-query-manifest t)))
  (turn-on-font-lock))

(defun git-goto-manifest-file ()
  "Go to the file listed on the current line"
  (interactive)
  (save-excursion
    (goto-char (point-at-bol))
    (looking-at ".*\t\\(.*\\)$")
    (let ((filename (match-string 1)))
      (if filename
        (find-file-other-window (concat default-directory "/" filename))
        (error "No filename at point")))))

;;;; ------------------------------------- git-status ------------------------------------

(defun git-status (&optional same-window)
  "Gives the current status of the index and working tree."
  (interactive)
  (git-command-window 'status same-window)
  (git-status-mode)
  (git-status-internal))

(defun git-status-internal ()
  (let ((inhibit-read-only t)
        (cooked-text
         ;; Do all our editing inside a temp buffer to prevent screen flicker.  We copy everything
         ;; back to tbe real buffer when we're ready to show the results.
         (with-temp-buffer
           (call-process "git" nil (current-buffer) nil "status")
           (goto-char (point-min))
           (git-markup-status)
           (git-on-all-hunks 'git-collapse-file)
           (buffer-substring (point-min) (point-max)))))
    (erase-buffer)
    (insert cooked-text)
    (goto-char (point-min))))
  
(defun git-current-chapter ()
  "Returns one of '(staged unstaged untracked) representing the current chapter for point."
  (save-excursion
    (goto-char (git-chapter-beginning))
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
       (git-sync-command nil "reset" "HEAD" "--" filename))
      ((unstaged untracked)
       (if (file-exists-p filename)
         (git-sync-command nil "add" "--" filename)
         (git-sync-command nil "rm" "--" filename))))
    (revert-buffer t t t)
    (goto-char (point-min))
    (re-search-forward (format "^[-ld].*:  ?%s" filename) nil t)
    (goto-char (point-at-bol))))
  

(defvar git-status-font-lock-keywords
  (append `((,git-chapter-header-re (0 'git-chapter-header))
            ("^# On branch \\(.*\\)" (0 'font-lock-comment-face) (1 'git-branch prepend)))
          git-whatsnew-font-lock-keywords))

(defvar git-status-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map git-hunk-display-map)
    (define-key map [?\ ] 'git-toggle-file-staged)
    (define-key map [?n] 'git-next-file)
    (define-key map [?p] 'git-prev-file)
    (define-key map [(control ?c) (control ?c)] 'git-commit-from-status)
    (define-key map [(control ?c) (control ?s)] 'darcs-quit-current)
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
  (turn-on-font-lock))

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
           (setq branch (match-string 1)))

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
               (staged (message "git diff --cached -- %s" filename)
                       (call-process "git" nil (current-buffer) nil "diff" "--cached" "--" filename))
               (unstaged (message "git diff -- %s" filename)
                       (call-process "git" nil (current-buffer) nil "diff" "--" filename)))
             (set-marker e (point))
             (save-restriction
               (narrow-to-region s e)
               (goto-char (point-min))
               (git-markup-hunks))
             (goto-char e)
             (message nil)))

          ((looking-at "#\t\\(renamed:\\)?[ ]+\\(.*\\) -> \\(.*\\)$")
           (let ((old-filename (match-string 2))
                 (new-filename (match-string 3))
                 (s nil))
             (kill-this-line)
             (setq s (point))
             (ecase chapter
               (staged (message "git diff --cached -- %s" new-filename)
                       (call-process "git" nil (current-buffer) nil "diff" "--cached" "--" new-filename)))
             (set-marker e (point))
             (save-restriction
               (narrow-to-region s e)
               (goto-char (point-min))
               (git-markup-hunks)
               (goto-char (point-min))
               (or (git-file-header-p) (git-next-file))
               (forward-line 1)
               (insert "# renamed from '" old-filename "'\n"))
             (goto-char e)
             (message nil)))
          
          ((and (eq chapter 'untracked)
                (looking-at "#\t\\(.*\\)$"))
           (let* ((filename (match-string 1))
                  (s nil)
                  (attrs (file-attributes filename)))
             (kill-this-line)
             (setq s (point))
             (insert (format "new file mode %d%.4o\n"
                             (if (stringp (elt attrs 0)) 12 10)
                             (file-modes filename)))
             (insert "--- /dev/null\n"
                     "+++ b/" filename "\n")
             (set-marker e (point))
             (save-restriction
               (narrow-to-region s e)
               (goto-char (point-min))
               (git-markup-hunks))
             (goto-char e)
             (setq lines-left nil)
             (message nil)))
          
          ;; Kill anything we don't recognize
          (t
           (kill-this-line)))
        (unless lines-left
          (setq lines-left (forward-line 1)))))))

(defun git-commit-from-status ()
  "Commit from the status window (i.e., don't pop up commit in a separate window)."
  (interactive)
  (git-commit t))

;;;; ------------------------------------- git-commit ------------------------------------

(defvar git-commit-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?c) (control ?c)] 'git-commit-execute)
    (define-key map [(control ?c) ?n] 'git-next-file)
    map)
  "Only thing that's different about the commit map is that is has a `C-c C-c' binding")

(defvar git-commit-status-submap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map git-status-map)
    (define-key map [(control ?c) (control ?c)] 'git-commit-execute)
    map)
  "Only thing that's different about the commit map is that is has a `C-c C-c' binding")

(defvar git-commit-buffer-instructions
  "# Please enter the commit message for your changes.
# (Comment lines starting with '#' will not be included)
#\\<git-commit-map>
# Type \\[git-commit-execute] to commit the staged changes.
# Type \\[kill-buffer] to abandon this commit buffer.
#
# The current status is listed below; you may stage or unstage
# changes before committing.
#
")

(defun git-commit-mode ()
  (unless (eq major-mode 'git-commit)
    (kill-all-local-variables))
  (setq font-lock-defaults '((git-status-font-lock-keywords) t))
  (setq major-mode 'git-commit)
  (setq mode-name "git-commit")
  (use-local-map git-commit-map)
  (set (make-local-variable 'revert-buffer-function)
       (lambda (ignore-auto noconfirm)
         (git-commit t (git-commit-message))))
  (setq selective-display t)
  (turn-on-font-lock))

(defvar git-commit-msg-overlay nil)

(defun git-commit (&optional same-window msg)
  "Commit the currently staged patches."
  (interactive)
  (git-command-window 'commit same-window)
  (git-commit-mode)
  (toggle-read-only 0)
  (let ((inhibit-read-only t))
    ;; Set up the status sub-display
    (git-status-internal)
    (insert (substitute-command-keys git-commit-buffer-instructions))

    (save-excursion
      (when (and (git-next-file-or-hunk)
                 (eq 'staged (git-current-chapter)))
        (subst-char-in-region (git-chapter-beginning) (git-chapter-end) ?\^M ?\n)))
    
    (goto-char (point-min))
    (insert "\n\n\n")
    (put-text-property (point) (point-max) 'read-only t)
    (put-text-property (point) (point-max) 'keymap git-commit-status-submap)

    ;; Set up the message overlay
    (unless git-commit-msg-overlay
      (set (make-local-variable 'git-commit-msg-overlay)
           (make-overlay (point-min) (point) nil nil t)))
    (move-overlay git-commit-msg-overlay (point-min) (point))
    (overlay-put git-commit-msg-overlay 'face 'git-commit-msg))
  (goto-char (point-min))
  (when msg
    (insert msg)
    (goto-char (point-min))))
  
(defun git-commit-execute ()
  "Commit the currently-staged changes using a message from the current commit buffer!"
  (interactive)
  (message "git commit")
  (git-sync-command (current-buffer) "commit" "-m" (git-commit-message))
  ;; If we return to a whatsnew or status window, refresh it
  (when (or (eq major-mode 'git-whatsnew)
            (eq major-mode 'git-status))
    (revert-buffer t t t)))

(defun git-commit-message ()
  "Return the commit message from the current buffer"
  (darcs-trim-newlines
   (buffer-substring (overlay-start git-commit-msg-overlay)
                     (overlay-end git-commit-msg-overlay))))

(defun git-revert ()
  "Revert the selected hunks"
  (interactive)
  (when (yes-or-no-p "Do you really want to revert these changes? ")
    (let ((plist-assoc (git-collect-hunk-plists)))
      (git-hunks '("stash" "save" "--patch" "cherry-picked revert") plist-assoc
                 (lambda (str)
                   (git-sync-command nil "stash" "drop")
                   (git-whatsnew t)
                   (message "Changes reverted"))))))


;;;; ---------------------------- simple pass-through commands ---------------------------

(defun git-push ()
  (interactive)
  (message "git push")
  (git-sync-command nil "push"))

(defun git-pull ()
  (interactive)
  (message "git pull")
  (git-sync-command nil "pull"))

(defun git-init ()
  (interactive)
  (git-sync-command nil "init"))

(defun git-add ()
  (interactive)
  (git-sync-command nil "add" (buffer-truename (current-buffer))))

(defun git-remove ()
  (interactive)
  (git-sync-command nil "rm" (buffer-truename (current-buffer))))

;;;; ====================================== git process interaction =====================================

(defun git-sync-command (killable-buffer &rest args)
  "Run git <args. synchronously.  Prints output as a message; kills KILLABLE-BUFFER on success if non-nil."
  (let ((ret nil))
    (with-temp-buffer
      (setq ret (apply 'call-process "git" nil (list (current-buffer) t) nil args))
      (while (progn
               (goto-char (point-min))
               (when (re-search-forward "^[^\r\n]*\r\\|^# [^\n]*\n\\|^\r?\n" nil t)
                 (delete-region (match-beginning 0) (match-end 0))
                 t)))
      (and (goto-char (point-max))
           (looking-at "^")
           (not (= (point) (point-min)))
           (delete-char -1))
      (message "%s" (buffer-substring (point-min) (point-max))))
    (when (and (zerop ret)
               killable-buffer)
      (kill-buffer killable-buffer))
    ret))
  
(defvar git-hunks-scan-pos nil
  "The point that `git-hunks-filter' should start scanning from")

(defvar git-responses nil
  "Patch responses for the currently-running interactive darcs process")

(defvar git-hunks-thunk nil
  "Function to execute after `git-hunks'")

(defun git-hunks (options responses thunk)
  "Run git with OPTIONS, responding to 'hunk'-level prompts based on RESPONSES.
   If a hunk is not listed in RESPONSES, it will be skipped.
   When the command has finished executing, THUNK is called with
   the contents of the output buffer."
  (let ((root-dir default-directory)
        (cmd-line "git")
        (process nil))
    (setq options (remove nil options))
    (dolist (opt options)
      (setq cmd-line (concat cmd-line " " opt)))
    (message "%s" cmd-line)
    
    (when (and (get-buffer "*git output*")
               (get-buffer-process "*git output*")
               (eq 'run (process-status (get-buffer-process "*git output*")))
               (yes-or-no-p "A git process is already running; kill it? ")) ;
  (kill-process (get-buffer-process "*git output*"))
  (kill-buffer "*git output*"))
               (setq process (apply 'start-process cmd-line "*git output*" "git" options))
               (with-current-buffer (process-buffer process)
                 (erase-buffer)
                 (set (make-local-variable 'git-responses) responses)
                 (set (make-local-variable 'git-hunks-scan-pos) (point-min))
                 (setq default-directory root-dir)
                 (make-local-hook 'kill-buffer-hook)
                 (add-hook 'kill-buffer-hook 'kill-current-buffer-process nil t))
               (set-process-sentinel process 'git-hunks-sentinel)
               (set (make-local-variable 'git-hunks-thunk) thunk)
               (set-process-filter process 'git-hunks-filter)))

(defun git-hunks-sentinel (proc string)
  (flet ((bufstr ()
           (with-current-buffer (process-buffer proc)
             (buffer-substring (point-min) (point-max)))))
    (if (and (string-match "^exited abnormally" string)
             (process-buffer proc))
      (message "%s\n%s: %s" (darcs-trim-newlines (bufstr))
               (process-name proc)
               (darcs-trim-newlines string))
      (when (and (not (eq 'run (process-status proc)))
                 (buffer-live-p (process-buffer proc)))
        (if git-hunks-thunk
          (funcall git-hunks-thunk (bufstr))
          (message nil))))))

(defun git-hunk-prompt (&optional opt)
  "Return non-nil if point is looking at a hunk prompt.
   If OPT is non-nil, return non-NIL only if OPT is one of the available commands.
   Side effect: Inserts a newline at the end of the prompt if one is not already present."
  (let ((case-fold-search nil))
    (if opt
      (looking-at (format "\\(?:Stage\\|Stash\\) \\(this hunk\\|deletion\\) [^]]*%s[^]]*\\]\\?" opt))
      (looking-at "\\(?:Stage\\|Stash\\) \\(this hunk\\|deletion\\) [^]]*\\]\\?"))))

(defun git-hunk-response ()
  "If we have a response for this hunk, then return it, otherwise return NIL"
  (save-excursion
    (re-search-backward "^@@ [^@]* @@")
    (let ((hunk-id (match-string 0)))
      (re-search-backward "^--- a/\\([^\r\n\t]*\\)\t?$")
      (let* ((hunk-filename (match-string 1))
             (hunk-key (format "%s/%s" hunk-filename hunk-id))
             (hunk-plist (cdr (assoc hunk-key git-responses))))
        (plist-get hunk-plist :response)))))

(defun git-hunks-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (goto-char (process-mark proc))
      (save-excursion
        (insert string "\n")
        (set-marker (process-mark proc) (point)))
      (goto-char (point-at-bol))
      (while (and (buffer-live-p (process-buffer proc))
                  (< (point) (point-max)))
        (cond
          ;; Splittable hunk
          ((and (git-hunk-prompt "s")
                (string= (git-hunk-response) "s"))
           ;; Delete the unsplit hunk
           (let ((e (point-at-eol))
                 (s (re-search-backward "^@@")))
             (delete-region s e))
           ;; Indicate that we want to split the hunk
           (process-send-string proc "s\n"))
          
          ;; Other hunk that we have a response for
          ((and (git-hunk-prompt) (git-hunk-response))
           (let ((cmd (format "%s\n" (git-hunk-response))))
             (save-excursion (goto-char (point-at-eol))
                             (insert cmd))
             (process-send-string proc cmd)))
          
          ;; Hunk that we have no response for
          ((git-hunk-prompt "n")
           (save-excursion (goto-char (point-at-eol))
                           (insert "n [default]\n"))
           (process-send-string proc "n\n")))
        (forward-line)))))


;;;; ============================================ From xdarcs ===========================================

(defun git-maybe-recenter (&optional median-height)
  "Recenter if we are more than MEDIAN-HEIGHT lines from the top of the buffer"
  (setq median-height (or median-height (/ (window-body-height) 4)))
  (let ((median-line (+ (line-number (window-start))
                        median-height)))
    (when (> (line-number) median-line)
      (recenter median-height)))
  (point))

;; kill-current-buffer-process
;; one-line-buffer
;; darcs-quit-current
;; darcs-trim-newlines

(provide 'jgit)
;;; jgit.el ends here
