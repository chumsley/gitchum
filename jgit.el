;;TODO
;; 1. Staging
;; 2. Commit

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

;;; Code:

;;;; =============================================== Faces ==============================================

(defface git-header
    '((((class color) (background light))
       (:background "grey85"))
      (t (:bold t)))
  "Common aspects of headers"
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

(defface git-file-header-deleted
    '((((class color))
       (:foreground "red"))
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

;;;; ---------------------------- Other customizable settings ----------------------------

(defcustom git-command-prefix [(control x) ?g]
  "Prefix key sequence for git commands."
  :group 'git)

(defcustom git-debug nil
  "When true, the *git output* buffer is never deleted"
  :type 'boolean
  :group 'git)

(defvar git-hunks-scan-pos (point-min)
  "The point that `git-hunks-filter' should start scanning from")

(defvar git-responses nil
  "Patch responses for the currently-running interactive darcs process")


;;;; ============================================== Keymaps =============================================

;;;; ----------------------------------- global keymap -----------------------------------

(defvar git-prefix-map
  (let ((map (make-sparse-keymap)))
;    (define-key map [?a] 'git-add)
;    (define-key map [?b] 'git-blame)
;    (define-key map [?l] 'git-log)
;    (define-key map [?=] 'git-diff)
;    (define-key map [??] 'git-describe-bindings)
;    (define-key map [?d] 'git-describe-patch)
;    (define-key map [?-] 'git-ediff)
;    (define-key map [?f] 'git-filelog)
    (define-key map [?G] 'git-pull)
    (define-key map [?S] 'git-push)
;    (define-key map [?i] 'git-init)
    (define-key map [?m] 'git-query-manifest)
    (define-key map [?w] 'git-whatsnew)
;    (define-key map [?x] 'git-remove)
    map)
  "The prefix for git commands")

(if (not (keymapp (lookup-key global-map git-command-prefix)))
  (define-key global-map git-command-prefix git-prefix-map))

(defvar git-hunk-display-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\ ] 'git-toggle-hunk-included)
    (define-key map [?\r] 'git-toggle-hunk-expanded)
    (define-key map [(control return)] 'git-find-hunk-in-other-window)
    (define-key map [?j] 'git-next-hunk)
    (define-key map [?k] 'git-prev-hunk)
    (define-key map [?J] 'git-next-file)
    (define-key map [?K] 'git-prev-file)
    (define-key map [?y] 'git-include-hunk)
    (define-key map [?n] 'git-exclude-hunk)
    (define-key map [?s] 'git-exclude-remaining-in-file)
    (define-key map [?f] 'git-include-remaining-in-file)
    (define-key map [?a] 'git-expand-all-hunks)
    (define-key map [?z] 'git-collapse-all-hunks)
    (define-key map [?Y] 'git-include-all-hunks)
    (define-key map [?N] 'git-exclude-all-hunks)
    (define-key map [?d] 'git-exclude-remaining)
    (define-key map [?q] 'darcs-quit-current)
    map)
  "Keymap for displaying lists of atomic hunks")

(defvar git-whatsnew-map
  (let ((map (make-sparse-keymap 'git-whatsnew-map)))
    (set-keymap-parent map git-hunk-display-map)
    (define-key map [(control ?c) (control ?s)] 'git-stage-from-whatsnew)
    (define-key map [(control ?c) (control ?c)] 'git-record-from-whatsnew)
;    (define-key map [(control ?c) (control ?r)] 'git-commit-revert)
    (define-key map [(control ?x) ?#] 'git-record-from-whatsnew)
    map)
  "Keymap for git-whatsnew-mode")

;;;; ============================================= Commands =============================================

;;;; ------------------------------------ git-whatsnew -----------------------------------

(defun git-whatsnew (&optional same-window)
  "Prints a list of all the changes in the current repo"
  (interactive)
  (let ((repo-dir default-directory)) ;TODO `git-repo-dir'
    (if same-window
      (switch-to-buffer (format "*git whatsnew: (%s)" repo-dir))
      (switch-to-buffer-other-window (format "*git whatsnew: (%s)" repo-dir)))
    (toggle-read-only 1)
    (setq default-directory repo-dir)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (git-hunks repo-dir '("add" "--patch") nil
               (lambda (raw-output)
                 (let ((inhibit-read-only t))
                   (insert raw-output)
                   (goto-char (point-min))
                   (save-excursion
                     (git-markup-hunks)))
                 (git-whatsnew-mode)))))

(defun git-markup-hunks ()
  "Starting from point and moving down the rest of the buffer,
   convert raw output from `git-hunks' and edit it to the format
   that we present to the user"
  (let ((lines-left 0)
        (latest-index nil)
        (latest-permissions nil)
        (latest-filename nil)
        (delcheated nil))
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
           (setq delcheated nil)
           (kill-this-line))

          ((looking-at "deleted file mode \\([0-7]+\\)")
           (setq latest-permissions (match-string 1))
           (setq delcheated t)
           (kill-this-line))

          ((looking-at "--- \\(?:a/\\([^\r\n]*\\)\\|/dev/null\\)")
           (let ((filename (match-string 1)))
             (setq latest-filename filename)
             (kill-this-line)))

          ((looking-at "\\+\\+\\+ \\(?:b/\\([^\r\n]*\\)\\|/dev/null\\)")
           (let ((filename (match-string 1)))
             (setq latest-filename (or filename latest-filename))
             (kill-this-line)
             ;; There must be a space after the filename, to make it easier to seperate the filename from the deleted tag
             (insert (translate-permissions latest-permissions) " " latest-filename " " (if delcheated "[deleted]" "") "\n")))

          ;; Ensure that hunk headers don't have context lines at the end
          ((looking-at "@@ \\([^@]*\\) @@\\([\r\n]*\\)")
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
            (xlat-perms (substring permstring 5 6) 1 "t"))))

(defvar git-whatsnew-font-lock-keywords
  '(
    ("^[ld-][r-][w-][xs-][r-][w-][xs-][r-][w-][xt-] \\(.*\\) \\[\\(deleted\\)\\]\n"
     (0 'git-file-header) (1 'git-file-header-filename prepend) (2 'git-file-header-deleted prepend))
    ("^[ld-][r-][w-][xs-][r-][w-][xs-][r-][w-][xt-] \\(.*\\) \n"
     (0 'git-file-header) (1 'git-file-header-filename prepend))
    
    ("\\(^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\) @@\\).*$"
     (1 'git-hunk-header))
    ("^[+>].*$"
     (0 'git-line-added))
    ("^[-<].*$"
     (0 'git-line-removed))
    ("^\\\\.*$"
     (0 'git-verbosity))
    ))

(define-derived-mode git-whatsnew-mode fundamental-mode
  (kill-all-local-variables)
  (setq font-lock-defaults '((git-whatsnew-font-lock-keywords))) ; Is this the right way to do this?
  (setq mode-name "git-whatsnew")
  (use-local-map git-whatsnew-map)
  (setq revert-buffer-function (lambda (ignore-auto noconfirm)
                                 (git-whatsnew t)))
  (setq selective-display t))

(defun git-file-header-p ()
  "Non-nil if point is currently on a file header line"
  (save-excursion
    (goto-char (point-at-bol))
    (looking-at "^[ld-][r-][w-][xs-][r-][w-][xs-][r-][w-][xt-] ")))

(defun git-hunk-header-p ()
  "Non-nil if point is currently on a hunk header line"
  (save-excursion
    (goto-char (point-at-bol))
    (looking-at "^@@")))

(defun git-file-beginning ()
  "Returns the position of the beginning of the current file"
  (if (git-file-header-p)
    (point-at-bol)
    (save-excursion
      (if (re-search-backward "^[ld-][r-][w-][xs-][r-][w-][xs-][r-][w-][xt-] ")
        (point)
        (error "No current file")))))

(defun git-file-end ()
  "Returns the position of the end of the current file"
  (save-excursion
    (goto-char (git-file-beginning))
    (while (and (zerop (forward-line 1))
                (not (git-file-header-p))))
      (when (= (point) (point-at-bol))
        (forward-line -1))
      (point-at-eol)))

(defun git-hunk-beginning (&optional noerror)
  "Returns the position of the beginning of the current hunk"
  (if (git-hunk-header-p)
    (point-at-bol)
    (save-excursion
      (if (re-search-backward "^@@" nil t)
        (point)
        (unless noerror (error "No current hunk"))))))

(defun git-hunk-end ()
  "Returns the position of the end of the current hunk"
  (save-excursion
    (goto-char (git-hunk-beginning))
    (while (and (zerop (forward-line 1))
                (not (git-file-header-p))
                (not (git-hunk-header-p))))
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
    (if p
      (goto-char p)
      (unless noerror (error "No more patches")))))

(defun git-prev-hunk (&optional noerror)
  "Move point to the beginning of the previous hunk"
  (interactive)
  (let ((p (save-excursion
             (goto-char (git-hunk-beginning))
             (and (re-search-backward "^@@" nil t)
                  (match-beginning 0)))))
               
    (if p
      (goto-char p)
      (unless noerror (error "No more patches")))))

(defun git-next-file ()
  "Move point to the beginning of the next file"
  (interactive)
  (let ((p (save-excursion
             (goto-char (point-at-eol))
             (and (re-search-forward "^[ld-][r-][w-][xs-][r-][w-][xs-][r-][w-][xt-] " nil t)
                  (match-beginning 0)))))
    (if p
      (goto-char p)
      (error "No more files"))))

(defun git-prev-file ()
  "Move point to the beginning of the previous file"
  (interactive)
  (let ((p (save-excursion
             (unless (git-file-header-p)
               (re-search-backward "^[ld-][r-][w-][xs-][r-][w-][xs-][r-][w-][xt-] " nil t))
             (and (re-search-backward "^[ld-][r-][w-][xs-][r-][w-][xs-][r-][w-][xt-] " nil t)
                  (match-beginning 0)))))
               
    (if p
      (goto-char p)
      (error "No more files"))))

(defun git-expand-hunk ()
  "Expand the current hunk"
  (subst-char-in-region (git-hunk-beginning) (git-hunk-end) ?\^M ?\n))

(defun git-collapse-hunk ()
  "Collapse the current hunk"
  (subst-char-in-region (git-hunk-beginning) (git-hunk-end) ?\n ?\^M))

(defun git-toggle-hunk-expanded ()
  "Expand or collapse the current hunk"
  (interactive)
  (let ((inhibit-read-only t)
        (expanded (save-excursion
                    (goto-char (git-hunk-beginning))
                    (re-search-forward "\n" (git-hunk-end) t))))
    (if expanded
      (git-collapse-hunk)
      (git-expand-hunk))))

(defun git-on-all-hunks (thunk)
  "Call THUNK with point on every hunk in the buffer"
  (save-excursion
    (goto-char (point-min))
    (while (git-next-hunk t)
      (save-excursion
        (funcall thunk)))))
    
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

;;;; ====================================== git process interaction =====================================

(defvar git-hunks-thunk nil
  "Function to execute after `git-hunks'")

(defun git-hunks (root-dir &optional options responses thunk)
  "Run git with OPTIONS, responding to 'hunk'-level prompts as follows:
     1. Hunks are always split if possible
     2. Atomic (unsplittable) hunks are skipped without
        decision unless they are listed in RESPONSES, in
        which case the specified response is given.
   When the command has finished executing, THUNK is called with
   the contents of the output buffer."
  (let ((default-directory root-dir)
        (cmd-line "git")
        (process nil))
    (setq options (remove nil options))
    (dolist (opt options)
      (setq cmd-line (concat cmd-line " " opt)))
    (message "%s" cmd-line)
    
    (when (and (get-buffer "*git output*")
               (get-buffer-process "*git output*")
               (eq 'run (process-status (get-buffer-process "*git output*")))
               (yes-or-no-p "A git process is already running; kill it?"))
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
  (when (and (string-match "^exited abnormally" string)
             (process-buffer proc))
    (message "%s: %s" (process-name proc) string))
  (when (and (not (eq 'run (process-status proc)))
             (buffer-live-p (process-buffer proc)))
    (message "") ; Get rid of the command-line message
    (when git-hunks-thunk
      (funcall git-hunks-thunk (with-current-buffer (process-buffer proc)
                                 (buffer-substring (point-min) (point-max)))))
    (unless git-debug
      (kill-buffer (process-buffer proc)))))

(defvar git-process-filter-mark-overlay nil
  "An overlay that highlights the currently unconsumed output in the git output buffer")

(defun git-stage-prompt (&optional opt)
  "Return non-nil if point is looking at a staging prompt.
   If OPT is non-nil, return non-NIL only if OPT is one of the available commands.
   Side effect: Inserts a newline at the end of the prompt if one is not already present."
  (let ((case-fold-search nil))
    (if opt
      (looking-at (format "Stage this hunk \\[[^]]*\\/%s\\/" opt))
      (looking-at "Stage this hunk [^]]*\\]\\?"))))

(defun git-hunk-response ()
  "If we have a response for this hunk, then return it, otherwise return NIL"
  (save-excursion
    (re-search-backward "^@@ [^@]* @@")
    (let ((hunk-id (match-string 0)))
      (re-search-backward "^--- a/\\([^\r\n]*\\)$")
      (let ((hunk-filename (match-string 1)))
        (cdr (assoc (format "%s/%s" hunk-filename hunk-id) git-responses))))))

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
          ((git-stage-prompt "s")
           ;; Delete the unsplit hunk
           (let ((e (point-at-eol))
                 (s (re-search-backward "^@@")))
             (delete-region s e))
           ;; Indicate that we want to split the hunk
           (process-send-string proc "s\n"))
          
          ;; Atomic hunk that we have a response for
          ((and (git-stage-prompt) (git-hunk-response))
           (process-send-string proc (format "%s\n" (git-hunk-response))))
          
          ;; Atomic hunk that we have no response for
          ((git-stage-prompt "n")
           (process-send-string proc "n\n")))
        (forward-line)))))


;;;; ============================================ From xdarcs ===========================================
;; kill-current-buffer-process
;; one-line-buffer
;; darcs-quit-current

;;; jgit.el ends here
