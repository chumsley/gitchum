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


;;;; ============================================== keymaps =============================================

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
    (define-key map [?y] 'git-include-hunk)
    (define-key map [?x] 'git-exclude-hunk)
    (define-key map [?s] 'git-exclude-all-in-current-file)
    (define-key map [?f] 'git-include-all-in-current-file)
    (define-key map [?a] 'git-expand-all-hunkes)
    (define-key map [?z] 'git-collapse-all-hunkes)
    (define-key map [?Y] 'git-include-all-hunkes)
    (define-key map [?X] 'git-exclude-all-hunkes)
    map)
  "Keymap for displaying lists of atomic hunkes")

;;;; ============================================= Commands =============================================

;;;; ------------------------------------ git-whatsnew -----------------------------------

(defun git-whatsnew (&optional same-window)
  "Prints a list of all the changes in the current repo"
  (interactive)
  (let ((repo-dir default-directory)) ;TODO `git-repo-dir'
    (if same-window
      (switch-to-buffer (format "*git whatsnew: (%s)" repo-dir))
      (switch-to-buffer-other-window (format "*git whatsnew: (%s)" repo-dir)))
    (setq default-directory repo-dir)
    (erase-buffer)
    (git-hunks repo-dir '("add" "--patch") nil
               (lambda (raw-output)
                 (insert raw-output)
                 (goto-char (point-min))
                 (save-excursion
                   (git-markup-hunks))))))

(defun git-markup-hunks ()
  "Starting from point and moving down the rest of the buffer,
   convert raw output from `git-hunks' and edit it to the format
   that we present to the user"
  (let ((lines-left 0)
        (latest-index nil)
        (latest-permissions nil))
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
           (kill-this-line))

          ((looking-at "--- a/\\([^\r\n]*\\)")
           (let ((filename (match-string 1)))
             (kill-this-line)
             (save-excursion
               (insert (format "%s %s (%s)\n" (translate-permissions latest-permissions) filename latest-index)))))

          ((looking-at "+++ b/")
           (kill-this-line))

          ;; Ensure that hunk headers don't have context lines at the end
          ((looking-at "@@ \\([^@]*\\) @@\\([\r\n]*\\)")
           (unless (zerop (length (match-string 2)))
             (save-excursion
               (goto-char (match-beginning 2))
               (insert "\n"))))
          
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

;;; jgit.el ends here
