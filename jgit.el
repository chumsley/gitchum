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

(defvar git-process-scan-pos (point-min)
  "The point that `git-process-filter' should start scanning from")

(defvar git-responses nil
  "Patch responses for the currently-running interactive darcs process")

;;;; ============================================= Commands =============================================

(defun git-whatsnew ()
  "Prints a list of all the changes in the current repo"
  (interactive)
  (git-hunks default-directory '("add" "--patch"))
  (switch-to-buffer "*git output*"))
  

;;;; ====================================== git process interaction =====================================

(defun git-hunks (root-dir &optional options responses)
  "Run git with OPTIONS, responding to 'hunk'-level prompts as follows:
     1. Hunks are always split if possible
     2. Atomic (unsplittable) hunks are skipped without
        decision unless they are listed in RESPONSES, in
        which case the specified response is given."
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
      (set (make-local-variable 'git-process-scan-pos) (point-min))
      (setq default-directory root-dir)
      (make-local-hook 'kill-buffer-hook)
      (add-hook 'kill-buffer-hook 'kill-current-buffer-process nil t)) ; NOTE `kill-current-buffer-process' is defined in xdarcs
    (set-process-sentinel process 'git-process-sentinel)
    (set-process-filter process 'git-hunks-process-filter)))

(defun git-process-sentinel (proc string)
  (when (and (string-match "^exited abnormally" string)
             (process-buffer proc))
    (message "%s: %s" (process-name proc) string))
  (when (and (not (eq 'run (process-status proc)))
             (buffer-live-p (process-buffer proc))
             (not git-debug))
    (kill-buffer (process-buffer proc))))

(defvar git-process-filter-mark-overlay nil
  "An overlay that highlights the currently unconsumed output in the git output buffer")

(defun git-stage-prompt (&optional opt)
  "Return non-nil if point is looking at a staging prompt.
   If OPT is non-nil, return non-NIL only if OPT is one of the available commands.
   Side effect: Inserts a newline at the end of the prompt if one is not already present."
  (let ((case-fold-search nil))
    (when (looking-at "Stage this hunk [^]]*\\]\\?")
      ;; Force a newline at the end of the prompt to keep our filtering from getting all screwed up
      (save-excursion
        (re-search-forward "\\]\\? ")
        (unless (looking-at "\r?\n")
          (insert "\n")))
      (if opt
        (looking-at (format "Stage this hunk \\[[^]]*\\/%s\\/" opt))
        t))))

(defun git-hunk-response ()
  "If we have a response for this hunk, then return it, otherwise return NIL"
  (save-excursion
    (re-search-backward "^@@ [^@]* @@")
    (let ((hunk-id (match-string 0)))
      (re-search-backward "^--- a/\\([^\r\n]*\\)$")
      (let ((hunk-filename (match-string 1)))
        (cdr (assoc (format "%s/%s" hunk-filename hunk-id) git-responses))))))

(defun git-hunks-process-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (goto-char (process-mark proc))
      (save-excursion
        (insert string "\n")
        (set-marker (process-mark proc) (point)))
      (goto-char (point-at-bol))
      (while (and (buffer-live-p (process-buffer proc))
                  (< (point) (point-max)))
        (when git-debug
          (message (format "Filter: %s" (buffer-substring (point-at-bol) (point-at-eol)))))
        (cond

          ;; Splittable hunk
          ((git-stage-prompt-contains "s")
           ;; Delete the unsplit hunk
           (let ((e (point-at-eol))
                 (s (re-search-backward "^@@")))
             (delete-region s e))
           ;; Indicate that we want to split the hunk
           (process-send-string proc "s\n"))
          
          ;; Atomic hunk that we have a response for
          ((and (git-stage-prompt) (git-hunk-response))
           (process-send-string proc (format "%s\n" (git-hunk-response)))
           (delete-region (point-at-bol) (save-excursion (forward-line 1) (point))))
          
          ;; Atomic hunk that we have no response for
          ((git-stage-prompt "j")
           (process-send-string proc "j\n")
           (delete-region (point-at-bol) (save-excursion (forward-line 1) (point))))           
          ((git-stage-prompt "d")
           (process-send-string proc "d\n")
           (delete-region (point-at-bol) (save-excursion (forward-line 1) (point))))

          ;; Removable line
          ((looking-at "Split into ")
           (delete-region (point-at-bol) (save-excursion (forward-line 1) (point))))
          )
        (forward-line)))))



;;; jgit.el ends here
