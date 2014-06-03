;;; gitchum.el --- xdarcs-like git integration for emacs

;; Copyright (C) 2010 James Wright

;; Author: James Wright <james@chumsley.org>
;; Created: 28 Apr 2010
;; Keywords: 

;; This file is not yet part of GNU Emacs.

;; gitchum.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; gitchum.el is distributed in the hope that it will be useful,
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
;; - git-log and git-filelog
;;   Don't know yet what this should look like exactly; I usually want to describe a patch and/or diff against it
;; - git-amend
;; - git-blame?  (I don't know if I really care about this or not)
;;; Code:

;;;; =============================================== Faces ==============================================

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
    (define-key map [?=] 'git-diff)
    (define-key map [?-] 'git-ediff)
;    (define-key map [??] 'git-describe-bindings)
;    (define-key map [?d] 'git-describe-patch)
;    (define-key map [?f] 'git-filelog)
    (define-key map [?G] 'git-pull)
    (define-key map [?S] 'git-push)
    (define-key map [?i] 'git-init)
    (define-key map [?m] 'git-query-manifest)
    (define-key map [?w] 'git-whatsnew)
    (define-key map [?s] 'git-staged)
    (define-key map [?c] 'git-commit)
    (define-key map [?x] 'git-remove)
    map)
  "The prefix for git commands")

(if (not (keymapp (lookup-key global-map git-command-prefix)))
  (define-key global-map git-command-prefix git-prefix-map))

;;;; ============================================= Commands =============================================

(defun git-command-window (name same-window)
  "Switch to a readonly git command window with the
  default-directory set to the repo root.  Contents and local
  variables might be leftover from previous
  instances."
  (let ((repo-dir (let ((dir (file-name-directory (expand-file-name (or (buffer-file-name (current-buffer))
                                                                        default-directory))))
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

;;;; ------------------------------- git-whatsnew -------------------------------

(defvar git-diff-map
  (let ((map (make-sparse-keymap 'git-diff-map)))
    (define-key map [?g] 'diff-goto-source)
    (define-key map [(control return)] 'diff-goto-source)
    
    (define-key map [?j] 'diff-hunk-next)
    (define-key map [?k] 'diff-hunk-prev)
    (define-key map [?n] 'diff-hunk-next)
    (define-key map [?p] 'diff-hunk-prev)
    (define-key map [?f] 'diff-file-next)
    (define-key map [?J] 'diff-file-next)
    (define-key map [?K] 'diff-file-prev)

    (define-key map [?s] 'diff-split-hunk)
    (define-key map [?d] 'diff-hunk-kill)
    (define-key map [?D] 'diff-file-kill)

    (define-key map [remap undo] 'diff-undo)
    (define-key map [remap undo-tree-undo] 'diff-undo)
    (define-key map [remap self-insert] 'undefined)

    (define-key map [?q] 'git-quit-current)
    map)
  "Keymap for navigating diff buffers")


(defvar git-whatsnew-map
  (let ((map (make-sparse-keymap 'git-whatsnew-map)))
    (set-keymap-parent map git-diff-map)
    (define-key map [(control ?c) (control ?a)] 'git-amend-from-whatsnew)
    (define-key map [(control ?c) (control ?s)] 'git-stage-from-whatsnew)
    (define-key map [(control ?c) (control ?c)] 'git-commit-from-whatsnew)
    (define-key map [(control ?c) (control ?r)] 'git-revert-from-whatsnew)
    (define-key map [(control ?x) ?#] 'git-commit-from-whatsnew)
    map)
  "Keymap for git-whatsnew-mode")

(defun git-whatsnew-mode ()
  (unless (eq major-mode 'git-whatsnew)
    ;; Don't kill locals if we're already in whatsnew-mode
    (kill-all-local-variables)
    (diff-mode))
  (setq major-mode 'git-whatsnew)
  (setq mode-name "git-whatsnew")
  (use-local-map git-whatsnew-map)
  (set (make-local-variable 'revert-buffer-function)
       (lambda (ignore-auto noconfirm)
         (git-whatsnew t)))
  (setq buffer-read-only t)
  (setq minor-mode-overriding-map-alist
        (delq (assoc 'buffer-read-only minor-mode-overriding-map-alist)
              minor-mode-overriding-map-alist)))

(defun git-whatsnew (&optional same-window)
  "Prints a list of all the changes in the current repo, and
allows some or all of the changes to be staged and/or committed."
  (interactive)
  (let ((inhibit-read-only t))
    (git-command-window 'whatsnew same-window)
    (erase-buffer)
    (call-process "git" nil (current-buffer) nil "diff"))
  (git-whatsnew-mode)
  (if (= (point-min) (point-max))
    (let ((inhibit-read-only t))
      (insert "No changes.")))
  (goto-char (point-min)))

(defun git-apply-buffer-diff (&rest options)
  "Call git apply on the current buffer's diff with OPTIONS."
  (let ((patchfile (make-temp-file "gitchum-patch.diff.")))
    (write-region nil nil patchfile)
    (if (zerop (apply 'git-sync-command nil "apply" (append options (list patchfile))))
      (delete-file patchfile))))

(defun git-revert-from-whatsnew ()
  "Revert the changes in the current buffer in the working tree."
  (interactive)
  (when (yes-or-no-p "Do you really want to revert these changes? ")
    (git-apply-buffer-diff "--reverse")
    (git-whatsnew t)))

(defun git-commit-from-whatsnew ()
  "Apply the changes in the current whatsnew window to the index
  and open a commit dialogue buffer."
  (interactive)
  (git-apply-buffer-diff "--cached")
  (git-commit t))

(defun git-amend-from-whatsnew ()
  "Apply the changes in the current whatsnew window to the most
  recent commit."
  (interactive)
  (git-apply-buffer-diff "--cached")
  (git-commit t t))

(defun git-stage-from-whatsnew ()
  "Apply the changes in the current whatsnew window to the index and refresh."
  (interactive)
  (git-apply-buffer-diff "--cached")
  (git-whatsnew t))

;;;; ------------------------------------- git-staged ------------------------------------

(defvar git-staged-map
  (let ((map (make-sparse-keymap 'git-staged-map)))
    (set-keymap-parent map git-diff-map)
    (define-key map [(control ?c) (control ?a)] 'git-amend-from-staged)
    (define-key map [(control ?c) (control ?c)] 'git-commit-from-staged)
    (define-key map [(control ?c) (control ?r)] 'git-unstage)
    (define-key map [(control ?c) (control ?u)] 'git-unstage)
    (define-key map [(control ?x) ?#] 'git-commit-from-staged)
    map)
  "Keymap for git-staged-mode")

(defun git-staged-mode ()
  (unless (eq major-mode 'git-staged)
    ;; Don't kill locals if we're already in status-mode
    (kill-all-local-variables)
    (diff-mode))
  (setq major-mode 'git-staged)
  (setq mode-name "git-staged")
  (use-local-map git-staged-map)
  (set (make-local-variable 'revert-buffer-function)
       (lambda (ignore-auto noconfirm)
         (git-staged t)))
  (setq buffer-read-only t)
  (setq minor-mode-overriding-map-alist
        (delq (assoc 'buffer-read-only minor-mode-overriding-map-alist)
              minor-mode-overriding-map-alist)))

(defun git-staged (&optional same-window)
  "Prints a list of all the currently staged changes in the current repo, and
allows some or all of the changes to be committed and/or reverted."
  (interactive)
  (let ((inhibit-read-only t))
    (git-command-window 'staged same-window)
    (erase-buffer)
    (call-process "git" nil (current-buffer) nil "diff" "--cached"))
  (git-staged-mode)
  (if (= (point-min) (point-max))
    (let ((inhibit-read-only t))
      (insert "No changes.")))
  (goto-char (point-min)))

(defun git-unstage ()
  "Remove the changes in the current buffer from the index."
  (interactive)
  (git-apply-buffer-diff "--cached" "--reverse")
  (git-staged t))

(defun git-commit-from-staged ()
  (interactive)
  (git-commit t))

(defun git-amend-from-staged ()
  (interactive)
  (git-commit t t))

;;;; -------------------------------------- git-diff -------------------------------------

;;TODO just do git-whatsnew on the current file
(defun git-diff (&optional same-window)
  "Shows the current state of the repository, according to the index."
  (interactive)
  (require 'ansi-color)
  (let ((filename (buffer-file-name (current-buffer))))
    (git-command-window 'diff same-window)
    (let ((inhibit-read-only t)
          (lines-left 0))
      (erase-buffer)
      (call-process "git" nil (current-buffer) nil "diff" "HEAD" "--color" "--color-words" "--" filename)
      (ansi-color-apply-on-region (point-min) (point-max))
      (goto-char (point-min)))))

;;;; ------------------------------------- git-ediff -------------------------------------

(defun git-ediff ()
  (interactive)
  (require 'ediff)
  (require 'vc)
  (require 'vc-git)
  (let ((filename (buffer-file-name (current-buffer))))
    (vc-switch-backend filename 'git)
    (ediff-load-version-control)
    (funcall
     (intern (format "ediff-%S-internal" ediff-version-control-package))
     "" "" nil)))

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
    (define-key map [?q] 'git-quit-current)
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

;;;; -------------------------------- git-commit-msg-mode --------------------------------

(defvar git-commit-msg-overlay nil)
(defvar git-commit-amend-p nil)

(defun git-commit-msg-mode ()
  "Major mode for editing git commit messages."
  (unless (eq major-mode 'git-commit-msg)
    (kill-all-local-variables))
  (setq font-lock-defaults '((diff-font-lock-keywords) t))
  (setq major-mode 'git-commit-msg)
  (setq mode-name "git-commit-msg")
  (use-local-map git-commit-msg-map)
  (set (make-local-variable 'git-commit-amend-p) nil)
  (turn-on-font-lock)

  ;; Protect the boilerplate
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#" nil t)
      (goto-char (point-at-bol))
      (put-text-property (point) (point-max) 'read-only t))

  ;; Set up the message overlay
    (unless git-commit-msg-overlay
      (set (make-local-variable 'git-commit-msg-overlay)
           (make-overlay (point-min) (point) nil nil t)))
    (move-overlay git-commit-msg-overlay (point-min) (point))
    (overlay-put git-commit-msg-overlay 'face 'git-commit-msg)))


(defvar git-commit-msg-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?c) (control ?c)] 'server-edit)
    map)
  "Add a `C-c C-c' binding.")

(defvar git-commit-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?c) (control ?c)] 'git-commit-execute)
    (define-key map [remap server-edit] 'git-commit-execute)    
    map)
  "The `C-c C-c' binding points to `git-commit-execute'
  instead of `server-edit', and all other `server-edit' bindings
  are also replaced.")

(defvar git-commit-buffer-instructions
  "
# Please enter the commit message for your changes.
# (Comment lines starting with '#' will not be included)
#\\<git-commit-map>
# Type \\[git-commit-execute] to commit the staged changes.
# Type \\[kill-buffer] to abandon this commit buffer.
#
# The current status is listed below.
#
")

(defun git-commit-insert-instructions (amendp)
  "Insert a message template for the next commit."
  (let ((p nil))
    (if amendp
      (progn
        (call-process "git" nil (current-buffer) nil "show" "--format=%B" "--quiet")
        (insert (substitute-command-keys git-commit-buffer-instructions))
        (setq p (point))
        (call-process "git" nil (current-buffer) nil "commit" "--dry-run" "--amend" "--verbose"))
      (insert (substitute-command-keys git-commit-buffer-instructions))
      (setq p (point))
      (call-process "git" nil (current-buffer) nil "commit" "--dry-run" "--verbose"))
    (goto-char p)
    (when (re-search-forward "^diff --git" nil t)
      (goto-char (point-at-bol))
      (forward-line -1)
      (string-rectangle p (point) "# "))))

(defun git-commit (&optional same-window amendp msg)
  "Commit the currently staged patches."
  (interactive)
  (git-command-window 'commit same-window)
  (toggle-read-only 0)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (git-commit-insert-instructions amendp)
    (goto-char (point-min)))
  (git-commit-msg-mode)
  (use-local-map git-commit-map)
  (setq git-commit-amend-p amendp))

(defun git-commit-execute ()
  "Commit the currently-staged patches using a message from the current commit buffer."
  (interactive)
  (message "git commit")
  (if git-commit-amend-p
    (git-sync-command (current-buffer) "commit" "--amend" "-m" (git-commit-message))
    (git-sync-command (current-buffer) "commit" "-m" (git-commit-message)))
  ;; If we return to a whatsnew or status window, refresh it
  (when (find major-mode '(git-whatsnew git-staged))
    (revert-buffer t t t)))

(defun git-commit-message ()
  "Return the commit message from the current buffer"
  (git-trim-newlines
   (buffer-substring (overlay-start git-commit-msg-overlay)
                     (overlay-end git-commit-msg-overlay))))

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

(defun git-add (&optional prefix)
  (interactive "P")
  (if prefix
    (git-sync-command nil "add" "-f" (file-truename (buffer-file-name)))
    (git-sync-command nil "add" (file-truename (buffer-file-name)))))

(defun git-remove ()
  (interactive)
  (git-sync-command nil "rm" buffer-file-truename))

;;;; ====================================== git process interaction =====================================

(defun git-sync-command (killable-buffer &rest args)
  "Run `git ARGS` synchronously.  Prints output as a message; kills KILLABLE-BUFFER on success if non-nil."
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
  

;;;; ============================================ From xdarcs ===========================================

(defun git-trim-newlines (text)
  "Trims leading and trailing newlines from TEXT"
  ;; TODO The flagrant inefficiency of this function makes baby Jesus cry.
  (while (and (> (length text) 0)
              (eq ?\n (aref text 0)))
    (setq text (substring text 1)))
  (while (and (> (length text) 0)
              (eq ?\n (aref text (- (length text) 1))))
    (setq text (substring text 0 (- (length text) 1))))
  text)

(defun git-quit-current ()
  "Hide the current buffer"
  (interactive)
  (if (one-window-p)
    (bury-buffer)
    (bury-buffer)
    (delete-window)))

(provide 'gitchum)

