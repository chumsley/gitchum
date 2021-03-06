;;; gitchum.el --- A set of commands for managing git repositories from emacs.

;; Copyright (C) 2010-2019 James Wright

;; Author: James Wright <james@chumsley.org>
;; Version: 1.3
;; Created: 28 Apr 2010
;; Keywords: git
;; Package-Requires: (ediff vc ansi-color)

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
;; Gitchum is a set of commands for managing git repositories from emacs.
;;
;; It focuses on managing the staging area and working tree at the
;; level of patches. It is intended to be a replacement/extension of
;; the command line cherry picking interface.

;; To get started, load `gitchum.el`.  Visit a file in a git
;; repository and make some changes.  Then type `C-x g w` or `M-x
;; git-whatsnew`.  A buffer will appear containing a diff-like view
;; of your changes.  You can _stage_ the contents of the buffer by
;; typing `C-c C-s`, or stage and commit in a single step by typing
;; `C-c C-c`.  If you wish to stage only some of the changes in the
;; whatsnew buffer, delete the patches that you wish to omit as
;; follows. Navigate to them using `j` and `k` (or `n` and `p`), and
;; then type `d` to delete the patch.  (This differs from the usual
;; `diff-mode` binding of `k`, but I really like my j/k navigation.)
;;
;; In addition, the following is supported:
;;
;;   * View staged changes using `git-staged`.  From this buffer you
;;     can edit the buffer to contain the patches you wish to
;;     operate on (as in the whatsnew buffer), and then:
;;     - Unstage them using `C-c C-u`, or
;;     - Commit them using `C-c C-c`.
;;
;;   * Revert working tree changes from the whatsnew buffer using
;;     `C-c C-r`.
;;
;;   * Amend the previous commit using `C-c C-a` from either the
;;     whatsnew buffer or the staged buffer.
;;
;; I have tested gitchum with emacs version 24.1.
;; The latest version is available for download at
;; <http://chumsley.org/downloads/gitchum.el>.

;;
;; Still TODO
;; - factor major-modes, since they all kinda look the same?
;; - factor the git-whatsnew boilerplate for deleting the window etc.?
;;
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

(defcustom git-binary-extensions '("pdf" "key"
                                   "jpg" "jpeg" "png")
  "Extensions to treat as binary files"
  :group 'git
  :type '(repeat string))
                                   


;;;; ============================================== Keymaps =============================================

;;;; ----------------------------------- global keymap -----------------------------------

(defvar git-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?a] 'git-add)
    (define-key map [?b] 'git-blame)
   (define-key map [?l] 'git-log)
    (define-key map [?=] 'git-diff)
    (define-key map [?-] 'git-ediff)
;    (define-key map [??] 'git-describe-bindings)
    (define-key map [?d] 'git-describe-commit)
    (define-key map [?f] 'git-filelog)
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

;;;; ============================================== Aliases =============================================

;; `vc-annotate' is so good, there's no reason to reinvent it
(defalias 'git-blame 'vc-annotate)

;;;; ============================================= Commands =============================================

(defun git-command-window (name same-window &optional filename)
  "Switch to a readonly git command window with the
  default-directory set to the repo root.  Contents and local
  variables might be leftover from previous
  instances."
  (let ((repo-dir (git-repo-dir)))
    (unless repo-dir
      (error "No git repo at or around '%s'"
             (file-name-directory (expand-file-name (or (buffer-file-name (current-buffer))
                                                        default-directory)))))
    (when filename
      (setq name (format "%s [%s]" name (file-name-nondirectory filename))))
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

(defvar git-whatsnew-target-file nil)

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
         (git-whatsnew t git-whatsnew-target-file)))
  (setq buffer-read-only t)
  (setq minor-mode-overriding-map-alist
        (delq (assoc 'buffer-read-only minor-mode-overriding-map-alist)
              minor-mode-overriding-map-alist))

  ;; local to allow navigation in binary hunks
  (set (make-local-variable 'diff-file-header-re)
       (concat "^Binary files .* differ$\\|" diff-file-header-re))
  (set (make-local-variable 'diff-hunk-header-re)
       (concat "^Binary files .* differ$\\|" diff-hunk-header-re)))
                            

(defun git-whatsnew (&optional same-window filename)
  "Prints a list of all the changes in the current repo, and
allows some or all of the changes to be staged and/or committed."
  (interactive)
  (let ((inhibit-read-only t))
    (git-command-window 'whatsnew same-window filename)
    (erase-buffer)
    (git-buffer-command "diff" filename))
  (git-whatsnew-mode)
  (set (make-local-variable 'git-whatsnew-target-file) filename)
  (if (= (point-min) (point-max))
    (let ((inhibit-read-only t))
      (insert "No changes.")))
  (goto-char (point-min))
  (git-diff-flag-binaries))

(defun git-apply-buffer-diff (&rest options)
  "Call git apply on the current buffer's diff with OPTIONS."
  (let ((patchfile (make-temp-file "gitchum-patch.diff.")))
    (write-region nil nil patchfile)
    (if (zerop (apply 'git-sync-command nil "apply" (append '("--ignore-whitespace") options (list patchfile))))
      (delete-file patchfile))))

(defun git-revert-from-whatsnew ()
  "Revert the changes in the current buffer in the working tree."
  (interactive)
  (when (yes-or-no-p "Do you really want to revert these changes? ")
    (git-restore-binaries)
    (unless (= (point-min) (point-max))
      (git-apply-buffer-diff "--reverse"))
    (git-whatsnew t git-whatsnew-target-file)))

(defun git-commit-from-whatsnew ()
  "Apply the changes in the current whatsnew window to the index
  and open a commit dialogue buffer."
  (interactive)
  (git-stage-binaries)
  (unless (= (point-min) (point-max))
    (git-apply-buffer-diff "--cached"))
  (git-commit nil t))

(defun git-amend-from-whatsnew ()
  "Apply the changes in the current whatsnew window to the most
  recent commit."
  (interactive)
  (if (git-amend-safe-p)
    (progn
      (git-stage-binaries)
      (unless (= (point-min) (point-max))
        (git-apply-buffer-diff "--cached"))
      (git-commit nil t t))
    (message "git-amend safety check failed!\nHEAD has been pushed to upstream.")))

(defun git-stage-from-whatsnew ()
  "Apply the changes in the current whatsnew window to the index and refresh."
  (interactive)
  (git-stage-binaries)
  (unless (= (point-min) (point-max))
    (git-apply-buffer-diff "--cached"))
  (git-whatsnew t git-whatsnew-target-file))


;;;; ------- binary file handling

(defvar git-binary-diff-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map git-diff-map)
    (define-key map [?d] 'git-binary-kill)
    (define-key map [?D] 'git-binary-kill)
    (define-key map [?s] 'git-binary-no-split)
    map))

(defun git-binary-kill ()
  (interactive)
  (when (get-char-property (point) 'binary-file)
    (let ((inhibit-read-only t)
          (s (or (previous-single-property-change (point) 'binary-file)
                 (point-min)))
          (e (or (next-single-property-change (point) 'binary-file)
                 (point-max))))
      (kill-region s e))))

(defun git-binary-no-split ()
  (interactive)
  (user-error "Splitting binary files is not supported"))

(defun git-diff-flag-binaries ()
  (save-excursion
    (goto-char (point-min))
    (let ((fs nil)
          (current-fname nil)
          (current-binary nil)
          (diff-re "^diff --git a/\\(.*\\) b/\\(.*\\)$")
          (binary-re "^Binary files .* differ$"))
      (labels ((flush-binary-properties ()
                 (let ((inhibit-read-only t))
                   (add-text-properties fs (point)
                                        (list 'face 'highlight
                                              'binary-file current-fname
                                              'keymap git-binary-diff-map))))
               (handle-diff-line ()
                 (when current-binary
                   (flush-binary-properties))
                 (setq fs (match-beginning 0)
                       current-binary nil
                       current-fname (match-string 1))
                 (when (member (file-name-extension current-fname)
                               git-binary-extensions)
                   (setq current-binary t))))             

        (when (looking-at diff-re)
          (handle-diff-line))

        (while (zerop (forward-line))
          (cond
            ((looking-at binary-re)
             (setq current-binary t))
            ((looking-at diff-re)             
             (handle-diff-line))))        

        (when current-binary
          (flush-binary-properties))))))

(defun map-and-kill-binary-files (fn)
  "Apply FN to each binary file in the buffer, killing each file
  after it is processed.  FN will be passed the name of the
  file."
  (flet ((maybe-handle-file ()
           (let ((fname (get-char-property (point) 'binary-file)))
             (when fname
               (funcall fn fname)
               (git-binary-kill)
               t))))
    (save-excursion
      (goto-char (point-min))
      (while (or (maybe-handle-file)
                 (zerop (forward-line)))))))

(defun git-stage-binaries ()
  "Apply binary 'diffs' by adding entire files.  Binary 'diffs'
  are removed after processing."
  (map-and-kill-binary-files
   (lambda (fname)
     (git-sync-command nil "add" (file-truename fname)))))

(defun git-restore-binaries ()
  "Revert binary 'diffs' by restoring entire files.  Binary 'diffs'
  are removed after processing."
  (map-and-kill-binary-files
   (lambda (fname)
     (git-sync-command nil "restore" (file-truename fname)))))

;;TODO handle stage-binary-files, commit-binary-files, and revert-binary-files

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
    (git-buffer-command "diff" "--cached"))
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
  (git-commit nil t))

(defun git-amend-from-staged ()
  (interactive)
  (if (git-amend-safe-p)
    (git-commit nil t t)
    (message "git-amend safety check failed!\nHEAD has been pushed to upstream.")))

;;;; -------------------------------------- git-diff -------------------------------------

(defun git-diff (&optional same-window)
  "Show changes for the current file only, and allow staging."
  (interactive)
  (unless (buffer-file-name)
    (error "No file associated with buffer"))
  (git-whatsnew nil (buffer-file-name)))

;;;; ------------------------------------- git-ediff -------------------------------------

(defun git-ediff (&optional revision)
  (interactive
   (if current-prefix-arg
       (list (read-string "Compare to version: ")) ;TODO history, completion of versions, etc.
     (list nil)))
  (require 'ediff)
  (require 'vc)
  (require 'vc-git)
  (let ((filename (buffer-file-name (current-buffer))))
    (unless filename
      (error "No file associated with buffer"))
    (vc-switch-backend filename 'git)
    (ediff-load-version-control)
    (funcall
     (intern (format "ediff-%S-internal" ediff-version-control-package))
     (or revision "") ; latest version
     "" ; current buffer
     nil)))

;;;; --------------------------------- git-query-manifest --------------------------------

(defun git-query-manifest (&optional same-window)
  "Shows the current state of the repository, according to the index."
  (interactive)
  (git-command-window 'manifest same-window)
  (let ((inhibit-read-only t)
        (lines-left 0))
    (erase-buffer)
    (git-buffer-command "ls-files" "--stage" "--full-name")
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
(defvar git-commit-all-p nil)

(defun git-commit-msg-mode ()
  "Major mode for editing git commit messages."
  (unless (eq major-mode 'git-commit-msg)
    (kill-all-local-variables))
  (setq font-lock-defaults '((diff-font-lock-keywords) t))
  (setq major-mode 'git-commit-msg)
  (setq mode-name "git-commit-msg")
  (use-local-map git-commit-msg-map)
  (set (make-local-variable 'git-commit-amend-p) nil)
  (set (make-local-variable 'git-commit-all-p) nil)
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

(defun git-commit-insert-instructions (amendp allp)
  "Insert a message template for the next commit."
  (let ((p nil))
    (if amendp
      (progn
        (git-buffer-command "show" "--format=%B" "--quiet")
        (insert (substitute-command-keys git-commit-buffer-instructions))
        (setq p (point))
        (git-buffer-command "commit" "--dry-run" (when allp "-a")  "--amend" "--verbose"))
      (progn
        (insert (substitute-command-keys git-commit-buffer-instructions))
        (setq p (point))
        (git-buffer-command "commit" "--dry-run" (when allp "-a") "--verbose" "--untracked-files=no")))
    (goto-char p)
    (when (re-search-forward "^Untracked files not listed" nil t)
      (delete-region (point-at-bol) (point-at-eol))
      (delete-char 1))
    (goto-char p)
    (when (re-search-forward "^diff --git" nil t)
      (goto-char (point-at-bol))
      (forward-line -1)
      (string-rectangle p (point) "# "))))

(defun git-commit (&optional allp same-window amendp msg)
  "Commit the currently staged patches."
  (interactive "P")
  (git-command-window 'commit same-window)
  (toggle-read-only 0)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (git-commit-insert-instructions amendp allp)
    (goto-char (point-min)))
  (git-commit-msg-mode)
  (use-local-map git-commit-map)
  (setq git-commit-amend-p amendp
        git-commit-all-p allp))

(defun git-amend-safe-p ()
  "Return non-NIL if it is safe to amend the HEAD commit.
'Safe' means either there is no upstream branch, or we are ahead
of the upstream branch."
  (let* ((raw (git-sync-internal "rev-list" "@{u}..@"))
         (ret (car raw))
         (out (cdr raw)))
    (cond
      ((and (zerop ret) (zerop (length out)))
       ;; Up to date with upstream branch
       nil)
      ((zerop ret)
       ;; Ahead of upstream branch
       t)
      (t
       ;; No upstream branch
       t))))
    
  
(defun git-commit-execute ()
  "Commit the currently-staged patches using a message from the current commit buffer."
  (interactive)
  (message "git commit")
  (cond
    ((and git-commit-amend-p
          (git-amend-safe-p))
     (git-sync-command (current-buffer) "commit" "--amend" "-m" (git-commit-message)))
    (git-commit-amend-p
     (message "%s" "git-amend safety check failed!\nHEAD has been pushed to upstream."))
    (t
     (git-sync-command (current-buffer) "commit" (when git-commit-all-p "-a") "-m" (git-commit-message))))

  ;; If we return to a whatsnew or status window, refresh it
  (when (find major-mode '(git-whatsnew git-staged))
    (revert-buffer t t t)))

(defun git-commit-message ()
  "Return the commit message from the current buffer"
  (git-trim-newlines
   (buffer-substring (overlay-start git-commit-msg-overlay)
                     (overlay-end git-commit-msg-overlay))))

;;;; -------------------------------------- git-log --------------------------------------

(defvar git-log-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?q] 'git-quit-current)
    (define-key map [?j] 'git-log-next-commit)
    (define-key map [?k] 'git-log-prev-commit)
    (define-key map [(control ?c) (control ?c)] 'git-log-checkout)
    map))

(defun git-log-checkout (&optional master)
  "Checkout the commit on the current line, or 'master' if prefix
  argument"
  (interactive "P")  
  (if master
      (and (git-sync-command nil "checkout" "master")
           (revert-buffer t t t))
    (let ((commit (get-text-property (point-at-bol) 'commit-hash)))
      (if commit
          (and (git-sync-command nil "checkout" commit)
               (revert-buffer t t t))
        (user-error "No commit hash at current line")))))


(defun git-log-next-commit (&optional delta)
  (interactive)
  (setq delta (or delta 1))
  (goto-char (point-at-bol))
  (while (and (zerop (forward-line delta))
              (not (looking-at ".*[0-9a-f]+")))))

(defun git-log-prev-commit ()
  (interactive)
  (git-log-next-commit -1))

(defvar git-log-filename nil
  "Filename being logged in the current window.")

(defun git-log (&optional same-window filename no-branch-graph)
  "Shows the activity log for the current repo."
  (interactive (list nil nil current-prefix-arg))
  (require 'ansi-color)
  (let ((inhibit-read-only t))
    (git-command-window 'log same-window filename)
    (erase-buffer)
    (if no-branch-graph
      (git-buffer-command "log" "--color=always" "--pretty=format:%C(blue)%h%C(reset) %C(dim green)%cd%C(reset) %<(12)%C(dim white)%an%C(reset) - %s%C(red)%d%C(reset)"
                          "--abbrev-commit" "--date=short" (when filename "--follow") "--" filename)
    
      (git-buffer-command "log" "--color=always" "--graph" "--pretty=format:%C(blue)%h%C(reset) %C(dim green)%cd%C(reset) %<(12)%C(dim white)%an%C(reset) - %s%C(red)%d%C(reset)"
                          "--abbrev-commit" "--date=short" "--all" "--" filename))
    (ansi-color-apply-on-region (point-min) (point-max))
    (goto-char (point-min))
    (git-log-add-properties))
  (git-log-mode))

(defun git-log-mode ()
  (unless (eq major-mode 'git-log)
    ;; Don't kill locals if we're already in log-mode
    (kill-all-local-variables)
    (diff-mode))
  (setq major-mode 'git-log)
  (setq mode-name "git-log")
  (use-local-map git-log-map)
  (set (make-local-variable 'revert-buffer-function)
       (lambda (ignore-auto noconfirm)
         (git-log t git-log-filename)))
  (setq buffer-read-only t)
  (setq minor-mode-overriding-map-alist
        (delq (assoc 'buffer-read-only minor-mode-overriding-map-alist)
              minor-mode-overriding-map-alist)))

(defun git-filelog (&optional same-window branch-graph)
  "Shows the activity log for the current file."
  (interactive (list nil current-prefix-arg))
  (require 'ansi-color)
  (git-log same-window (buffer-file-name) (not branch-graph)))

(defvar git-describe-commit-property-map
  (let ((map (make-sparse-keymap 'git-describe-commit-property-map)))
    (define-key map '[return] 'git-describe-commit-at-point)
    (define-key map '[mouse-1] 'git-describe-commit-from-event)
    map))

(defun git-log-add-properties ()
  (save-excursion
    (goto-char (point-min))
    (let ((remain 0)
          (inhibit-read-only t))
      (while (zerop remain)
        (goto-char (point-at-bol))
        (let ((p (re-search-forward "[0-9a-f]+" nil t)))
          (when p
            (setq p (point))
            (forward-word -1)
            (put-text-property (point-at-bol) p 'mouse-face 'highlight)
            (put-text-property (point-at-bol) p 'keymap git-describe-commit-property-map)
            (put-text-property (point-at-bol) p 'commit-hash (buffer-substring-no-properties (point) p))))
        (goto-char (point-at-eol))
        (setq remain (forward-line))))))            

(defun git-describe-commit-from-event (event)
  (interactive "e")
  (goto-char (second (second event)))
  (git-describe-commit-at-point))

(defun git-describe-commit-at-point ()
  (interactive)
  (if (get-text-property (point) 'commit-hash)
      (git-describe-commit t (get-text-property (point) 'commit-hash))
    (user-error "No commit hash at point")))

;;;; --------------------------------- git-describe-commit --------------------------------

(defvar git-describe-commit-map
  (let ((map (make-sparse-keymap 'git-describe-commit-map)))
    (set-keymap-parent map git-diff-map)
    map))

(defun git-describe-commit-mode ()
  (unless (eq major-mode 'git-describe-commit)
    ;; Don't kill locals if we're already in describe-commit-mode
    (kill-all-local-variables)
    (diff-mode))
  (setq major-mode 'git-describe-commit)
  (setq mode-name "git-describe-commit")
  (use-local-map git-describe-commit-map)
  (setq buffer-read-only t)
  (setq minor-mode-overriding-map-alist
        (delq (assoc 'buffer-read-only minor-mode-overriding-map-alist)
              minor-mode-overriding-map-alist)))

(defun git-describe-commit (&optional same-window commit-hash)
  "Describe COMMIT-HASH"
  (interactive (list t (read-string "Describe commit: ")))
  (git-command-window 'describe-commit same-window commit-hash)
  (use-local-map git-describe-commit-map)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (git-buffer-command "show" "--date=local" commit-hash))
  (git-describe-commit-mode)
  (goto-char (point-min)))

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


;;;; ============================================= Mode-line ============================================

(defvar git-watched-repos nil
  "Git-dirs being watched")

(defvar git-mode-line nil
  "The git branch for the mode-line")
(make-variable-buffer-local 'git-mode-line)

(defun git-install-mode-line ()
  (add-hook 'find-file-hook 'git-resync-mode-line)
  (add-hook 'after-save-hook 'git-resync-mode-line)
  (add-hook 'after-revert-hook 'git-resync-mode-line))

(defun git-buffer-killed-callback ()
  (let ((buffer-count 0)
        (repo-dir (when (buffer-file-name) (git-repo-dir))))
    (when repo-dir
      (map-repo-buffers
       (lambda ()
         (setq buffer-count (+ buffer-count 1))))
      (when (<= buffer-count 1)
        (let ((cell (assoc repo-dir git-watched-repos)))
          (when cell
            (file-notify-rm-watch (cdr cell))
            (setq git-watched-repos (remove (assoc repo-dir git-watched-repos)
                                            git-watched-repos))))))))

(defun map-repo-buffers (fn &optional file-name)
  "Call FN with current buffer set to every buffer that shares a
  git repo with FILE-NAME (or with the current buffer if
  FILE-NAME is NIL)."
  (let* ((default-directory
          (if file-name (file-name-directory file-name) default-directory))
         (repo-dir (git-repo-dir)))
    (mapc (lambda (buffer)
            (when (buffer-file-name buffer)
              (with-current-buffer buffer
                (when (equal (git-repo-dir) repo-dir)
                  (funcall fn)))))
          (buffer-list))))

(defun git-gitdir-changed-callback (event)
  (git-resync-mode-line (third event)))

(defun git-resync-mode-line (&optional file-name)
  "Update the mode-line in all buffers in the same repository as
  FILE-NAME.  If FILE-NAME, is not provided, then use the file of
  the current buffer."
  (let* ((default-directory
          (if file-name (file-name-directory file-name) default-directory))
         (repo-dir (git-repo-dir)))
    (when repo-dir
      (let* ((default-directory repo-dir)
             (git-dir (concat repo-dir ".git"))
             (ret (git-mode-line)))
        ;; First make sure we're watching the .git dir for changes
        (unless (assoc repo-dir git-watched-repos)
          (push (cons repo-dir
                      (file-notify-add-watch git-dir '(change) 'git-gitdir-changed-callback))
                git-watched-repos))

        ;; Then set the git modeline
        (map-repo-buffers
         (lambda ()
           ;; Hook into kill-buffer so we can remove the watch if necessary
           (add-hook 'kill-buffer-hook 'git-buffer-killed-callback)

           ;; Actually update the modeline
           (setq git-mode-line
                 (when ret
                   (concat "[" ret "]")))))))))

(defun git-mode-line (&optional sep)
  "Return a string to insert into the mode-line.
The output is a simplified version of the output given by the code at 
<https://github.com/git/git/blob/master/contrib/completion/git-prompt.sh>.

The branch name is given if possible, followed by a separator, followed by
  * if there are unstaged changes
  + if there are staged but uncommitted changes
  $ if there are stashed changes

Empty repositories (which should display '#') are not yet handled.
Upstream comparison (ahead/behind/diverged from upstream) is not yet suppported.
Merge and rebase descriptions are significantly less detailed than the git-prompt versions.
"
  (unless sep
    (setq sep ":"))
  (let ((rev-out (git-sync-internal "rev-parse"
                                    "--git-dir"
                                    "--is-inside-git-dir" "--is-bare-repository" "--is-inside-work-tree"
                                    "--short" "HEAD")))
    (when (zerop (car rev-out))
      (let* ((vals (split-string (cdr rev-out)))
             (git-dir (nth 0 vals))
             (in-gitdir (string-equal (nth 1 vals) "true"))
             (is-bare (string-equal (nth 2 vals) "true"))
             (in-work (string-equal (nth 3 vals) "true"))
             (short-sha (nth 4 vals))

             (head-vals (git-sync-internal "symbolic-ref" "HEAD"))
             (detachedp (not (zerop (car head-vals))))
             (symbolic-head (car (last (split-string (cdr head-vals) "/"))))
             
             (c "")
             (b "")
             (r ""))
             
        (cond
          ((or (file-exists-p (concat git-dir "/rebase-merge")) ; git-prompt.sh goes into a lot more detail about rebasing
               (file-exists-p (concat git-dir "/rebase-apply")))
           (setq r "|REBASE"))
          ((file-exists-p (concat git-dir "/MERGE_HEAD"))
           (setq r "|MERGING"))
          ((file-exists-p (concat git-dir "/CHERRY_PICK_HEAD"))
           (setq r "|CHERRY-PICKING"))
          ((file-exists-p (concat git-dir "/REVERT_HEAD"))
           (setq r "|REVERTING"))
          ((file-exists-p (concat git-dir "/BISECT_LOG"))
           (setq r "|BISECTING")))
        (if detachedp
          (setq b (concat "(" short-sha "...)"))
          (setq b symbolic-head))
        (cond
          (is-bare (setq c "BARE:"))
          (in-gitdir (setq b "GIT_DIR!")))
        (when in-work
          (when (not (zerop (car (git-sync-internal "diff" "--no-ext-diff" "--quiet" "--exit-code"))))
            (setq b (concat b sep "*"))
            (setq sep ""))
          (when (not (zerop (car (git-sync-internal "diff-index" "--cached" "--quiet" "HEAD"))))
            (setq b (concat b sep "+"))
            (setq sep ""))
          (when (file-readable-p (concat git-dir "/refs/stash"))
            (setq b (concat b sep "$"))
            (setq sep "")))
        (concat c b r)))))

;;;; ====================================== git process interaction =====================================

(defun git-sync-internal (&rest args)
  "Run `git ARGS` synchronously, and return a pair of (RET . OUTPUT)."
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
      (cons ret (buffer-substring (point-min) (point-max))))))

(defun git-sync-command (killable-buffer &rest args)
  "Run `git ARGS` synchronously.  Prints output as a message; kills KILLABLE-BUFFER on success if non-nil."
  (let* ((raw (apply 'git-sync-internal (remove nil args)))
         (ret (car raw))
         (out (cdr raw)))
    (message "%s" out)
    (when (and (zerop ret)
               killable-buffer)
      (kill-buffer killable-buffer))
    ret))

(defun git-buffer-command (&rest args)
  "Run `git ARGS` asynchronously, with the output going to the
  current buffer.  Any NIL args will be removed."
  (apply 'call-process "git" nil (current-buffer) nil
         (remove nil args)))

(defun git-repo-dir ()
  "Get the repo directory around the current directory."
  (let ((repo-dir (locate-dominating-file default-directory ".git")))
    (when repo-dir
      (abbreviate-file-name (file-name-directory (file-truename repo-dir))))))



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

;;; gitchum.el ends here
