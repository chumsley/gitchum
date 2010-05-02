;; Value of `font-lock-keywords' in diff-mode.
;; Still not sure what the T represents.
(t
 (("\\(^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\) @@\\)\\(.*\\)$"
   (1 diff-hunk-header-face)
   (6 diff-function-face))
  ("^\\(\\*\\{15\\}\\)\\(.*\\)$"
   (1 diff-hunk-header-face)
   (2 diff-function-face))
  ("^\\*\\*\\* .+ \\*\\*\\*\\*" . diff-hunk-header-face)
  ("^--- .+ ----$" . diff-hunk-header-face)
  ("^[0-9,]+[acd][0-9,]+$" . diff-hunk-header-face)
  ("^---$" . diff-hunk-header-face)
  ("^\\(---\\|\\+\\+\\+\\|\\*\\*\\*\\) \\([^	\n]+?\\)\\(?:	.*\\| \\(\\*\\*\\*\\*\\|----\\)\\)?\n"
   (0 diff-header-face)
   (2
    (if
        (not
         (match-end 3))
      diff-file-header-face)
    prepend))
  ("^\\([-<]\\)\\(.*\n\\)"
   (1 diff-indicator-removed-face)
   (2 diff-removed-face))
  ("^\\([+>]\\)\\(.*\n\\)"
   (1 diff-indicator-added-face)
   (2 diff-added-face))
  ("^\\(!\\)\\(.*\n\\)"
   (1 diff-indicator-changed-face)
   (2 diff-changed-face))
  ("^Index: \\(.+\\).*\n"
   (0 diff-header-face)
   (1 diff-index-face prepend))
  ("^Only in .*\n" . diff-nonexistent-face)
  ("^\\(#\\)\\(.*\\)"
   (1 font-lock-comment-delimiter-face)
   (2 font-lock-comment-face))
  ("^[^-=+*!<>#].*\n"
   (0 diff-context-face)))
 ("\\(^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\) @@\\)\\(.*\\)$"
  (1 diff-hunk-header-face)
  (6 diff-function-face))
 ("^\\(\\*\\{15\\}\\)\\(.*\\)$"
  (1 diff-hunk-header-face)
  (2 diff-function-face))
 ("^\\*\\*\\* .+ \\*\\*\\*\\*"
  (0 diff-hunk-header-face))
 ("^--- .+ ----$"
  (0 diff-hunk-header-face))
 ("^[0-9,]+[acd][0-9,]+$"
  (0 diff-hunk-header-face))
 ("^---$"
  (0 diff-hunk-header-face))
 ("^\\(---\\|\\+\\+\\+\\|\\*\\*\\*\\) \\([^	\n]+?\\)\\(?:	.*\\| \\(\\*\\*\\*\\*\\|----\\)\\)?\n"
  (0 diff-header-face)
  (2
   (if
       (not
        (match-end 3))
     diff-file-header-face)
   prepend))
 ("^\\([-<]\\)\\(.*\n\\)"
  (1 diff-indicator-removed-face)
  (2 diff-removed-face))
 ("^\\([+>]\\)\\(.*\n\\)"
  (1 diff-indicator-added-face)
  (2 diff-added-face))
 ("^\\(!\\)\\(.*\n\\)"
  (1 diff-indicator-changed-face)
  (2 diff-changed-face))
 ("^Index: \\(.+\\).*\n"
  (0 diff-header-face)
  (1 diff-index-face prepend))
 ("^Only in .*\n"
  (0 diff-nonexistent-face))
 ("^\\(#\\)\\(.*\\)"
  (1 font-lock-comment-delimiter-face)
  (2 font-lock-comment-face))
 ("^[^-=+*!<>#].*\n"
  (0 diff-context-face)))