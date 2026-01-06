;;; rust-indent-fix.el --- Fix rust-mode indentation for brace-binding patterns -*- lexical-binding: t -*-

;;; Commentary:
;;
;; rust-mode's `rust-align-to-expr-after-brace' uses `forward-word' to find
;; alignment targets, but `forward-word' skips punctuation. This causes
;; indentation bugs that this package fixes:
;;
;; 1. In `{ let foo = ...' or `{ if let Some(x) = ...', continuation lines
;;    align to `let'/`if' instead of being indented by rust-indent-offset.
;;
;; 2. In `(( { ... }, { ...', the second `{' aligns to content inside the
;;    first block instead of to the first `{'.
;;
;; 3. In `{ expr (...', content inside the paren aligns to `expr' instead
;;    of being indented by rust-indent-offset beyond `expr'.
;;
;; 4. Closing `}' of a `{ let }' block misaligns with its opening `{'.

;;; Code:

(require 'rust-mode)

(defcustom rust-indent-fix-enabled t
  "When non-nil, apply indentation fixes for brace-binding patterns."
  :type 'boolean
  :group 'rust-mode)

(defvar rust-indent-fix--recursion-guard nil
  "Prevents infinite recursion when our advice calls the original indent function.")

(defun rust-indent-fix--inside-brace-binding-block-p ()
  "Return t if point's immediately enclosing bracket is `{ let' or `{ if let'."
  (save-excursion
    (when (> (rust-paren-level) 0)
      (ignore-errors
        (backward-up-list)
        (looking-at "{[[:space:]]*\\(if[[:space:]]+\\)?let\\b")))))

(defun rust-indent-fix--at-block-result-expression-p ()
  "Return t if current line is the result expression (after the let's semicolon)."
  (save-excursion
    (forward-line -1)
    (end-of-line)
    (skip-chars-backward "[:space:]")
    (eq (char-before) ?\;)))

(defun rust-indent-fix--brace-column-when-first-in-paren ()
  "If enclosing paren/bracket starts with `{', return the `{' column, else nil.
This detects patterns like `( { let' or `[ { let'."
  (save-excursion
    (when (> (rust-paren-level) 0)
      (ignore-errors
        (backward-up-list)
        (when (looking-at "[][()]")
          (forward-char)
          (skip-chars-forward "[:space:]")
          (when (eq (char-after) ?{)
            (current-column)))))))

(defun rust-indent-fix--line-starts-with-closing-brace-p ()
  "Return t if current line starts with `}'."
  (save-excursion
    (back-to-indentation)
    (eq (char-after) ?})))

(defun rust-indent-fix--opening-brace-column ()
  "Return the column of the opening `{' for a line starting with `}'."
  (save-excursion
    (back-to-indentation)
    (when (eq (char-after) ?})
      (ignore-errors
        (forward-char)
        (backward-sexp)
        (current-column)))))

(defun rust-indent-fix--enclosing-brace-column ()
  "Return the column of the enclosing `{' bracket."
  (save-excursion
    (when (> (rust-paren-level) 0)
      (ignore-errors
        (backward-up-list)
        (when (eq (char-after) ?{)
          (current-column))))))

(defun rust-indent-fix--brace-binding-continuation-indent (rust-mode-indent)
  "Indent for continuation lines inside `{ let' or `{ if let' blocks.
RUST-MODE-INDENT is what rust-mode calculated. Returns corrected indent."
  (cond
   ;; Line starts with } - align with opening {
   ((rust-indent-fix--line-starts-with-closing-brace-p)
    (rust-indent-fix--opening-brace-column))
   ;; Result expression (after semicolon) - align with opening {
   ((rust-indent-fix--at-block-result-expression-p)
    (rust-indent-fix--enclosing-brace-column))
   ;; Continuation line - add offset
   (t
    (+ rust-mode-indent rust-indent-offset))))

(defun rust-indent-fix--paren-containing-brace-indent (rust-mode-indent)
  "Indent for lines inside `( {' or `[ {' where rust-mode over-indented.
RUST-MODE-INDENT is what rust-mode calculated. Returns the brace column
if rust-mode aligned to content after the brace instead of the brace itself."
  (let ((brace-column (rust-indent-fix--brace-column-when-first-in-paren)))
    (when (and brace-column (> rust-mode-indent brace-column))
      brace-column)))

(defun rust-indent-fix--inside-paren-on-brace-line-p ()
  "Return t if inside a paren/bracket chain that includes a brace-line paren.
This detects patterns like `{ expr (' where content inside the paren
needs extra indentation. Also handles nested parens where an ancestor
paren is on a brace line."
  (save-excursion
    (when (> (rust-paren-level) 0)
      (ignore-errors
        (backward-up-list)
        ;; Only applies inside parens/brackets, not braces
        (when (looking-at "[][()]")
          ;; Check if this paren's line or any ancestor paren's line starts with {
          (let ((found nil))
            (while (and (not found) (> (rust-paren-level) 0))
              (back-to-indentation)
              (if (eq (char-after) ?{)
                  (setq found t)
                (backward-up-list)))
            found))))))

(defun rust-indent-fix--paren-on-brace-line-indent (rust-mode-indent)
  "Indent for lines inside a paren whose line starts with `{'.
RUST-MODE-INDENT is what rust-mode calculated. Returns indent + offset."
  (+ rust-mode-indent rust-indent-offset))

(defun rust-indent-fix--calculate-corrected-indentation ()
  "Return corrected indentation for current line, or nil if no correction needed."
  (save-excursion
    (back-to-indentation)
    (let ((rust-mode-indent (current-column)))
      (cond
       ;; Case 1: Inside { let } or { if let } block
       ((rust-indent-fix--inside-brace-binding-block-p)
        (rust-indent-fix--brace-binding-continuation-indent rust-mode-indent))
       ;; Case 2: Inside ( { ... or [ { ... - align to the {
       ((rust-indent-fix--paren-containing-brace-indent rust-mode-indent))
       ;; Case 3: Inside paren on a line starting with { - add offset
       ((rust-indent-fix--inside-paren-on-brace-line-p)
        (rust-indent-fix--paren-on-brace-line-indent rust-mode-indent))))))

(defun rust-indent-fix--indent-line-advice (orig-fun &rest args)
  "Advice wrapping `rust-mode--indent-line' to apply our indentation fixes."
  (if rust-indent-fix--recursion-guard
      (apply orig-fun args)
    (let ((rust-indent-fix--recursion-guard t))
      (apply orig-fun args)
      (when rust-indent-fix-enabled
        (let ((corrected (rust-indent-fix--calculate-corrected-indentation)))
          (when corrected
            (indent-line-to corrected)))))))

;;;###autoload
(defun rust-indent-fix-enable ()
  "Enable the rust-indent-fix advice."
  (interactive)
  (advice-add 'rust-mode--indent-line :around #'rust-indent-fix--indent-line-advice)
  (message "rust-indent-fix enabled"))

;;;###autoload
(defun rust-indent-fix-disable ()
  "Disable the rust-indent-fix advice."
  (interactive)
  (advice-remove 'rust-mode--indent-line #'rust-indent-fix--indent-line-advice)
  (message "rust-indent-fix disabled"))

(provide 'rust-indent-fix)
;;; rust-indent-fix.el ends here
