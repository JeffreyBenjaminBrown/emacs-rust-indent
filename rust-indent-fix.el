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
;; 4. Closing `}' and result expressions of `{ let }' blocks misalign with
;;    the opening `{'.

;;; Code:

(require 'rust-mode)

;;;; Configuration

(defcustom rust-indent-fix-enabled t
  "When non-nil, apply indentation fixes for brace-binding patterns."
  :type 'boolean
  :group 'rust-mode)

(defvar rust-indent-fix--recursion-guard nil
  "Prevents infinite recursion when our advice calls the original indent function.")

;;;; Navigation Helpers

(defun rust-indent-fix--enclosing-bracket-column ()
  "Return column of the immediately enclosing bracket, or nil if at top level."
  (save-excursion
    (when (> (rust-paren-level) 0)
      (ignore-errors
        (backward-up-list)
        (current-column)))))

(defun rust-indent-fix--enclosing-brace-column ()
  "Return column of enclosing `{', or nil if enclosing bracket isn't `{'."
  (save-excursion
    (when (> (rust-paren-level) 0)
      (ignore-errors
        (backward-up-list)
        (when (eq (char-after) ?{)
          (current-column))))))

(defun rust-indent-fix--matching-open-brace-column ()
  "If line starts with `}', return column of its matching `{'. Else nil."
  (save-excursion
    (back-to-indentation)
    (when (eq (char-after) ?})
      (ignore-errors
        (forward-char)
        (backward-sexp)
        (current-column)))))

;;;; Line Classification Predicates

(defun rust-indent-fix--line-starts-with-closing-brace-p ()
  "Return t if current line's first non-whitespace char is `}'."
  (save-excursion
    (back-to-indentation)
    (eq (char-after) ?})))

(defun rust-indent-fix--line-starts-with-open-brace-p ()
  "Return t if current line's first non-whitespace char is `{'."
  (save-excursion
    (back-to-indentation)
    (eq (char-after) ?{)))

(defun rust-indent-fix--previous-line-ends-with-semicolon-p ()
  "Return t if previous line ends with `;' (ignoring trailing whitespace)."
  (save-excursion
    (forward-line -1)
    (end-of-line)
    (skip-chars-backward "[:space:]")
    (eq (char-before) ?\;)))

(defun rust-indent-fix--previous-line-is-compact-brace-let-p ()
  "Return t if previous line is a compact `{ let x = ...; or `{ if let x = ...;'.
This detects single-line definitions where the result should be indented further."
  (save-excursion
    (forward-line -1)
    (back-to-indentation)
    (looking-at "{[[:space:]]*\\(if[[:space:]]+\\)?let\\b")))

(defun rust-indent-fix--enclosing-bracket-is-brace-let-p ()
  "Return t if immediately enclosing bracket is `{ let' or `{ if let'."
  (save-excursion
    (when (> (rust-paren-level) 0)
      (ignore-errors
        (backward-up-list)
        (looking-at "{[[:space:]]*\\(if[[:space:]]+\\)?let\\b")))))

(defun rust-indent-fix--enclosing-paren-starts-with-brace-p ()
  "Return t if enclosing `(' or `[' has `{' as its first non-whitespace content."
  (save-excursion
    (when (> (rust-paren-level) 0)
      (ignore-errors
        (backward-up-list)
        (when (looking-at "[][()]")
          (forward-char)
          (skip-chars-forward "[:space:]")
          (eq (char-after) ?{))))))

(defun rust-indent-fix--any-enclosing-paren-on-brace-line-p ()
  "Return t if any enclosing paren/bracket is on a line that starts with `{'.
Walks up through parens/brackets only, stopping when a brace is encountered."
  (save-excursion
    (let ((found nil))
      (while (and (not found) (> (rust-paren-level) 0))
        (ignore-errors
          (backward-up-list)
          (if (looking-at "[][()]")
              ;; It's a paren/bracket - check if its line starts with {
              (save-excursion
                (back-to-indentation)
                (when (eq (char-after) ?{)
                  (setq found t)))
            ;; It's a brace - stop walking up the chain
            (setq found 'stop))))
      (eq found t))))

;;;; Indentation Calculation for Each Case

(defun rust-indent-fix--indent-for-brace-binding-block (rust-mode-calculated-indent)
  "Calculate indent for a line inside a `{ let }' or `{ if let }' block.
RUST-MODE-CALCULATED-INDENT is what rust-mode computed."
  (cond
   ((rust-indent-fix--line-starts-with-closing-brace-p)
    (rust-indent-fix--matching-open-brace-column))
   ((rust-indent-fix--previous-line-ends-with-semicolon-p)
    ;; Result expression: align with `let' keyword (brace-col + 2).
    ;; For compact patterns where `{ let x = ...; is on one line,
    ;; add another offset since the result is a continuation.
    (let ((let-column (+ (rust-indent-fix--enclosing-brace-column) 2)))
      (if (rust-indent-fix--previous-line-is-compact-brace-let-p)
          (+ let-column rust-indent-offset)
        let-column)))
   (t
    (+ rust-mode-calculated-indent rust-indent-offset))))

(defun rust-indent-fix--indent-for-paren-starting-with-brace (rust-mode-calculated-indent)
  "Calculate indent when inside `( {' or `[ {' - align to the `{'.
RUST-MODE-CALCULATED-INDENT is what rust-mode computed.
Returns corrected column or nil if no correction needed."
  (save-excursion
    (when (> (rust-paren-level) 0)
      (ignore-errors
        (backward-up-list)
        (when (looking-at "[][()]")
          (forward-char)
          (skip-chars-forward "[:space:]")
          (when (eq (char-after) ?{)
            (let ((brace-column (current-column)))
              (when (> rust-mode-calculated-indent brace-column)
                brace-column))))))))

(defun rust-indent-fix--indent-for-paren-on-brace-line (rust-mode-calculated-indent)
  "Calculate indent when inside a paren whose line starts with `{'.
RUST-MODE-CALCULATED-INDENT is what rust-mode computed."
  (+ rust-mode-calculated-indent rust-indent-offset))

;;;; Main Dispatch

(defun rust-indent-fix--calculate-corrected-indentation ()
  "Return corrected indentation for current line, or nil if no correction needed."
  (save-excursion
    (back-to-indentation)
    (let ((rust-mode-calculated-indent (current-column)))
      (cond
       ((rust-indent-fix--enclosing-bracket-is-brace-let-p)
        (rust-indent-fix--indent-for-brace-binding-block rust-mode-calculated-indent))

       ((rust-indent-fix--indent-for-paren-starting-with-brace rust-mode-calculated-indent))

       ;; Don't apply "paren on brace line" fix if line starts with `{'.
       ;; A line starting with `{' is a new brace block, not content to be indented.
       ((and (rust-indent-fix--any-enclosing-paren-on-brace-line-p)
             (not (rust-indent-fix--line-starts-with-open-brace-p)))
        (rust-indent-fix--indent-for-paren-on-brace-line rust-mode-calculated-indent))))))

;;;; Advice and Public Interface

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
