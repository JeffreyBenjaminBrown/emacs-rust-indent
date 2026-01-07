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

(defun rust-indent-fix--enclosing-brace-has-inline-content-p ()
  "Return t if enclosing `{' has non-trivial content on the same line.
This detects patterns like `{ Ok(Some(x))' or `{ expr' where the brace
is followed by code on the same line (not just whitespace or comments).
Returns nil for struct literals like `{ field: value, ...' since fields
should align, not get extra indentation."
  (save-excursion
    (when (> (rust-paren-level) 0)
      (ignore-errors
        (backward-up-list)
        (when (eq (char-after) ?{)
          (forward-char)
          (skip-chars-forward "[:space:]")
          ;; Check if there's non-comment content before end of line,
          ;; but NOT a struct literal field (identifier followed by :)
          (and (not (eolp))
               (not (looking-at "//"))
               (not (looking-at "[a-zA-Z_][a-zA-Z0-9_]*[[:space:]]*:"))))))))

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

(defun rust-indent-fix--previous-line-is-blank-p ()
  "Return t if previous line is blank or only whitespace."
  (save-excursion
    (forward-line -1)
    (looking-at "^[[:space:]]*$")))

(defun rust-indent-fix--enclosing-paren-first-content-column ()
  "Return column of first non-whitespace content after enclosing `(' or `['.
Returns nil if not inside a paren/bracket."
  (save-excursion
    (when (> (rust-paren-level) 0)
      (ignore-errors
        (backward-up-list)
        (when (looking-at "[][()]")
          (forward-char)
          (skip-chars-forward "[:space:]\n")
          (current-column))))))

(defun rust-indent-fix--any-enclosing-paren-on-brace-line-p ()
  "Return t if any enclosing paren/bracket is on a line that starts with `{',
or starts with `(' or `[' with inline content (where the starting bracket
is different from the one we're inside).
Walks up through parens/brackets only, stopping when a brace is encountered."
  (save-excursion
    (let ((found nil))
      (while (and (not found) (> (rust-paren-level) 0))
        (ignore-errors
          (backward-up-list)
          (if (looking-at "[][()]")
              ;; It's a paren/bracket - check if its line starts with a bracket
              (let ((paren-pos (point)))
                (save-excursion
                  (back-to-indentation)
                  (cond
                   ;; Line starts with { - always triggers the fix
                   ((eq (char-after) ?{)
                    (setq found t))
                   ;; Line starts with ( or [ - only triggers if:
                   ;; 1. The bracket at start of line is NOT the one we just found
                   ;; 2. There's inline content after the bracket
                   ((and (looking-at "[][()]")
                         (not (= (point) paren-pos)))
                    (forward-char)
                    (skip-chars-forward "[:space:]")
                    (when (not (eolp))
                      (setq found t))))))
            ;; It's a brace - stop walking up the chain
            (setq found 'stop))))
      (eq found t))))

(defun rust-indent-fix--indent-for-lambda-continuation ()
  "Calculate indent for a lambda continuation.
Returns (indent . body-on-next-line-p), or nil if not in a lambda context.
Excludes lambdas with brace bodies (those should follow brace rules).
Handles both inline body (`|x| expr`) and body on next line (`|x|\\nexpr`).
For lambdas at column 0 with inline body, aligns with body + offset."
  (save-excursion
    (let ((current-line (line-number-at-pos))
          (result nil))
      (when (> (rust-paren-level) 0)
        (ignore-errors
          (backward-up-list)
          (when (looking-at "[][()]")
            (forward-line 1)
            (while (and (not result) (< (line-number-at-pos) current-line))
              (beginning-of-line)
              (cond
               ;; Case 1: |...| followed by non-whitespace that's NOT {
               ;; (lambda with inline body, but not brace body)
               ((re-search-forward "|[^|\n]*|[[:space:]]*\\([^{[:space:]\n]\\)" (line-end-position) t)
                (let* ((body-start (match-beginning 1))
                       (lambda-col (save-excursion
                                     (back-to-indentation)
                                     (current-column)))
                       (body-col (save-excursion
                                   (goto-char body-start)
                                   (current-column))))
                  ;; If lambda is at col 0, use body_col + offset for better alignment
                  ;; Otherwise, use lambda_col + offset
                  ;; Return cons with nil to indicate inline body
                  (if (= lambda-col 0)
                      (setq result (cons (+ body-col rust-indent-offset) nil))
                    (setq result (cons (+ lambda-col rust-indent-offset) nil)))))
               ;; Case 2: |...| at end of line (body on next line)
               ;; Body gets 2x offset from lambda for visual distinction
               ((save-excursion
                  (beginning-of-line)
                  (re-search-forward "|[^|\n]*|[[:space:]]*$" (line-end-position) t))
                (let ((lambda-col (save-excursion
                                    (back-to-indentation)
                                    (current-column))))
                  ;; Return cons with t to indicate body on next line
                  (setq result (cons (+ lambda-col (* 2 rust-indent-offset)) t)))))
              (forward-line 1)))))
      result)))

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

(defun rust-indent-fix--indent-for-paren-with-punctuation-arg (rust-mode-calculated-indent)
  "Correct indent when paren's first arg starts with punctuation like `&'.
RUST-MODE-CALCULATED-INDENT is what rust-mode computed.
Returns corrected column or nil if no correction needed."
  (let ((first-content-col (rust-indent-fix--enclosing-paren-first-content-column)))
    (when (and first-content-col
               (> rust-mode-calculated-indent first-content-col))
      first-content-col)))

(defun rust-indent-fix--indent-after-blank-line ()
  "Return 0 if previous line is blank and we're at top level, else nil."
  (when (and (rust-indent-fix--previous-line-is-blank-p)
             (= (rust-paren-level) 0))
    0))

(defun rust-indent-fix--indent-for-brace-with-inline-content (rust-mode-calculated-indent)
  "Calculate indent for continuation inside `{ content' blocks.
RUST-MODE-CALCULATED-INDENT is what rust-mode computed.
This handles patterns like `{ Ok(Some(x))' in match arms."
  (+ rust-mode-calculated-indent rust-indent-offset))

;;;; Main Dispatch

(defun rust-indent-fix--calculate-corrected-indentation ()
  "Return corrected indentation for current line, or nil if no correction needed."
  (save-excursion
    (back-to-indentation)
    (let ((rust-mode-calculated-indent (current-column)))
      (cond
       ;; After blank line at top level, reset to column 0
       ((rust-indent-fix--indent-after-blank-line))

       ((rust-indent-fix--enclosing-bracket-is-brace-let-p)
        (rust-indent-fix--indent-for-brace-binding-block rust-mode-calculated-indent))

       ((rust-indent-fix--indent-for-paren-starting-with-brace rust-mode-calculated-indent))

       ;; Don't apply "paren on brace line" fix if line starts with `{'.
       ;; A line starting with `{' is a new brace block, not content to be indented.
       ((and (rust-indent-fix--any-enclosing-paren-on-brace-line-p)
             (not (rust-indent-fix--line-starts-with-open-brace-p)))
        (rust-indent-fix--indent-for-paren-on-brace-line rust-mode-calculated-indent))

       ;; Lambda with inline content: |...| expr on a line means continuations
       ;; should be indented beyond the lambda start.
       ;; For body-on-next-line lambdas, body already gets 2x offset, so
       ;; continuations stay at that level.
       ((let ((lambda-result (rust-indent-fix--indent-for-lambda-continuation)))
          (when lambda-result
            (car lambda-result))))

       ;; Fix alignment when first paren argument starts with punctuation like &
       ((rust-indent-fix--indent-for-paren-with-punctuation-arg rust-mode-calculated-indent))

       ;; Inside `{ content' blocks (like match arms), add extra indentation
       ;; for continuation lines (but not lines starting with `}' or new statements after `;')
       ((and (rust-indent-fix--enclosing-brace-has-inline-content-p)
             (not (rust-indent-fix--line-starts-with-closing-brace-p))
             (not (rust-indent-fix--previous-line-ends-with-semicolon-p)))
        (rust-indent-fix--indent-for-brace-with-inline-content rust-mode-calculated-indent))))))

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
