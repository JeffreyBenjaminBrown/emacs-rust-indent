;;; jbb-rust-indent.el --- Better indentation for rust-mode -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This package overrides rust-mode's indentation with a cleaner algorithm.
;; It hooks into rust-mode and replaces `indent-line-function`.
;;
;; Usage:
;;   (require 'jbb-rust-indent)
;;
;; The indentation offset is controlled by `rust-indent-offset' (from rust-mode).
;;
;; === Algorithm ===
;;
;; 1. Find the innermost unclosed bracket (paren, brace, or bracket)
;;
;; 2. If there IS content after the bracket on its line:
;;    → Align to that content (Case A)
;;
;; 3. If there is NO content after the bracket:
;;    → Find the "base" column and add offset (Case B)
;;    → Base is first text before the bracket, not past another opening brace
;;
;; === Brace vs Paren/Bracket ===
;;
;; Braces { } use semicolons as statement separators.
;; Parens ( ) and brackets [ ] use commas.
;; This affects handling after statement/element endings.

;;; Code:

(require 'cl-lib)

;; We'll use rust-indent-offset from rust-mode when available
(defvar rust-indent-offset)

(defun jbb-rust--indent-offset ()
  "Return the indentation offset to use."
  (if (boundp 'rust-indent-offset)
      rust-indent-offset
    4))

;;;; Indentation Helpers

(defun jbb-rust--bracket-depth ()
  "Return current bracket nesting depth."
  (nth 0 (syntax-ppss)))

(defun jbb-rust--innermost-bracket-pos ()
  "Return position of innermost unclosed bracket, or nil."
  (nth 1 (syntax-ppss)))

(defun jbb-rust--in-string-p ()
  "Return non-nil if point is inside a string."
  (nth 3 (syntax-ppss)))

(defun jbb-rust--in-comment-p ()
  "Return non-nil if point is inside a comment."
  (nth 4 (syntax-ppss)))

(defun jbb-rust--column-at (pos)
  "Return column at position POS."
  (save-excursion
    (goto-char pos)
    (current-column)))

(defun jbb-rust--prev-line-blank-p ()
  "Return non-nil if previous line is blank."
  (save-excursion
    (forward-line -1)
    (looking-at-p "^[[:space:]]*$")))

(defun jbb-rust--prev-line-indentation ()
  "Return indentation of previous non-blank line."
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp)) (looking-at-p "^[[:space:]]*$"))
      (forward-line -1))
    (current-indentation)))

(defun jbb-rust--prev-line-ends-semicolon-p ()
  "Return non-nil if previous non-blank line ends with semicolon."
  (save-excursion
    (forward-line -1)
    (end-of-line)
    (skip-chars-backward " \t")
    (eq (char-before) ?\;)))

(defun jbb-rust--prev-line-ends-semicolon-or-comma-p ()
  "Return non-nil if previous line ends with semicolon or comma."
  (save-excursion
    (forward-line -1)
    (end-of-line)
    (skip-chars-backward " \t")
    (memq (char-before) '(?\; ?,))))

(defun jbb-rust--first-content-after (pos)
  "Return column of first content after POS on same line, or nil if none.
Content excludes whitespace and comments."
  (save-excursion
    (goto-char pos)
    (forward-char)
    (skip-chars-forward " \t")
    (cond
     ((eolp) nil)
     ((looking-at-p "//") nil)
     (t (current-column)))))

(defun jbb-rust--matching-bracket-column ()
  "Return column of bracket matching the closing bracket at point.
If the opening bracket is at end of line (or followed only by comment),
return the line's indentation. Otherwise return the bracket's column."
  (save-excursion
    (back-to-indentation)
    (ignore-errors
      (forward-char)
      (backward-sexp)
      ;; Now at matching open bracket
      (let ((bracket-col (current-column)))
        (forward-char)
        (skip-chars-forward " \t")
        (if (or (eolp) (looking-at-p "//"))
            ;; Bracket at end of line - use line's indentation
            (progn (back-to-indentation) (current-column))
          ;; Bracket has content - use bracket's column
          bracket-col)))))

(defun jbb-rust--is-let-block-p (brace-pos)
  "Return non-nil if brace at BRACE-POS starts a { let } or { if let } block."
  (save-excursion
    (goto-char brace-pos)
    (when (eq (char-after) ?{)
      (forward-char)
      (skip-chars-forward " \t")
      (looking-at-p "\\(if[[:space:]]+\\)?let\\b"))))

(defun jbb-rust--let-block-base-column (brace-pos)
  "Return base column for { let } or { if let } block at BRACE-POS.
For { let }, returns `let' column. For { if let }, returns `if' column."
  (save-excursion
    (goto-char brace-pos)
    (forward-char)
    (skip-chars-forward " \t")
    (current-column)))

(defun jbb-rust--let-keyword-column (brace-pos)
  "Return column of `let' keyword in block starting at BRACE-POS.
For result expression alignment after semicolon."
  (save-excursion
    (goto-char brace-pos)
    (forward-char)
    (skip-chars-forward " \t")
    (when (looking-at "if[[:space:]]+")
      (goto-char (match-end 0)))
    (current-column)))

(defun jbb-rust--base-column-before (bracket-pos)
  "Find base column by looking backward from BRACKET-POS.
Look for first text after any preceding `{' on the same line.
If no preceding `{', use line indentation."
  (save-excursion
    (goto-char bracket-pos)
    (let* ((line-start (line-beginning-position))
           (start-from line-start))
      ;; Search backward for a { on this line
      (when (re-search-backward "{" line-start t)
        (setq start-from (1+ (point))))
      ;; Find first non-whitespace from start-from
      (goto-char start-from)
      (skip-chars-forward " \t")
      (if (< (point) bracket-pos)
          (current-column)
        ;; Nothing before bracket - use line's indentation
        (back-to-indentation)
        (current-column)))))

;;;; Lambda Detection

(defun jbb-rust--lambda-indent ()
  "Calculate lambda-based indent, or nil if not applicable.
Searches backward from current line for a lambda header."
  (save-excursion
    (let ((orig-line (line-number-at-pos))
          (bracket-pos (jbb-rust--innermost-bracket-pos)))
      (when bracket-pos
        (goto-char bracket-pos)
        (when (memq (char-after) '(?\( ?\[))
          ;; We're inside ( or [, search for lambda on preceding lines
          (forward-line 1)
          (catch 'found
            (while (< (line-number-at-pos) orig-line)
              (back-to-indentation)
              (cond
               ;; |...| with inline body (not brace)
               ((looking-at "|[^|\n]*|[ \t]*\\([^{ \t\n]\\)")
                (let ((body-col (save-excursion
                                  (goto-char (match-beginning 1))
                                  (current-column))))
                  (throw 'found body-col)))
               ;; |...| alone on line (body on next line)
               ((looking-at "|[^|\n]*|[ \t]*$")
                (let ((lambda-col (current-column)))
                  (throw 'found (+ lambda-col (jbb-rust--indent-offset)))))
               ;; |...| { - brace body, don't use lambda rules
               ((looking-at "|[^|\n]*|[ \t]*{")
                (throw 'found nil)))
              (forward-line 1))
            nil))))))

;;;; Main Indentation Logic

(defun jbb-rust--line-is-lambda-header-p ()
  "Return non-nil if current line is a lambda header |args|."
  (save-excursion
    (back-to-indentation)
    (looking-at "|[^|\n]*|")))

(defun jbb-rust--calculate-indent ()
  "Calculate proper indentation for current line."
  (save-excursion
    (back-to-indentation)
    (cond
     ;; In string: don't change
     ((jbb-rust--in-string-p)
      (current-indentation))

     ;; In comment: keep current
     ((jbb-rust--in-comment-p)
      (current-indentation))

     ;; Lambda header line: keep current indentation
     ((jbb-rust--line-is-lambda-header-p)
      (current-indentation))

     ;; Closing bracket: align to matching open
     ((looking-at-p "[])}]")
      (or (jbb-rust--matching-bracket-column) 0))

     ;; At top level with blank line before: column 0
     ((and (= (jbb-rust--bracket-depth) 0)
           (jbb-rust--prev-line-blank-p))
      0)

     ;; At top level but previous line is indented:
     ;; use previous line's indent (handles unbalanced braces in fragments)
     ((and (= (jbb-rust--bracket-depth) 0)
           (> (jbb-rust--prev-line-indentation) 0))
      (jbb-rust--prev-line-indentation))

     ;; At top level: column 0
     ((= (jbb-rust--bracket-depth) 0)
      0)

     ;; Inside brackets
     (t
      (jbb-rust--indent-inside-brackets)))))

(defun jbb-rust--indent-inside-brackets ()
  "Calculate indent for a line inside brackets."
  (let* ((bracket-pos (jbb-rust--innermost-bracket-pos))
         (bracket-char (char-after bracket-pos))
         (content-col (jbb-rust--first-content-after bracket-pos))
         (lambda-indent (jbb-rust--lambda-indent)))

    (cond
     ;; Lambda context takes precedence
     (lambda-indent lambda-indent)

     ;; Case A: Content follows bracket on same line
     (content-col
      (cond
       ;; Inside { let } or { if let } block
       ((and (eq bracket-char ?{)
             (jbb-rust--is-let-block-p bracket-pos))
        (if (jbb-rust--prev-line-ends-semicolon-p)
            (jbb-rust--let-keyword-column bracket-pos)
          (+ (jbb-rust--let-block-base-column bracket-pos)
             (jbb-rust--indent-offset))))

       ;; Inside { with other inline content (match arm, etc)
       ((eq bracket-char ?{)
        (if (jbb-rust--prev-line-ends-semicolon-or-comma-p)
            content-col
          (+ content-col (jbb-rust--indent-offset))))

       ;; Default: align to content
       (t content-col)))

     ;; Case B: Nothing after bracket - find base and add offset
     (t
      (jbb-rust--case-b-indent bracket-pos bracket-char)))))

(defun jbb-rust--case-b-indent (bracket-pos bracket-char)
  "Calculate indent for Case B (nothing after bracket).
BRACKET-POS is position of innermost bracket, BRACKET-CHAR its character."
  (cond
   ;; Inside a { - check for enclosing { let } context
   ((eq bracket-char ?{)
    (let ((outer-brace (jbb-rust--find-enclosing-let-block bracket-pos)))
      (if outer-brace
          (jbb-rust--let-block-base-column outer-brace)
        (+ (jbb-rust--base-column-before bracket-pos)
           (jbb-rust--indent-offset)))))
   ;; Other brackets: base + offset
   (t
    (+ (jbb-rust--base-column-before bracket-pos)
       (jbb-rust--indent-offset)))))

(defun jbb-rust--find-enclosing-let-block (inner-pos)
  "Find enclosing { let } or { if let } block containing INNER-POS.
Returns position of the enclosing brace, or nil if none."
  (save-excursion
    (goto-char inner-pos)
    (let ((ppss (syntax-ppss))
          (result nil))
      (dolist (pos (nth 9 ppss))
        (when (and (< pos inner-pos)
                   (eq (char-after pos) ?{)
                   (jbb-rust--is-let-block-p pos))
          (setq result pos)))
      result)))

;;;; Public Interface

(defun jbb-rust-indent-line ()
  "Indent current line as Rust code."
  (interactive)
  (let ((indent (jbb-rust--calculate-indent))
        (offset (- (current-column) (current-indentation))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0)
        (forward-char offset)))))

(defun jbb-rust-indent-setup ()
  "Set up jbb-rust indentation for the current buffer."
  (setq-local indent-line-function #'jbb-rust-indent-line)
  (setq-local indent-tabs-mode nil))

;;;###autoload
(defun jbb-rust-indent-enable ()
  "Enable jbb-rust-indent for all rust-mode buffers."
  (interactive)
  (add-hook 'rust-mode-hook #'jbb-rust-indent-setup)
  (add-hook 'rust-ts-mode-hook #'jbb-rust-indent-setup)
  (message "jbb-rust-indent enabled for rust-mode"))

;;;###autoload
(defun jbb-rust-indent-disable ()
  "Disable jbb-rust-indent for rust-mode buffers."
  (interactive)
  (remove-hook 'rust-mode-hook #'jbb-rust-indent-setup)
  (remove-hook 'rust-ts-mode-hook #'jbb-rust-indent-setup)
  (message "jbb-rust-indent disabled"))

;; Auto-enable when loaded
(jbb-rust-indent-enable)

(provide 'jbb-rust-indent)
;;; jbb-rust-indent.el ends here
