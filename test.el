;;; test.el --- Tests for rust-indent-fix -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Run with: emacs --batch -l test.el
;; Exit code 0 = all tests passed, 1 = some tests failed

;;; Code:

(add-to-list 'load-path "/home/ubuntu/rust-indent")
(require 'rust-mode)
(require 'rust-prog-mode)
(require 'rust-indent-fix)

(setq rust-indent-offset 2)

(defvar test-all-passed t)

(defun test-indent (code line-num expected description)
  "Test that LINE-NUM in CODE indents to EXPECTED with fix enabled."
  (rust-indent-fix-enable)
  (with-temp-buffer
    (rust-mode)
    (insert code)
    (goto-char (point-min))
    (forward-line (1- line-num))
    (let ((original (current-indentation)))
      (funcall indent-line-function)
      (let* ((result (current-indentation))
             (pass (= result expected)))
        (unless pass (setq test-all-passed nil))
        (message "  %s: %d -> %d (want %d) %s"
                 description original result expected
                 (if pass "PASS" "FAIL"))
        pass))))

;;; ============================================================
;;; Issue 1: { let continuation indentation
;;; ============================================================

(defvar test-brace-let-code "
fn example() {
    Ok (( { let visible : HashSet < ID > =
          subscribee_content . iter ()
          . filter ( | id | ! subscriber_hides . contains ( id ) )
          . cloned () . collect ();
          visible },
       other_stuff ))
}
")

;; Line numbers (1-indexed):
;; 1: empty
;; 2: fn example() {
;; 3:     Ok (( { let visible...  <- { at col 10, let at col 12
;; 4:           subscribee_content...  <- should be 14 (let-col + offset)
;; 5:           . filter...
;; 6:           . cloned...
;; 7:           visible },  <- result expr, should be 12 (let-col)
;; 8:        other_stuff ))

(message "")
(message "=== Issue 1: { let continuation indentation ===")
(message "  (rust-indent-offset = %d)" rust-indent-offset)
(test-indent test-brace-let-code 4 14 "subscribee_content (continuation)")
(test-indent test-brace-let-code 5 14 ".filter (continuation)")
(test-indent test-brace-let-code 6 14 ".cloned (continuation)")
(test-indent test-brace-let-code 7 12 "visible (result expression)")

;;; ============================================================
;;; Issue 1b: { if let continuation indentation
;;; ============================================================

(defvar test-brace-if-let-code "
fn example() {
    child_treeids }
    { if let Some ( child_mut )
      = node_ref . tree () . get_mut ( child_treeid ) {
        add_missing_info_dfs ( child_mut ); }} }}
}
")

;; Line numbers (1-indexed):
;; 1: empty
;; 2: fn example() {
;; 3:     child_treeids }
;; 4:     { if let Some...  <- { at col 4, if at col 6
;; 5:       = node_ref...   <- should be 8 (if-col + offset)

(message "")
(message "=== Issue 1b: { if let continuation indentation ===")
(test-indent test-brace-if-let-code 5 8 "= node_ref (continuation)")

;;; ============================================================
;;; Issue 2: Tuple element alignment
;;; ============================================================

(defvar test-tuple-align-code "
fn example() {
  Ok (( { let visible : HashSet < ID > =
            subscribee_content . iter ()
            . filter ( | id | ! subscriber_hides . contains ( id ) )
            . cloned () . collect ();
          visible },
        { let hidden : HashSet < ID > =
            subscribee_content . iter ()
            . filter ( | id | subscriber_hides . contains ( id ) )
            . cloned () . collect ();
          hidden } )) }
}
")

;; Line numbers (1-indexed):
;; 1: empty
;; 2: fn example() {
;; 3:   Ok (( { let visible...  <- first { at col 8
;; 4-7: ...
;; 8:         { let hidden...   <- should align to col 8 (same as first {)

(message "")
(message "=== Issue 2: Tuple element alignment ===")
(test-indent test-tuple-align-code 8 8 "second { let (should align to first {)")

;;; ============================================================
;;; Issue 3: { expr ( paren content indentation
;;; ============================================================

(defvar test-brace-expr-paren-code "
fn example() {
    { errors.push (
      BufferValidationError::Multiple (
        orgnode.clone() )); }}
}
")

;; Line numbers (1-indexed):
;; 1: empty
;; 2: fn example() {
;; 3:     { errors.push (  <- { at col 4, ( at end of line
;; 4:       BufferValidationError...  <- should be 8 (6 + offset)
;; 5:         orgnode.clone...  <- should be 10 (8 + offset)

(message "")
(message "=== Issue 3: { expr ( paren content indentation ===")
(test-indent test-brace-expr-paren-code 4 8 "BufferValidationError (inside paren on brace line)")
(test-indent test-brace-expr-paren-code 5 10 "orgnode.clone (nested paren)")

;;; ============================================================
;;; Issue 4: Closing brace alignment
;;; ============================================================

(defvar test-closing-brace-code "
fn example() {
  { // comment
    if { let count: usize =
           node_ref . count();
         count
    } > 1
    { errors.push ( x ); }}
}
")

;; Line numbers (1-indexed):
;; 1: empty
;; 2: fn example() {
;; 3:   { // comment
;; 4:     if { let count...  <- { at col 7
;; 5:            node_ref...
;; 6:          count
;; 7:     } > 1  <- } should align with { at col 7

(message "")
(message "=== Issue 4: Closing brace alignment ===")
(test-indent test-closing-brace-code 7 7 "} > 1 (closing brace aligns with opening)")

;;; ============================================================
;;; Issue 5: Result expression alignment (nested)
;;; ============================================================

(defvar test-result-expr-code "
fn example() {
  triples.extend(
    { let node_triples : Vec<Triple> =
        saveinstructions(
          { let orgnode: &OrgNode = node_ref.value();
            orgnode },
            config, driver).await?;
      node_triples } );
}
")

;; Line numbers (1-indexed):
;; 1: empty
;; 2: fn example() {
;; 3:   triples.extend(
;; 4:     { let node_triples...  <- { at col 4, let at col 6
;; 5-8: ...
;; 9:       node_triples }  <- result expr should be 6 (let-col)

(message "")
(message "=== Issue 5: Result expression alignment ===")
(test-indent test-result-expr-code 9 6 "node_triples (result aligns with let)")

;;; ============================================================
;;; Issue 6: Compact { let x = ...; result } patterns
;;; ============================================================

(defvar test-compact-brace-let-code "
for edge in forest.root().traverse() {
  if let ego_tree::iter::Edge::Open(node_ref) = edge {
    triples.extend(
      { let node_triples : Vec<MergeInstructionTriple> =
          saveinstructions_from_the_merge_in_an_orgnode(
            { let orgnode: &OrgNode = node_ref.value();
            orgnode },
            config, driver).await?;
      node_triples } ); } }
")

;; Line numbers (1-indexed):
;; 1: empty
;; 2: for edge...
;; 3:   if let...
;; 4:     triples.extend(
;; 5:       { let node_triples...  <- { at col 6, let at col 8
;; 6:           saveinstructions(
;; 7:               { let orgnode...  <- should be 12 (not 14, don't add extra indent for `{')
;; 8:               orgnode },  <- compact pattern: should be 16 (let-col 14 + offset)
;; 9:               config, driver...
;; 10:       node_triples }  <- multi-line: should be 8 (let-col)

(message "")
(message "=== Issue 6: Compact { let x = ...; result } patterns ===")
(test-indent test-compact-brace-let-code 7 12 "{ let orgnode (inner brace block)")
(test-indent test-compact-brace-let-code 8 16 "orgnode (compact result, let-col + offset)")
(test-indent test-compact-brace-let-code 10 8 "node_triples (multi-line result, let-col)")

;;; ============================================================
;;; Issue 7: Punctuation in first paren argument
;;; ============================================================

(defvar test-punctuation-arg-code "
f( &x,
y,
z )
")

;; Line numbers (1-indexed):
;; 1: empty
;; 2: f( &x,  <- & at col 3
;; 3: y,      <- should be 3 (align with &, not with x)

(message "")
(message "=== Issue 7: Punctuation in first paren argument ===")
(test-indent test-punctuation-arg-code 3 3 "y after f(&x, (align with &)")

;;; ============================================================
;;; Issue 8: Blank line resets top-level indent
;;; ============================================================

(defvar test-blank-line-code "
f( x,
   &y,
   z)

f( &x,
   y )
")

;; Line numbers (1-indexed):
;; 1: empty
;; 2-4: first f(...)
;; 5: blank line
;; 6: f( &x,  <- should be 0 (reset after blank line)

(message "")
(message "=== Issue 8: Blank line resets top-level indent ===")
(test-indent test-blank-line-code 6 0 "f( after blank line (reset to 0)")

;;; ============================================================
;;; Issue 9: ( expr ( paren content indentation
;;; ============================================================

(defvar test-paren-expr-paren-code "
fn example() {
let alias_col_id : Option<NodeId> =
  ( unique_orgnode_child_with_interp (
    tree, node_id, Interp::AliasCol )
    . map_err ( |e| e.to_string() ) ? );
}
")

;; Line numbers (1-indexed):
;; 1: empty
;; 2: fn example() {
;; 3: let alias_col_id...
;; 4:   ( unique_orgnode_child_with_interp (  <- outer ( at col 2, inner ( at end
;; 5:     tree, node_id...  <- should be 6 (4 + offset), not 4
;; 6:     . map_err...

(message "")
(message "=== Issue 9: ( expr ( paren content indentation ===")
(test-indent test-paren-expr-paren-code 5 6 "tree (inside nested paren on paren line)")

;; Non-regression: simple ( x, y ) should NOT add extra indent
(defvar test-simple-paren-code "
( x,
y )
")

;; y should align with x at col 2, not col 4
(test-indent test-simple-paren-code 3 2 "y in ( x, y ) (no extra indent)")

;;; ============================================================
;;; Issue 10: Struct literal field alignment
;;; ============================================================

(defvar test-struct-literal-code "
fn example() {
    parent_mut . append ( NodePair { mskgnode: Some(skgnode),
                                     orgnode: new_orgnode } )
}
")

;; Line numbers (1-indexed):
;; 1: empty
;; 2: fn example() {
;; 3:     parent_mut ... { mskgnode: ...  <- mskgnode at col 37
;; 4:                                     orgnode: ...  <- should be 37, not 39

(message "")
(message "=== Issue 10: Struct literal field alignment ===")
(test-indent test-struct-literal-code 4 37 "orgnode (struct field aligns with mskgnode)")

;;; ============================================================
;;; Issue 11: Brace blocks with inline content (match arms)
;;; ============================================================

(defvar test-match-arm-code1 "
fn example() {
    { Ok(Some(id)) if &id == target_skgid
      => return Ok(true),
      Ok(_) => continue, }}
")

(defvar test-match-arm-code2 "
fn example() {
    { Ok(Some(id)) if &id == target_skgid =>
      return Ok(true),
      Ok(_) => continue, }}
")

;; Both cases: { at col 4, continuation should be at col 8

(message "")
(message "=== Issue 11: Brace blocks with inline content ===")
(test-indent test-match-arm-code1 4 8 "=> on second line (match arm)")
(test-indent test-match-arm-code2 4 8 "body on second line (match arm)")

;;; ============================================================
;;; Summary
;;; ============================================================

(message "")
(if test-all-passed
    (progn
      (message "=== ALL TESTS PASSED ===")
      (kill-emacs 0))
  (progn
    (message "=== SOME TESTS FAILED ===")
    (kill-emacs 1)))

;;; test.el ends here
