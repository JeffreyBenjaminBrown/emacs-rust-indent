;;; tests.el --- Auto-generated tests -*- lexical-binding: t -*-

(add-to-list 'load-path "/home/ubuntu/rust-indent")
(require 'rust-mode)
(require 'rust-prog-mode)
(require 'rust-indent-fix)

(setq rust-indent-offset 2)

(defvar test-all-passed t)
(defvar test-count 0)
(defvar test-passed-count 0)

(defun indent-code-string (code-string)
  "Indent CODE-STRING using rust-mode and return the result."
  (rust-indent-fix-enable)
  (with-temp-buffer
    (rust-mode)
    (insert code-string)
    (goto-char (point-min))
    (while (not (eobp))
      (funcall indent-line-function)
      (forward-line 1))
    (buffer-string)))

(defun run-test (name ugly-code pretty-code)
  "Run a single test."
  (setq test-count (1+ test-count))
  (let* ((result (indent-code-string ugly-code))
         (pass (string= result pretty-code)))
    (if pass
        (progn
          (setq test-passed-count (1+ test-passed-count))
          (message "PASS: %s" name))
      (setq test-all-passed nil)
      (message "FAIL: %s" name)
      (message "  Expected:")
      (dolist (line (split-string pretty-code "\n"))
        (message "    |%s|" line))
      (message "  Got:")
      (dolist (line (split-string result "\n"))
        (message "    |%s|" line)))))

(message "")
(message "=== Running 13 tests ===")
(message "")

(run-test "{ let continuation"
  "fn example() {
    Ok (( { let visible : HashSet < ID > =
    subscribee_content . iter ()
    . filter ( | id | ! subscriber_hides . contains ( id ) )
    . cloned () . collect ();
    visible },
       other_stuff ))
}"
  "fn example() {
  Ok (( { let visible : HashSet < ID > =
            subscribee_content . iter ()
            . filter ( | id | ! subscriber_hides . contains ( id ) )
            . cloned () . collect ();
          visible },
        other_stuff ))
}")

(run-test "{ if let continuation"
  "fn example() {
    child_treeids }
    { if let Some ( child_mut )
    = node_ref . tree () . get_mut ( child_treeid ) {
        add_missing_info_dfs ( child_mut ); }} }}
}"
  "fn example() {
  child_treeids }
  { if let Some ( child_mut )
      = node_ref . tree () . get_mut ( child_treeid ) {
    add_missing_info_dfs ( child_mut ); }} }}
}")

(run-test "tuple element alignment"
  "fn example() {
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
}"
  "fn example() {
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
}")

(run-test "{ expr ( paren content"
  "fn example() {
    { errors.push (
    BufferValidationError::Multiple (
    orgnode.clone() )); }}
}"
  "fn example() {
  { errors.push (
      BufferValidationError::Multiple (
        orgnode.clone() )); }}
}")

(run-test "closing brace alignment"
  "fn example() {
  { // comment
    if { let count: usize =
           node_ref . count();
         count
} > 1
    { errors.push ( x ); }}
}"
  "fn example() {
  { // comment
    if { let count: usize =
           node_ref . count();
         count
       } > 1
    { errors.push ( x ); }}
}")

(run-test "punctuation in first paren arg"
  "f( &x,
y,
z )"
  "f( &x,
   y,
   z )")

(run-test "simple paren no extra indent"
  "( x,
    y )"
  "( x,
  y )")

(run-test "struct literal field alignment"
  "fn example() {
    parent_mut . append ( NodePair { mskgnode: Some(skgnode),
    orgnode: new_orgnode } )
}"
  "fn example() {
  parent_mut . append ( NodePair { mskgnode: Some(skgnode),
                                   orgnode: new_orgnode } )
}")

(run-test "match arm brace blocks (=> on next line)"
  "fn example() {
    { Ok(Some(id)) if &id == target_skgid
=> return Ok(true),
      Ok(_) => continue, }}
}"
  "fn example() {
  { Ok(Some(id)) if &id == target_skgid
      => return Ok(true),
    Ok(_) => continue, }}
}")

(run-test "match arm brace blocks (body on next line)"
  "fn example() {
    { Ok(Some(id)) if &id == target_skgid =>
return Ok(true),
      Ok(_) => continue, }}"
  "fn example() {
  { Ok(Some(id)) if &id == target_skgid =>
      return Ok(true),
    Ok(_) => continue, }}")

(run-test "lambda with inline content (indented)"
  "fn example() {
  with_node_mut (
    tree, node_id,
    |mut node_mut| node_mut
. prepend ( x )
. id () ) ? };
}"
  "fn example() {
  with_node_mut (
    tree, node_id,
    |mut node_mut| node_mut
                   . prepend ( x )
                   . id () ) ? };
}")

(run-test "lambda body on next line"
  "f(
|mut col_mut|
col_mut . append (
  NodePair { mskgnode: None,
             orgnode: x })
. id () )"
  "f(
|mut col_mut|
  col_mut . append (
    NodePair { mskgnode: None,
               orgnode: x })
  . id () )")

(run-test "lambda with brace body"
  "f(
|np| { np . orgnode . metadata . code . viewRequests
. remove ( & ViewRequest::Definitive );
np . orgnode . metadata . code . indefinitive =
false; } )"
  "f(
|np| { np . orgnode . metadata . code . viewRequests
         . remove ( & ViewRequest::Definitive );
       np . orgnode . metadata . code . indefinitive =
         false; } )")

(message "")
(message "=== Results: %d/%d passed ===" test-passed-count test-count)
(if test-all-passed
    (progn
      (message "=== ALL TESTS PASSED ===")
      (kill-emacs 0))
  (progn
    (message "=== SOME TESTS FAILED ===")
    (kill-emacs 1)))