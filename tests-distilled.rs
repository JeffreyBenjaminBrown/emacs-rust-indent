/// This is not a working program.
/// It is instead a collection of examples,
/// from which to automatically generate tests.

///
/// { let continuation
///

/// ugly

fn example() {
    Ok (( { let visible : HashSet < ID > =
    subscribee_content . iter ()
    . filter ( | id | ! subscriber_hides . contains ( id ) )
    . cloned () . collect ();
    visible },
       other_stuff ))
}

/// pretty

fn example() {
  Ok (( { let visible : HashSet < ID > =
            subscribee_content . iter ()
            . filter ( | id | ! subscriber_hides . contains ( id ) )
            . cloned () . collect ();
          visible },
        other_stuff ))
}

///
/// { if let continuation
///

/// ugly

fn example() {
    child_treeids }
    { if let Some ( child_mut )
    = node_ref . tree () . get_mut ( child_treeid ) {
        add_missing_info_dfs ( child_mut ); }}

/// pretty

fn example() {
  child_treeids }
{ if let Some ( child_mut )
    = node_ref . tree () . get_mut ( child_treeid ) {
  add_missing_info_dfs ( child_mut ); }}

///
/// tuple element alignment
///

/// ugly

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

/// pretty

// This should hold for braces, parens and brackets.

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

///
/// { expr ( paren content
///

/// ugly

fn example() {
    { errors.push (
    BufferValidationError::Multiple (
    orgnode.clone() )); }}

/// pretty

// The rule here is to find the most recent unclosed opening thing
// (paren, brace or bracket),
// and if there is no text after it on that line,
// then find the first character of the text preceding it if any
// without proceeding past another opening brace,
// and indent two characters farther in than that character.
// (If there *is* text after it,
// then we should align to the first character of that text,
// as tested under 'tuple element alignment'.)

fn example() {
  { errors.push (
      BufferValidationError::Multiple (
        orgnode.clone() )); }}

///
/// closing brace alignment
///

/// ugly

fn example() {
  { // comment
    if { let count: usize =
           node_ref . count();
         count
} > 1
    { errors.push ( x ); }}
}

/// pretty

// This should hold for parens, braces and brackets.

fn example() {
  { // comment
    if { let count: usize =
           node_ref . count();
         count
       } > 1
    { errors.push ( x ); }}
}

///
/// punctuation in first paren arg
///

// Should apply for quotation marks, ampersands, maybe other stuff.

/// ugly

f( &x,
y,
z )

/// pretty

f( &x,
   y,
   z )

///
/// simple paren no extra indent
///

/// ugly

( x,
    y )

/// pretty

( x,
  y )

///
/// struct literal field alignment
///

/// ugly

fn example() {
    parent_mut . append ( NodePair { mskgnode: Some(skgnode),
    orgnode: new_orgnode } )
}

/// pretty

fn example() {
  parent_mut . append ( NodePair { mskgnode: Some(skgnode),
                                   orgnode: new_orgnode } )
}

///
/// match arm brace blocks (=> on next line)
///

/// ugly

fn example() {
    { Ok(Some(id)) if &id == target_skgid
=> return Ok(true),
      Ok(_) => continue, }}

/// pretty

fn example() {
  { Ok(Some(id)) if &id == target_skgid
      => return Ok(true),
    Ok(_) => continue, }}

///
/// match arm brace blocks (body on next line)
///

/// ugly

fn example() {
    { Ok(Some(id)) if &id == target_skgid =>
return Ok(true),
      Ok(_) => continue, }}

/// pretty

fn example() {
  { Ok(Some(id)) if &id == target_skgid =>
      return Ok(true),
    Ok(_) => continue, }}

///
/// lambda with inline content (indented)
///

/// ugly

fn example() {
  with_node_mut (
    tree, node_id,
    |mut node_mut| node_mut
. prepend ( x )
. id () ) ? };

/// pretty

fn example() {
  with_node_mut (
    tree, node_id,
    |mut node_mut| node_mut
      . prepend ( x )
      . id () ) ? };

///
/// lambda body on next line
///

/// ugly

f(
|mut col_mut|
col_mut . append (
  NodePair { mskgnode: None,
             orgnode: x })
. id () )

/// pretty

f(
|mut col_mut|
  col_mut . append (
    NodePair { mskgnode: None,
               orgnode: x })
  . id () )

///
/// lambda with brace body
///

/// ugly

f(
|np| { np . orgnode . metadata . code . viewRequests
. remove ( & ViewRequest::Definitive );
np . orgnode . metadata . code . indefinitive =
false; } )

/// pretty

f(
|np| { np . orgnode . metadata . code . viewRequests
         . remove ( & ViewRequest::Definitive );
       np . orgnode . metadata . code . indefinitive =
         false; } )

///
/// within-function top-level let
///

/// ugly

async fn append_indefinitive_node (
) -> Result < (), Box<dyn Error> > {
  let ( skgnode, mut orgnode ) : ( SkgNode, OrgNode ) =
  skgnode_and_orgnode_from_id (
    config, driver, node_id ) . await ?; }

/// pretty

async fn append_indefinitive_node (
) -> Result < (), Box<dyn Error> > {
  let ( skgnode, mut orgnode ) : ( SkgNode, OrgNode ) =
    skgnode_and_orgnode_from_id (
      config, driver, node_id ) . await ?; }
