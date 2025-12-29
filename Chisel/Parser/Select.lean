/-
  Chisel.Parser.Select
  SELECT statement parser
-/
import Chisel.Core.Select
import Chisel.Parser.Lexer

namespace Chisel.Parser

open Parser Lexer

-- Forward declare parseExpr
opaque parseExprImpl : Parser Expr

namespace SelectParser

/-- Parse a single select item (expression with optional alias) -/
def parseSelectItem : Parser SelectItem := do
  let expr ← parseExprImpl
  let alias_ ← optional (keyword "AS" *> ident <|> tryP ident)
  pure (SelectItem.mk expr alias_)

/-- Parse select list (columns) -/
def parseSelectList : Parser (List SelectItem) := do
  let first ← parseSelectItem
  let rest ← many (comma *> parseSelectItem)
  pure (first :: rest)

/-- Parse table name with optional alias -/
def parseTableName : Parser TableRef := do
  let name ← ident
  let alias_ ← optional (optional (keyword "AS") *> ident)
  pure (.table name alias_)

/-- Parse parenthesized subquery in FROM -/
partial def parseSubqueryRef : Parser TableRef := do
  lparen
  let sel ← parseSelectCore
  rparen
  let alias_ ← optional (keyword "AS") *> ident
  pure (.subquery sel alias_)

/-- Parse join type -/
def parseJoinType : Parser JoinType := do
  let natural ← optional (keyword "NATURAL")
  let left ← optional (keyword "LEFT")
  let right ← optional (keyword "RIGHT")
  let full ← optional (keyword "FULL")
  let cross ← optional (keyword "CROSS")
  let inner ← optional (keyword "INNER")
  let _ ← optional (keyword "OUTER")
  let _ ← keyword "JOIN"

  if cross.isSome then pure .cross
  else if left.isSome then pure .left
  else if right.isSome then pure .right
  else if full.isSome then pure .full
  else pure .inner

/-- Parse a single table reference (table name or subquery) -/
partial def parseTablePrimary : Parser TableRef := do
  (tryP parseSubqueryRef)
  <|> parseTableName

/-- Parse FROM clause with joins -/
partial def parseTableRef : Parser TableRef := do
  let mut left ← parseTablePrimary

  while true do
    skipWs
    let joinOpt ← optional (tryP parseJoinType)
    match joinOpt with
    | none => break
    | some joinType =>
      let right ← parseTablePrimary
      let on ← if joinType != .cross then
          optional (keyword "ON" *> parseExprImpl)
        else
          pure none
      left := .join joinType left right on

  pure left

/-- Parse ORDER BY item -/
def parseOrderItem : Parser OrderItem := do
  let expr ← parseExprImpl
  let dir ← do
    let asc ← optional (keyword "ASC")
    let desc ← optional (keyword "DESC")
    if desc.isSome then pure SortDir.desc
    else pure SortDir.asc
  let nulls ← optional do
    let _ ← keyword "NULLS"
    let first ← optional (keyword "FIRST")
    if first.isSome then pure NullsOrder.first
    else do
      let _ ← keyword "LAST"
      pure NullsOrder.last
  pure (OrderItem.mk expr dir nulls)

/-- Parse ORDER BY clause -/
def parseOrderBy : Parser (List OrderItem) := do
  let _ ← keyword "ORDER"
  let _ ← keyword "BY"
  let first ← parseOrderItem
  let rest ← many (comma *> parseOrderItem)
  pure (first :: rest)

/-- Parse GROUP BY clause -/
def parseGroupBy : Parser (List Expr) := do
  let _ ← keyword "GROUP"
  let _ ← keyword "BY"
  let first ← parseExprImpl
  let rest ← many (comma *> parseExprImpl)
  pure (first :: rest)

/-- Parse LIMIT clause -/
def parseLimit : Parser Nat := do
  let _ ← keyword "LIMIT"
  let n ← intLit
  pure n.toNat

/-- Parse OFFSET clause -/
def parseOffset : Parser Nat := do
  let _ ← keyword "OFFSET"
  let n ← intLit
  pure n.toNat

/-- Parse complete SELECT statement -/
partial def parseSelectCore : Parser SelectCore := do
  skipWs
  let _ ← keyword "SELECT"

  -- DISTINCT
  let distinct ← optional (keyword "DISTINCT")
  let _ ← optional (keyword "ALL")

  -- Columns
  let columns ← parseSelectList

  -- FROM
  let from_ ← optional (keyword "FROM" *> parseTableRef)

  -- WHERE
  let where_ ← optional (keyword "WHERE" *> parseExprImpl)

  -- GROUP BY
  let groupBy ← optional parseGroupBy

  -- HAVING
  let having ← optional (keyword "HAVING" *> parseExprImpl)

  -- ORDER BY
  let orderBy ← optional parseOrderBy

  -- LIMIT
  let limit ← optional parseLimit

  -- OFFSET
  let offset ← optional parseOffset

  pure (SelectCore.mk
    distinct.isSome
    columns
    from_
    where_
    (groupBy.getD [])
    having
    (orderBy.getD [])
    limit
    offset)

end SelectParser

/-- Parse SELECT statement -/
def parseSelect : Parser SelectCore := SelectParser.parseSelectCore

end Chisel.Parser
