/-
  Chisel.Parser.DML
  INSERT, UPDATE, DELETE statement parsers
-/
import Chisel.Core.DML
import Chisel.Parser.Lexer

namespace Chisel.Parser

open Parser Lexer

-- Forward declarations
opaque parseExprImpl : Parser Expr
opaque parseSelectImpl : Parser SelectCore
opaque parseTableRefImpl : Parser TableRef

namespace DMLParser

/-- Parse conflict action (OR IGNORE, OR REPLACE, etc.) -/
def parseConflictAction : Parser ConflictAction := do
  let _ ← keyword "OR"
  (keyword "ABORT" *> pure .abort)
  <|> (keyword "ROLLBACK" *> pure .rollback)
  <|> (keyword "FAIL" *> pure .fail)
  <|> (keyword "IGNORE" *> pure .ignore)
  <|> (keyword "REPLACE" *> pure .replace)

/-- Parse SELECT item for RETURNING -/
def parseSelectItem : Parser SelectItem := do
  let expr ← parseExprImpl
  let alias_ ← optional (keyword "AS" *> ident <|> tryP ident)
  pure (SelectItem.mk expr alias_)

/-- Parse RETURNING clause -/
def parseReturning : Parser (List SelectItem) := do
  let _ ← keyword "RETURNING"
  let first ← parseSelectItem
  let rest ← many (comma *> parseSelectItem)
  pure (first :: rest)

/-- Parse column list for INSERT -/
def parseColumnList : Parser (List String) := do
  lparen
  let first ← ident
  let rest ← many (comma *> ident)
  rparen
  pure (first :: rest)

/-- Parse VALUES row -/
def parseValuesRow : Parser (List Expr) := do
  lparen
  let first ← parseExprImpl
  let rest ← many (comma *> parseExprImpl)
  rparen
  pure (first :: rest)

/-- Parse INSERT statement -/
def parseInsert : Parser InsertStmt := do
  skipWs
  let _ ← keyword "INSERT"

  -- Optional conflict action
  let onConflict ← optional parseConflictAction

  let _ ← keyword "INTO"
  let table ← ident

  -- Optional column list
  let columns ← optional parseColumnList

  -- VALUES or SELECT
  let valuesOrSelect ← do
    let valuesKw ← optional (keyword "VALUES")
    if valuesKw.isSome then
      let first ← parseValuesRow
      let rest ← many (comma *> parseValuesRow)
      pure (.inl (first :: rest))
    else
      let sel ← parseSelectImpl
      pure (.inr sel)

  let (values, fromSelect) := match valuesOrSelect with
    | .inl vs => (vs, none)
    | .inr sel => ([], some sel)

  -- RETURNING
  let returning ← optional parseReturning

  pure {
    table
    columns := columns.getD []
    values
    fromSelect
    onConflict
    returning := returning.getD []
  }

/-- Parse SET assignment -/
def parseAssignment : Parser Assignment := do
  let column ← ident
  let _ ← symbol "="
  let value ← parseExprImpl
  pure { column, value }

/-- Parse UPDATE statement -/
def parseUpdate : Parser UpdateStmt := do
  skipWs
  let _ ← keyword "UPDATE"

  -- Optional conflict action (UPDATE OR IGNORE, etc.)
  let _ ← optional parseConflictAction

  let table ← ident

  -- Optional alias
  let alias_ ← optional (optional (keyword "AS") *> ident)

  let _ ← keyword "SET"

  -- Assignments
  let first ← parseAssignment
  let rest ← many (comma *> parseAssignment)
  let assignments := first :: rest

  -- Optional FROM
  let from_ ← optional (keyword "FROM" *> parseTableRefImpl)

  -- Optional WHERE
  let where_ ← optional (keyword "WHERE" *> parseExprImpl)

  -- RETURNING
  let returning ← optional parseReturning

  pure {
    table
    alias_
    set := assignments
    from_
    where_
    returning := returning.getD []
  }

/-- Parse DELETE statement -/
def parseDelete : Parser DeleteStmt := do
  skipWs
  let _ ← keyword "DELETE"
  let _ ← keyword "FROM"

  let table ← ident

  -- Optional alias
  let alias_ ← optional (optional (keyword "AS") *> ident)

  -- Optional WHERE
  let where_ ← optional (keyword "WHERE" *> parseExprImpl)

  -- RETURNING
  let returning ← optional parseReturning

  pure {
    table
    alias_
    where_
    returning := returning.getD []
  }

end DMLParser

/-- Parse INSERT statement -/
def parseInsert : Parser InsertStmt := DMLParser.parseInsert

/-- Parse UPDATE statement -/
def parseUpdate : Parser UpdateStmt := DMLParser.parseUpdate

/-- Parse DELETE statement -/
def parseDelete : Parser DeleteStmt := DMLParser.parseDelete

end Chisel.Parser
