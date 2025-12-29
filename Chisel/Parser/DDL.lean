/-
  Chisel.Parser.DDL
  CREATE TABLE, CREATE INDEX, DROP, ALTER statement parsers
-/
import Chisel.Core.DDL
import Chisel.Parser.Lexer

namespace Chisel.Parser

open Parser Lexer

-- Forward declaration
opaque parseExprImpl : Parser Expr

namespace DDLParser

/-- Parse column type -/
def parseColumnType : Parser ColumnType := do
  let typeName ← identOrKeyword
  let upper := typeName.toUpper

  if upper == "INTEGER" || upper == "INT" then
    pure .integer
  else if upper == "REAL" || upper == "FLOAT" || upper == "DOUBLE" then
    pure .real
  else if upper == "TEXT" || upper == "STRING" then
    pure .text
  else if upper == "BLOB" then
    pure .blob
  else if upper == "BOOLEAN" || upper == "BOOL" then
    pure .boolean
  else if upper == "DATETIME" || upper == "TIMESTAMP" then
    pure .datetime
  else if upper == "NUMERIC" || upper == "DECIMAL" then
    -- Optional precision and scale
    let params ← optional do
      lparen
      let p ← intLit
      let s ← optional (comma *> intLit)
      rparen
      pure (p.toNat, s.map Int.toNat)
    match params with
    | some (p, s) => pure (.numeric (some p) s)
    | none => pure (.numeric none none)
  else if upper == "VARCHAR" || upper == "CHAR" then
    let len ← optional (lparen *> intLit <* rparen)
    pure (.varchar (len.map Int.toNat))
  else
    pure (.custom typeName)

/-- Parse foreign key action -/
def parseForeignKeyAction : Parser ForeignKeyAction := do
  (keyword "NO" *> keyword "ACTION" *> pure .noAction)
  <|> (keyword "RESTRICT" *> pure .restrict)
  <|> (keyword "CASCADE" *> pure .cascade)
  <|> (keyword "SET" *> keyword "NULL" *> pure .setNull)
  <|> (keyword "SET" *> keyword "DEFAULT" *> pure .setDefault)

/-- Parse column constraint -/
def parseColumnConstraint : Parser ColumnConstraint := do
  -- PRIMARY KEY
  (do
    let _ ← keyword "PRIMARY"
    let _ ← keyword "KEY"
    let autoInc ← optional (keyword "AUTOINCREMENT")
    pure (.primaryKey autoInc.isSome))
  -- NOT NULL
  <|> (keyword "NOT" *> keyword "NULL" *> pure .notNull)
  -- UNIQUE
  <|> (keyword "UNIQUE" *> pure .unique)
  -- DEFAULT
  <|> (do
    let _ ← keyword "DEFAULT"
    let value ← parseExprImpl
    pure (.default value))
  -- CHECK
  <|> (do
    let _ ← keyword "CHECK"
    lparen
    let expr ← parseExprImpl
    rparen
    pure (.check expr))
  -- REFERENCES
  <|> (do
    let _ ← keyword "REFERENCES"
    let table ← ident
    lparen
    let column ← ident
    rparen
    let onDelete ← optional (keyword "ON" *> keyword "DELETE" *> parseForeignKeyAction)
    let onUpdate ← optional (keyword "ON" *> keyword "UPDATE" *> parseForeignKeyAction)
    pure (.references table column onDelete onUpdate))
  -- COLLATE
  <|> (do
    let _ ← keyword "COLLATE"
    let name ← ident
    pure (.collate name))

/-- Parse column definition -/
def parseColumnDef : Parser ColumnDef := do
  let name ← ident
  let type ← parseColumnType
  let constraints ← many parseColumnConstraint
  pure { name, type, constraints }

/-- Parse table constraint -/
def parseTableConstraint : Parser TableConstraint := do
  -- Optional CONSTRAINT name
  let _ ← optional (keyword "CONSTRAINT" *> ident)

  -- PRIMARY KEY (col1, col2, ...)
  (do
    let _ ← keyword "PRIMARY"
    let _ ← keyword "KEY"
    lparen
    let first ← ident
    let rest ← many (comma *> ident)
    rparen
    pure (.primaryKey (first :: rest)))
  -- UNIQUE (col1, col2, ...)
  <|> (do
    let _ ← keyword "UNIQUE"
    lparen
    let first ← ident
    let rest ← many (comma *> ident)
    rparen
    pure (.unique (first :: rest)))
  -- CHECK (expr)
  <|> (do
    let _ ← keyword "CHECK"
    lparen
    let expr ← parseExprImpl
    rparen
    pure (.check expr))
  -- FOREIGN KEY (cols) REFERENCES table (cols)
  <|> (do
    let _ ← keyword "FOREIGN"
    let _ ← keyword "KEY"
    lparen
    let first ← ident
    let rest ← many (comma *> ident)
    rparen
    let _ ← keyword "REFERENCES"
    let refTable ← ident
    lparen
    let refFirst ← ident
    let refRest ← many (comma *> ident)
    rparen
    let onDelete ← optional (keyword "ON" *> keyword "DELETE" *> parseForeignKeyAction)
    let onUpdate ← optional (keyword "ON" *> keyword "UPDATE" *> parseForeignKeyAction)
    pure (.foreignKey (first :: rest) refTable (refFirst :: refRest) onDelete onUpdate))

/-- Parse CREATE TABLE statement -/
def parseCreateTable : Parser CreateTableStmt := do
  skipWs
  let _ ← keyword "CREATE"

  -- TEMPORARY / TEMP
  let temporary ← optional (keyword "TEMPORARY" <|> keyword "TEMP")

  let _ ← keyword "TABLE"

  -- IF NOT EXISTS
  let ifNotExists ← optional (keyword "IF" *> keyword "NOT" *> keyword "EXISTS")

  let name ← ident

  lparen

  -- Parse column definitions and table constraints
  let firstItem ← parseColumnDef
  let restItems ← many (comma *> (tryP parseTableConstraint >>= fun c => pure (.inr c)
                                  <|> (parseColumnDef >>= fun c => pure (.inl c))))

  rparen

  -- STRICT
  let strict ← optional (keyword "STRICT")

  -- Separate columns and constraints
  let mut columns := [firstItem]
  let mut constraints := []
  for item in restItems do
    match item with
    | .inl col => columns := columns ++ [col]
    | .inr con => constraints := constraints ++ [con]

  pure {
    name
    ifNotExists := ifNotExists.isSome
    columns
    constraints
    temporary := temporary.isSome
    strict := strict.isSome
  }

/-- Parse ALTER operation -/
def parseAlterOp : Parser AlterOp := do
  -- ADD COLUMN
  (do
    let _ ← keyword "ADD"
    let _ ← optional (keyword "COLUMN")
    let col ← parseColumnDef
    pure (.addColumn col))
  -- DROP COLUMN
  <|> (do
    let _ ← keyword "DROP"
    let _ ← optional (keyword "COLUMN")
    let name ← ident
    pure (.dropColumn name))
  -- RENAME COLUMN
  <|> (do
    let _ ← keyword "RENAME"
    let _ ← optional (keyword "COLUMN")
    let oldName ← ident
    let _ ← keyword "TO"
    let newName ← ident
    pure (.renameColumn oldName newName))
  -- RENAME TO (table)
  <|> (do
    let _ ← keyword "RENAME"
    let _ ← keyword "TO"
    let newName ← ident
    pure (.renameTable newName))

/-- Parse ALTER TABLE statement -/
def parseAlterTable : Parser AlterTableStmt := do
  skipWs
  let _ ← keyword "ALTER"
  let _ ← keyword "TABLE"
  let table ← ident
  let first ← parseAlterOp
  let rest ← many (comma *> parseAlterOp)
  pure {
    table
    operations := first :: rest
  }

/-- Parse DROP TABLE statement -/
def parseDropTable : Parser DropTableStmt := do
  skipWs
  let _ ← keyword "DROP"
  let _ ← keyword "TABLE"
  let ifExists ← optional (keyword "IF" *> keyword "EXISTS")
  let table ← ident
  pure {
    table
    ifExists := ifExists.isSome
  }

/-- Parse index column -/
def parseIndexColumn : Parser (String × Option SortDir) := do
  let name ← ident
  let dir ← do
    let asc ← optional (keyword "ASC")
    let desc ← optional (keyword "DESC")
    if desc.isSome then pure (some SortDir.desc)
    else if asc.isSome then pure (some SortDir.asc)
    else pure none
  pure (name, dir)

/-- Parse CREATE INDEX statement -/
def parseCreateIndex : Parser CreateIndexStmt := do
  skipWs
  let _ ← keyword "CREATE"

  -- UNIQUE
  let unique ← optional (keyword "UNIQUE")

  let _ ← keyword "INDEX"

  -- IF NOT EXISTS
  let ifNotExists ← optional (keyword "IF" *> keyword "NOT" *> keyword "EXISTS")

  let name ← ident
  let _ ← keyword "ON"
  let table ← ident

  lparen
  let first ← parseIndexColumn
  let rest ← many (comma *> parseIndexColumn)
  rparen

  -- WHERE (partial index)
  let where_ ← optional (keyword "WHERE" *> parseExprImpl)

  pure {
    name
    table
    columns := first :: rest
    unique := unique.isSome
    ifNotExists := ifNotExists.isSome
    where_
  }

/-- Parse DROP INDEX statement -/
def parseDropIndex : Parser DropIndexStmt := do
  skipWs
  let _ ← keyword "DROP"
  let _ ← keyword "INDEX"
  let ifExists ← optional (keyword "IF" *> keyword "EXISTS")
  let name ← ident
  pure {
    name
    ifExists := ifExists.isSome
  }

end DDLParser

/-- Parse CREATE TABLE statement -/
def parseCreateTable : Parser CreateTableStmt := DDLParser.parseCreateTable

/-- Parse ALTER TABLE statement -/
def parseAlterTable : Parser AlterTableStmt := DDLParser.parseAlterTable

/-- Parse DROP TABLE statement -/
def parseDropTable : Parser DropTableStmt := DDLParser.parseDropTable

/-- Parse CREATE INDEX statement -/
def parseCreateIndex : Parser CreateIndexStmt := DDLParser.parseCreateIndex

/-- Parse DROP INDEX statement -/
def parseDropIndex : Parser DropIndexStmt := DDLParser.parseDropIndex

end Chisel.Parser
