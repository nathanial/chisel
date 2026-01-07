/-
  Chisel.Parser
  SQL parser public API (using Sift parser combinators)
-/
import Sift
import Chisel.Core.DML
import Chisel.Core.DDL
import Chisel.Parser.Lexer
import Chisel.Parser.Param

namespace Chisel.Parser

-- Open Sift but hide the ambiguous names; use Lexer's versions
open Sift hiding optional lexeme symbol
open Lexer

-- Re-export Sift.optional to avoid _root_.optional
abbrev opt {α : Type} (p : Parser α) := Sift.optional p

-- Helpers that return List instead of Array
def manyList {α : Type} (p : Parser α) : Parser (List α) := do
  let arr ← many p
  pure arr.toList

def many1List {α : Type} (p : Parser α) : Parser (List α) := do
  let arr ← many1 p
  pure arr.toList

def sepByList {α β : Type} (p : Parser α) (sep : Parser β) : Parser (List α) := do
  let arr ← sepBy p sep
  pure arr.toList

def sepBy1List {α β : Type} (p : Parser α) (sep : Parser β) : Parser (List α) := do
  let arr ← sepBy1 p sep
  pure arr.toList

/-!
## Expression Parser

This section implements the expression parser with proper mutual recursion
between expressions and SELECT statements (for subqueries).
-/

/-- Operator precedence levels (higher = binds tighter) -/
def precOr : Nat := 1
def precAnd : Nat := 2
def precNot : Nat := 3
def precCompare : Nat := 4
def precConcat : Nat := 5
def precAddSub : Nat := 6
def precMulDiv : Nat := 7

/-- Check if identifier is an aggregate function -/
def isAggFunc (name : String) : Bool :=
  let upper := name.toUpper
  ["COUNT", "SUM", "AVG", "MIN", "MAX", "TOTAL", "GROUP_CONCAT"].contains upper

mutual

/-- Parse literal value -/
partial def parseLiteral : Parser Expr := do
  (nullLit *> pure (.lit .null))
  <|> (trueLit *> pure (.lit (.bool true)))
  <|> (falseLit *> pure (.lit (.bool false)))
  <|> (do
    let s ← stringLit
    pure (.lit (.string s)))
  <|> (do
    let b ← blobLit
    pure (.lit (.blob b)))
  <|> (do
    let num ← number
    match num with
    | .inl i => pure (.lit (.int i))
    | .inr f => pure (.lit (.float f)))

/-- Parse parameter placeholder -/
partial def parseParam : Parser Expr := do
  (positionalParam *> pure (.param none none))
  <|> (do
    let idx ← indexedParam
    pure (.param none (some idx)))
  <|> (do
    let name ← colonParam
    pure (.param (some name) none))
  <|> (do
    let name ← atParam
    pure (.param (some name) none))

/-- Parse CASE expression -/
partial def parseCase : Parser Expr := do
  let _ ← keyword "CASE"
  let cases ← many1List parseWhenClause
  let else_ ← opt (keyword "ELSE" *> parseExprPrec precOr)
  let _ ← keyword "END"
  pure (.case_ cases else_)
where
  parseWhenClause : Parser (Expr × Expr) := do
    let _ ← keyword "WHEN"
    let cond ← parseExprPrec precOr
    let _ ← keyword "THEN"
    let result ← parseExprPrec precOr
    pure (cond, result)

/-- Parse CAST expression -/
partial def parseCast : Parser Expr := do
  let _ ← keyword "CAST"
  lparen
  let e ← parseExprPrec precOr
  let _ ← keyword "AS"
  let typeName ← identOrKeyword
  rparen
  pure (.cast e typeName)

/-- Parse function call (after seeing lparen) -/
partial def parseFuncCall (name : String) : Parser Expr := do
  if isAggFunc name then
    let distinct ← opt (keyword "DISTINCT")
    let upper := name.toUpper
    if upper == "COUNT" then
      let starOrExpr ← (star *> pure none) <|> (parseExprPrec precOr >>= fun e => pure (some e))
      rparen
      match starOrExpr with
      | none => pure (.agg .countAll none (distinct.isSome))
      | some e => pure (.agg .count (some e) (distinct.isSome))
    else if upper == "GROUP_CONCAT" then
      let e ← parseExprPrec precOr
      let sep ← opt (comma *> stringLit)
      rparen
      pure (.agg (.groupConcat sep) (some e) (distinct.isSome))
    else
      let e ← parseExprPrec precOr
      rparen
      let aggFunc := match upper with
        | "SUM" => AggFunc.sum
        | "AVG" => AggFunc.avg
        | "MIN" => AggFunc.min
        | "MAX" => AggFunc.max
        | "TOTAL" => AggFunc.total
        | _ => AggFunc.count
      pure (.agg aggFunc (some e) (distinct.isSome))
  else
    let args ← sepByList (parseExprPrec precOr) comma
    rparen
    pure (.func name args)

/-- Parse primary expression -/
partial def parsePrimary : Parser Expr := do
  skipWs
  -- Subquery: (SELECT ...)
  (attempt do
    lparen
    let sel ← parseSelectCore
    rparen
    pure (.subquery sel))
  -- Parenthesized expression
  <|> (attempt do
    lparen
    let e ← parseExprPrec precOr
    rparen
    pure e)
  -- CASE expression
  <|> (attempt parseCase)
  -- CAST expression
  <|> (attempt parseCast)
  -- NOT EXISTS
  <|> (attempt do
    let _ ← keyword "NOT"
    let _ ← keyword "EXISTS"
    lparen
    let sel ← parseSelectCore
    rparen
    pure (.notExists sel))
  -- EXISTS
  <|> (attempt do
    let _ ← keyword "EXISTS"
    lparen
    let sel ← parseSelectCore
    rparen
    pure (.exists_ sel))
  -- Parameter
  <|> (attempt parseParam)
  -- Literal
  <|> (attempt parseLiteral)
  -- table.* or just *
  <|> (attempt do
    let tbl ← ident
    dot
    star
    pure (.tableStar tbl))
  <|> (attempt do
    star
    pure .star)
  -- Function call or column reference
  <|> (attempt do
    let name ← identOrKeyword
    let hasParen ← opt lparen
    match hasParen with
    | none =>
      let qualified ← opt (dot *> ((star *> pure none) <|> (identOrKeyword >>= fun s => pure (some s))))
      match qualified with
      | some (some col) => pure (.qualified name col)
      | some none => pure (.tableStar name)
      | none => pure (.col name)
    | some () => parseFuncCall name)

/-- Parse unary expression -/
partial def parseUnary : Parser Expr := do
  skipWs
  (attempt do
    let _ ← keyword "NOT"
    let e ← parseUnary
    pure (.unary .not e))
  <|> (attempt do
    let _ ← lexeme (char '-')
    let e ← parseUnary
    pure (.unary .neg e))
  <|> parsePrimary

/-- Apply binary operator -/
partial def applyOp (op : String) (left right : Expr) : Expr :=
  match op with
  | "OR" => .binary .or left right
  | "AND" => .binary .and left right
  | "=" => .binary .eq left right
  | "<>" | "!=" => .binary .neq left right
  | "<" => .binary .lt left right
  | "<=" => .binary .lte left right
  | ">" => .binary .gt left right
  | ">=" => .binary .gte left right
  | "||" => .binary .concat left right
  | "+" => .binary .add left right
  | "-" => .binary .sub left right
  | "*" => .binary .mul left right
  | "/" => .binary .div left right
  | "%" => .binary .mod left right
  | "LIKE" => .binary .like left right
  | "NOT LIKE" => .binary .notLike left right
  | "GLOB" => .binary .glob left right
  | "IS" => .binary .is left right
  | "IS NOT" => .binary .isNot left right
  | "IS NULL" => .unary .isNull left
  | "IS NOT NULL" => .unary .isNotNull left
  | _ => left

/-- Parse expression with precedence -/
partial def parseExprPrec (minPrec : Nat) : Parser Expr := do
  let mut left ← parseUnary

  while true do
    skipWs
    let s ← Sift.Parser.get

    -- Try OR
    if minPrec <= precOr then
      let orOp ← opt (keyword "OR")
      if orOp.isSome then
        let right ← parseExprPrec (precOr + 1)
        left := applyOp "OR" left right
        continue

    -- Try AND
    if minPrec <= precAnd then
      let andOp ← opt (keyword "AND")
      if andOp.isSome then
        let right ← parseExprPrec (precAnd + 1)
        left := applyOp "AND" left right
        continue

    -- Try comparison operators
    if minPrec <= precCompare then
      -- IS NOT NULL, IS NULL, IS NOT, IS
      let isOp ← opt (keyword "IS")
      if isOp.isSome then
        let notOp ← opt (keyword "NOT")
        let nullOp ← opt (keyword "NULL")
        match notOp, nullOp with
        | some _, some _ =>
          left := applyOp "IS NOT NULL" left left
        | some _, none =>
          let right ← parseExprPrec (precCompare + 1)
          left := applyOp "IS NOT" left right
        | none, some _ =>
          left := applyOp "IS NULL" left left
        | none, none =>
          let right ← parseExprPrec (precCompare + 1)
          left := applyOp "IS" left right
        continue

      -- NOT IN, NOT LIKE, NOT BETWEEN, NOT GLOB
      let notOp ← opt (keyword "NOT")
      if notOp.isSome then
        let inOp ← opt (keyword "IN")
        if inOp.isSome then
          lparen
          let subq ← opt (attempt parseSelectCore)
          match subq with
          | some sel =>
            rparen
            left := .notInSubquery left sel
          | none =>
            let values ← sepBy1List (parseExprPrec precOr) comma
            rparen
            left := .notInValues left values
          continue
        let likeOp ← opt (keyword "LIKE")
        if likeOp.isSome then
          let right ← parseExprPrec (precCompare + 1)
          left := applyOp "NOT LIKE" left right
          continue
        let betweenOp ← opt (keyword "BETWEEN")
        if betweenOp.isSome then
          let lower ← parseExprPrec precAddSub
          let _ ← keyword "AND"
          let upper ← parseExprPrec precAddSub
          left := .unary .not (.between left lower upper)
          continue
        let globOp ← opt (keyword "GLOB")
        if globOp.isSome then
          let right ← parseExprPrec (precCompare + 1)
          left := .unary .not (.binary .glob left right)
          continue
        Sift.Parser.set s
        break

      -- IN
      let inOp ← opt (keyword "IN")
      if inOp.isSome then
        lparen
        let subq ← opt (attempt parseSelectCore)
        match subq with
        | some sel =>
          rparen
          left := .inSubquery left sel
        | none =>
          let values ← sepBy1List (parseExprPrec precOr) comma
          rparen
          left := .inValues left values
        continue

      -- LIKE
      let likeOp ← opt (keyword "LIKE")
      if likeOp.isSome then
        let right ← parseExprPrec (precCompare + 1)
        left := applyOp "LIKE" left right
        continue

      -- BETWEEN
      let betweenOp ← opt (keyword "BETWEEN")
      if betweenOp.isSome then
        let lower ← parseExprPrec precAddSub
        let _ ← keyword "AND"
        let upper ← parseExprPrec precAddSub
        left := .between left lower upper
        continue

      -- GLOB
      let globOp ← opt (keyword "GLOB")
      if globOp.isSome then
        let right ← parseExprPrec (precCompare + 1)
        left := applyOp "GLOB" left right
        continue

      -- <>, !=, <=, >=, <, >, =
      let neq ← opt (symbol "<>" <|> symbol "!=")
      if neq.isSome then
        let right ← parseExprPrec (precCompare + 1)
        left := applyOp "<>" left right
        continue
      let lte ← opt (symbol "<=")
      if lte.isSome then
        let right ← parseExprPrec (precCompare + 1)
        left := applyOp "<=" left right
        continue
      let gte ← opt (symbol ">=")
      if gte.isSome then
        let right ← parseExprPrec (precCompare + 1)
        left := applyOp ">=" left right
        continue
      let lt ← opt (symbol "<")
      if lt.isSome then
        let right ← parseExprPrec (precCompare + 1)
        left := applyOp "<" left right
        continue
      let gt ← opt (symbol ">")
      if gt.isSome then
        let right ← parseExprPrec (precCompare + 1)
        left := applyOp ">" left right
        continue
      let eq ← opt (symbol "=")
      if eq.isSome then
        let right ← parseExprPrec (precCompare + 1)
        left := applyOp "=" left right
        continue

    -- Concatenation ||
    if minPrec <= precConcat then
      let concatOp ← opt (symbol "||")
      if concatOp.isSome then
        let right ← parseExprPrec (precConcat + 1)
        left := applyOp "||" left right
        continue

    -- Add/Sub
    if minPrec <= precAddSub then
      let addOp ← opt (symbol "+")
      if addOp.isSome then
        let right ← parseExprPrec (precAddSub + 1)
        left := applyOp "+" left right
        continue
      let subOp ← opt (symbol "-")
      if subOp.isSome then
        let right ← parseExprPrec (precAddSub + 1)
        left := applyOp "-" left right
        continue

    -- Mul/Div/Mod
    if minPrec <= precMulDiv then
      let mulOp ← opt (symbol "*")
      if mulOp.isSome then
        let right ← parseExprPrec (precMulDiv + 1)
        left := applyOp "*" left right
        continue
      let divOp ← opt (symbol "/")
      if divOp.isSome then
        let right ← parseExprPrec (precMulDiv + 1)
        left := applyOp "/" left right
        continue
      let modOp ← opt (symbol "%")
      if modOp.isSome then
        let right ← parseExprPrec (precMulDiv + 1)
        left := applyOp "%" left right
        continue

    -- No operator found at this precedence
    break

  pure left

/-- Parse SELECT item -/
partial def parseSelectItem : Parser SelectItem := do
  let expr ← parseExprPrec precOr
  let alias_ ← opt (keyword "AS" *> ident <|> attempt ident)
  pure (SelectItem.mk expr alias_)

/-- Parse table reference -/
partial def parseTableRef : Parser TableRef := do
  let primary ← parseTablePrimary
  parseJoins primary
where
  parseTablePrimary : Parser TableRef := do
    (attempt do
      lparen
      let sel ← parseSelectCore
      rparen
      let alias_ ← opt (keyword "AS") *> ident
      pure (.subquery sel alias_))
    <|> do
      let name ← ident
      let alias_ ← opt (optional (keyword "AS") *> ident)
      pure (.table name alias_)

  parseJoins (left : TableRef) : Parser TableRef := do
    skipWs
    let joinType ← opt parseJoinType
    match joinType with
    | none => pure left
    | some jt =>
      let right ← parseTablePrimary
      let on ← if jt != .cross then
          optional (keyword "ON" *> parseExprPrec precOr)
        else
          pure none
      parseJoins (.join jt left right on)

  parseJoinType : Parser JoinType := do
    let _ ← opt (keyword "NATURAL")
    let left ← opt (keyword "LEFT")
    let right ← opt (keyword "RIGHT")
    let full ← opt (keyword "FULL")
    let cross ← opt (keyword "CROSS")
    let _ ← opt (keyword "INNER")
    let _ ← opt (keyword "OUTER")
    let _ ← keyword "JOIN"
    if cross.isSome then pure .cross
    else if left.isSome then pure .left
    else if right.isSome then pure .right
    else if full.isSome then pure .full
    else pure .inner

/-- Parse ORDER BY item -/
partial def parseOrderItem : Parser OrderItem := do
  let expr ← parseExprPrec precOr
  let dir ← do
    let desc ← opt (keyword "DESC")
    if desc.isSome then pure SortDir.desc
    else do
      let _ ← opt (keyword "ASC")
      pure SortDir.asc
  let nulls ← opt do
    let _ ← keyword "NULLS"
    let first ← opt (keyword "FIRST")
    if first.isSome then pure NullsOrder.first
    else do
      let _ ← keyword "LAST"
      pure NullsOrder.last
  pure (OrderItem.mk expr dir nulls)

/-- Parse SELECT statement -/
partial def parseSelectCore : Parser SelectCore := do
  skipWs
  let _ ← keyword "SELECT"
  let distinct ← opt (keyword "DISTINCT")
  let _ ← opt (keyword "ALL")
  let first ← parseSelectItem
  let rest ← manyList (comma *> parseSelectItem)
  let columns := first :: rest
  let from_ ← opt (keyword "FROM" *> parseTableRef)
  let where_ ← opt (keyword "WHERE" *> parseExprPrec precOr)
  let groupBy ← opt do
    let _ ← keyword "GROUP"
    let _ ← keyword "BY"
    let first ← parseExprPrec precOr
    let rest ← manyList (comma *> parseExprPrec precOr)
    pure (first :: rest)
  let having ← opt (keyword "HAVING" *> parseExprPrec precOr)
  let orderBy ← opt do
    let _ ← keyword "ORDER"
    let _ ← keyword "BY"
    let first ← parseOrderItem
    let rest ← manyList (comma *> parseOrderItem)
    pure (first :: rest)
  let limit ← opt (keyword "LIMIT" *> intLit)
  let offset ← opt (keyword "OFFSET" *> intLit)
  pure (SelectCore.mk
    distinct.isSome
    columns
    from_
    where_
    (groupBy.getD [])
    having
    (orderBy.getD [])
    (limit.map Int.toNat)
    (offset.map Int.toNat))

end

/-- Parse expression (entry point) -/
def parseExpr : Parser Expr := parseExprPrec precOr

/-- Parse SELECT statement (entry point) -/
def parseSelect : Parser SelectCore := parseSelectCore

/-!
## DML Parsers
-/

/-- Parse INSERT statement -/
def parseInsert : Parser InsertStmt := do
  skipWs
  let _ ← keyword "INSERT"
  let onConflict ← opt do
    let _ ← keyword "OR"
    (keyword "ABORT" *> pure ConflictAction.abort)
    <|> (keyword "ROLLBACK" *> pure .rollback)
    <|> (keyword "FAIL" *> pure .fail)
    <|> (keyword "IGNORE" *> pure .ignore)
    <|> (keyword "REPLACE" *> pure .replace)
  let _ ← keyword "INTO"
  let table ← ident
  let columns ← opt do
    lparen
    let first ← ident
    let rest ← manyList (comma *> ident)
    rparen
    pure (first :: rest)
  let valuesOrSelect ← do
    let valuesKw ← opt (keyword "VALUES")
    if valuesKw.isSome then
      let parseRow := do
        lparen
        let first ← parseExpr
        let rest ← manyList (comma *> parseExpr)
        rparen
        pure (first :: rest)
      let first ← parseRow
      let rest ← manyList (comma *> parseRow)
      pure (Sum.inl (first :: rest) : List (List Expr) ⊕ SelectCore)
    else
      let sel ← parseSelect
      pure (Sum.inr sel : List (List Expr) ⊕ SelectCore)
  let (values, fromSelect) := match valuesOrSelect with
    | Sum.inl vs => (vs, none)
    | Sum.inr sel => ([], some sel)
  let returning ← opt do
    let _ ← keyword "RETURNING"
    let first ← parseSelectItem
    let rest ← manyList (comma *> parseSelectItem)
    pure (first :: rest)
  pure {
    table
    columns := columns.getD []
    values
    fromSelect
    onConflict
    returning := returning.getD []
  }

/-- Parse UPDATE statement -/
def parseUpdate : Parser UpdateStmt := do
  skipWs
  let _ ← keyword "UPDATE"
  let _ ← opt do
    let _ ← keyword "OR"
    keyword "ABORT" <|> keyword "ROLLBACK" <|> keyword "FAIL" <|>
    keyword "IGNORE" <|> keyword "REPLACE"
  let table ← ident
  let alias_ ← opt (optional (keyword "AS") *> ident)
  let _ ← keyword "SET"
  let parseAssignment := do
    let column ← ident
    let _ ← symbol "="
    let value ← parseExpr
    pure { column, value : Assignment }
  let first ← parseAssignment
  let rest ← manyList (comma *> parseAssignment)
  let from_ ← opt (keyword "FROM" *> parseTableRef)
  let where_ ← opt (keyword "WHERE" *> parseExpr)
  let returning ← opt do
    let _ ← keyword "RETURNING"
    let first ← parseSelectItem
    let rest ← manyList (comma *> parseSelectItem)
    pure (first :: rest)
  pure {
    table
    alias_
    set := first :: rest
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
  let alias_ ← opt (optional (keyword "AS") *> ident)
  let where_ ← opt (keyword "WHERE" *> parseExpr)
  let returning ← opt do
    let _ ← keyword "RETURNING"
    let first ← parseSelectItem
    let rest ← manyList (comma *> parseSelectItem)
    pure (first :: rest)
  pure {
    table
    alias_
    where_
    returning := returning.getD []
  }

/-!
## DDL Parsers
-/

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
    let params ← opt do
      lparen
      let p ← intLit
      let s ← opt (comma *> intLit)
      rparen
      pure (p.toNat, s.map Int.toNat)
    match params with
    | some (p, s) => pure (.numeric (some p) s)
    | none => pure (.numeric none none)
  else if upper == "VARCHAR" || upper == "CHAR" then
    let len ← opt (lparen *> intLit <* rparen)
    pure (.varchar (len.map Int.toNat))
  else
    pure (.custom typeName)

/-- Parse foreign key action -/
def parseFKAction : Parser ForeignKeyAction := do
  (keyword "NO" *> keyword "ACTION" *> pure .noAction)
  <|> (keyword "RESTRICT" *> pure .restrict)
  <|> (keyword "CASCADE" *> pure .cascade)
  <|> (keyword "SET" *> keyword "NULL" *> pure .setNull)
  <|> (keyword "SET" *> keyword "DEFAULT" *> pure .setDefault)

/-- Parse column constraint -/
def parseColumnConstraint : Parser ColumnConstraint := do
  (do
    let _ ← keyword "PRIMARY"
    let _ ← keyword "KEY"
    let autoInc ← opt (keyword "AUTOINCREMENT")
    pure (.primaryKey autoInc.isSome))
  <|> (keyword "NOT" *> keyword "NULL" *> pure .notNull)
  <|> (keyword "UNIQUE" *> pure .unique)
  <|> (do
    let _ ← keyword "DEFAULT"
    let value ← parseExpr
    pure (.default value))
  <|> (do
    let _ ← keyword "CHECK"
    lparen
    let expr ← parseExpr
    rparen
    pure (.check expr))
  <|> (do
    let _ ← keyword "REFERENCES"
    let table ← ident
    lparen
    let column ← ident
    rparen
    let onDelete ← opt (keyword "ON" *> keyword "DELETE" *> parseFKAction)
    let onUpdate ← opt (keyword "ON" *> keyword "UPDATE" *> parseFKAction)
    pure (.references table column onDelete onUpdate))
  <|> (do
    let _ ← keyword "COLLATE"
    let name ← ident
    pure (.collate name))

/-- Parse column definition -/
def parseColumnDef : Parser ColumnDef := do
  let name ← ident
  let type ← parseColumnType
  let constraints ← manyList parseColumnConstraint
  pure { name, type, constraints }

/-- Parse table constraint -/
def parseTableConstraint : Parser TableConstraint := do
  let _ ← opt (keyword "CONSTRAINT" *> ident)
  (do
    let _ ← keyword "PRIMARY"
    let _ ← keyword "KEY"
    lparen
    let first ← ident
    let rest ← manyList (comma *> ident)
    rparen
    pure (.primaryKey (first :: rest)))
  <|> (do
    let _ ← keyword "UNIQUE"
    lparen
    let first ← ident
    let rest ← manyList (comma *> ident)
    rparen
    pure (.unique (first :: rest)))
  <|> (do
    let _ ← keyword "CHECK"
    lparen
    let expr ← parseExpr
    rparen
    pure (.check expr))
  <|> (do
    let _ ← keyword "FOREIGN"
    let _ ← keyword "KEY"
    lparen
    let first ← ident
    let rest ← manyList (comma *> ident)
    rparen
    let _ ← keyword "REFERENCES"
    let refTable ← ident
    lparen
    let refFirst ← ident
    let refRest ← manyList (comma *> ident)
    rparen
    let onDelete ← opt (keyword "ON" *> keyword "DELETE" *> parseFKAction)
    let onUpdate ← opt (keyword "ON" *> keyword "UPDATE" *> parseFKAction)
    pure (.foreignKey (first :: rest) refTable (refFirst :: refRest) onDelete onUpdate))

/-- Parse CREATE TABLE statement -/
def parseCreateTable : Parser CreateTableStmt := do
  skipWs
  let _ ← keyword "CREATE"
  let temporary ← opt (keyword "TEMPORARY" <|> keyword "TEMP")
  let _ ← keyword "TABLE"
  let ifNotExists ← opt (keyword "IF" *> keyword "NOT" *> keyword "EXISTS")
  let name ← ident
  lparen
  let firstCol ← parseColumnDef
  let restItems ← many (comma *> (
    attempt (parseTableConstraint >>= fun c => pure (Sum.inr c : ColumnDef ⊕ TableConstraint))
    <|> (parseColumnDef >>= fun c => pure (Sum.inl c : ColumnDef ⊕ TableConstraint))))
  rparen
  let strict ← opt (keyword "STRICT")
  let mut columns := [firstCol]
  let mut constraints := []
  for item in restItems do
    match item with
    | Sum.inl col => columns := columns ++ [col]
    | Sum.inr con => constraints := constraints ++ [con]
  pure {
    name
    ifNotExists := ifNotExists.isSome
    columns
    constraints
    temporary := temporary.isSome
    strict := strict.isSome
  }

/-- Parse ALTER TABLE statement -/
def parseAlterTable : Parser AlterTableStmt := do
  skipWs
  let _ ← keyword "ALTER"
  let _ ← keyword "TABLE"
  let table ← ident
  let parseOp :=
    (do
      let _ ← keyword "ADD"
      let _ ← opt (keyword "COLUMN")
      let col ← parseColumnDef
      pure (AlterOp.addColumn col))
    <|> (do
      let _ ← keyword "DROP"
      let _ ← opt (keyword "COLUMN")
      let name ← ident
      pure (.dropColumn name))
    <|> (do
      let _ ← keyword "RENAME"
      let _ ← opt (keyword "COLUMN")
      let oldName ← ident
      let _ ← keyword "TO"
      let newName ← ident
      pure (.renameColumn oldName newName))
    <|> (do
      let _ ← keyword "RENAME"
      let _ ← keyword "TO"
      let newName ← ident
      pure (.renameTable newName))
  let first ← parseOp
  let rest ← manyList (comma *> parseOp)
  pure { table, operations := first :: rest }

/-- Parse DROP TABLE statement -/
def parseDropTable : Parser DropTableStmt := do
  skipWs
  let _ ← keyword "DROP"
  let _ ← keyword "TABLE"
  let ifExists ← opt (keyword "IF" *> keyword "EXISTS")
  let table ← ident
  pure { table, ifExists := ifExists.isSome }

/-- Parse CREATE INDEX statement -/
def parseCreateIndex : Parser CreateIndexStmt := do
  skipWs
  let _ ← keyword "CREATE"
  let unique ← opt (keyword "UNIQUE")
  let _ ← keyword "INDEX"
  let ifNotExists ← opt (keyword "IF" *> keyword "NOT" *> keyword "EXISTS")
  let name ← ident
  let _ ← keyword "ON"
  let table ← ident
  lparen
  let parseCol := do
    let name ← ident
    let dir ← do
      let desc ← opt (keyword "DESC")
      if desc.isSome then pure (some SortDir.desc)
      else do
        let asc ← opt (keyword "ASC")
        if asc.isSome then pure (some SortDir.asc)
        else pure none
    pure (name, dir)
  let first ← parseCol
  let rest ← manyList (comma *> parseCol)
  rparen
  let where_ ← opt (keyword "WHERE" *> parseExpr)
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
  let ifExists ← opt (keyword "IF" *> keyword "EXISTS")
  let name ← ident
  pure { name, ifExists := ifExists.isSome }

/-!
## Statement Union Type
-/

/-- Union type for all statement types -/
inductive Statement where
  | select (s : SelectCore)
  | insert (s : InsertStmt)
  | update (s : UpdateStmt)
  | delete (s : DeleteStmt)
  | createTable (s : CreateTableStmt)
  | createIndex (s : CreateIndexStmt)
  | dropTable (s : DropTableStmt)
  | dropIndex (s : DropIndexStmt)
  | alterTable (s : AlterTableStmt)
  deriving Inhabited

/-- Parse any SQL statement -/
def parseStatement : Parser Statement := do
  skipWs
  (attempt (parseSelect >>= fun s => pure (.select s)))
  <|> (attempt (parseInsert >>= fun s => pure (.insert s)))
  <|> (attempt (parseUpdate >>= fun s => pure (.update s)))
  <|> (attempt (parseDelete >>= fun s => pure (.delete s)))
  <|> (attempt (parseCreateTable >>= fun s => pure (.createTable s)))
  <|> (attempt (parseCreateIndex >>= fun s => pure (.createIndex s)))
  <|> (attempt (parseDropTable >>= fun s => pure (.dropTable s)))
  <|> (attempt (parseDropIndex >>= fun s => pure (.dropIndex s)))
  <|> (attempt (parseAlterTable >>= fun s => pure (.alterTable s)))

/-!
## Public API
-/

/-- Parse SQL expression string -/
def Expr.parse (sql : String) : Except ParseError Expr :=
  Parser.run parseExpr sql

/-- Parse SQL SELECT statement -/
def SelectCore.parse (sql : String) : Except ParseError SelectCore :=
  Parser.run parseSelect sql

/-- Parse SQL INSERT statement -/
def InsertStmt.parse (sql : String) : Except ParseError InsertStmt :=
  Parser.run parseInsert sql

/-- Parse SQL UPDATE statement -/
def UpdateStmt.parse (sql : String) : Except ParseError UpdateStmt :=
  Parser.run parseUpdate sql

/-- Parse SQL DELETE statement -/
def DeleteStmt.parse (sql : String) : Except ParseError DeleteStmt :=
  Parser.run parseDelete sql

/-- Parse SQL CREATE TABLE statement -/
def CreateTableStmt.parse (sql : String) : Except ParseError CreateTableStmt :=
  Parser.run parseCreateTable sql

/-- Parse SQL CREATE INDEX statement -/
def CreateIndexStmt.parse (sql : String) : Except ParseError CreateIndexStmt :=
  Parser.run parseCreateIndex sql

/-- Parse SQL DROP TABLE statement -/
def DropTableStmt.parse (sql : String) : Except ParseError DropTableStmt :=
  Parser.run parseDropTable sql

/-- Parse SQL DROP INDEX statement -/
def DropIndexStmt.parse (sql : String) : Except ParseError DropIndexStmt :=
  Parser.run parseDropIndex sql

/-- Parse SQL ALTER TABLE statement -/
def AlterTableStmt.parse (sql : String) : Except ParseError AlterTableStmt :=
  Parser.run parseAlterTable sql

/-- Parse any SQL statement -/
def Statement.parse (sql : String) : Except ParseError Statement :=
  Parser.run parseStatement sql

end Chisel.Parser
