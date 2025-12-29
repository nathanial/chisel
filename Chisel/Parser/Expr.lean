/-
  Chisel.Parser.Expr
  SQL expression parser
-/
import Chisel.Core.Expr
import Chisel.Parser.Lexer

namespace Chisel.Parser

open Parser Lexer

/-- Forward declaration for parseSelect (defined in Select.lean) -/
opaque parseSelectCore : Parser SelectCore

/-- Operator precedence levels (higher = binds tighter) -/
inductive Prec where
  | or_       : Prec  -- 1: OR
  | and_      : Prec  -- 2: AND
  | not_      : Prec  -- 3: NOT (unary)
  | compare   : Prec  -- 4: =, <>, <, <=, >, >=, IS, LIKE, IN, BETWEEN
  | concat    : Prec  -- 5: ||
  | addSub    : Prec  -- 6: +, -
  | mulDiv    : Prec  -- 7: *, /, %
  | unaryNeg  : Prec  -- 8: - (unary)
  | primary   : Prec  -- 9: literals, columns, functions, parens
  deriving Repr, BEq, Ord

def Prec.toNat : Prec → Nat
  | .or_ => 1
  | .and_ => 2
  | .not_ => 3
  | .compare => 4
  | .concat => 5
  | .addSub => 6
  | .mulDiv => 7
  | .unaryNeg => 8
  | .primary => 9

instance : LE Prec where
  le a b := a.toNat <= b.toNat

instance : LT Prec where
  lt a b := a.toNat < b.toNat

instance (a b : Prec) : Decidable (a ≤ b) := Nat.decLe a.toNat b.toNat
instance (a b : Prec) : Decidable (a < b) := Nat.decLt a.toNat b.toNat

namespace ExprParser

/-- Parse literal value -/
def parseLiteral : Parser Expr := do
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
def parseParam : Parser Expr := do
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

/-- Parse column reference (possibly qualified) -/
def parseColumn : Parser Expr := do
  let first ← ident
  let qualified ← optional (dot *> (star <|> (ident >>= fun s => pure ())))
  match qualified with
  | some () =>
    -- Check if we consumed a star or an ident
    let s ← get
    -- Need to re-parse to know what we got
    pure (.qualified first first)  -- Placeholder, will fix below
  | none =>
    pure (.col first)

/-- Parse column reference with qualified names -/
def parseColumnRef : Parser Expr := do
  let first ← ident
  let rest ← optional (dot *> identOrKeyword)
  match rest with
  | some second => pure (.qualified first second)
  | none => pure (.col first)

/-- Parse star expression -/
def parseStar : Parser Expr := do
  star
  pure .star

/-- Parse table.* expression -/
def parseTableStar : Parser Expr := do
  let tbl ← ident
  dot
  star
  pure (.tableStar tbl)

/-- Parse aggregate function -/
def parseAggFunc (name : String) : Parser Expr := do
  lparen
  let distinct ← optional (keyword "DISTINCT")
  let upper := name.toUpper
  if upper == "COUNT" then
    let starOrExpr ← (star *> pure none) <|> (parseExprPrec .or_ >>= fun e => pure (some e))
    rparen
    match starOrExpr with
    | none => pure (.agg .countAll none (distinct.isSome))
    | some e => pure (.agg .count (some e) (distinct.isSome))
  else if upper == "SUM" then
    let e ← parseExprPrec .or_
    rparen
    pure (.agg .sum (some e) (distinct.isSome))
  else if upper == "AVG" then
    let e ← parseExprPrec .or_
    rparen
    pure (.agg .avg (some e) (distinct.isSome))
  else if upper == "MIN" then
    let e ← parseExprPrec .or_
    rparen
    pure (.agg .min (some e) (distinct.isSome))
  else if upper == "MAX" then
    let e ← parseExprPrec .or_
    rparen
    pure (.agg .max (some e) (distinct.isSome))
  else if upper == "TOTAL" then
    let e ← parseExprPrec .or_
    rparen
    pure (.agg .total (some e) (distinct.isSome))
  else if upper == "GROUP_CONCAT" then
    let e ← parseExprPrec .or_
    let sep ← optional (comma *> stringLit)
    rparen
    pure (.agg (.groupConcat sep) (some e) (distinct.isSome))
  else
    fail s!"unknown aggregate function: {name}"

/-- Parse CASE expression -/
def parseCase : Parser Expr := do
  let _ ← keyword "CASE"
  let cases ← many1 parseWhenClause
  let else_ ← optional (keyword "ELSE" *> parseExprPrec .or_)
  let _ ← keyword "END"
  pure (.case_ cases else_)
where
  parseWhenClause : Parser (Expr × Expr) := do
    let _ ← keyword "WHEN"
    let cond ← parseExprPrec .or_
    let _ ← keyword "THEN"
    let result ← parseExprPrec .or_
    pure (cond, result)

/-- Parse CAST expression -/
def parseCast : Parser Expr := do
  let _ ← keyword "CAST"
  lparen
  let e ← parseExprPrec .or_
  let _ ← keyword "AS"
  let typeName ← identOrKeyword
  rparen
  pure (.cast e typeName)

/-- Parse EXISTS expression -/
def parseExists : Parser Expr := do
  let _ ← keyword "EXISTS"
  lparen
  let sel ← parseSelectCore
  rparen
  pure (.exists_ sel)

/-- Parse NOT EXISTS expression -/
def parseNotExists : Parser Expr := do
  let _ ← keyword "NOT"
  let _ ← keyword "EXISTS"
  lparen
  let sel ← parseSelectCore
  rparen
  pure (.notExists sel)

/-- Check if identifier is an aggregate function -/
def isAggFunc (name : String) : Bool :=
  let upper := name.toUpper
  ["COUNT", "SUM", "AVG", "MIN", "MAX", "TOTAL", "GROUP_CONCAT"].contains upper

/-- Parse function call or identifier -/
def parseFuncOrIdent : Parser Expr := do
  let name ← identOrKeyword
  let hasParen ← optional lparen
  match hasParen with
  | none => pure (.col name)
  | some () =>
    if isAggFunc name then
      -- Backtrack and parse as aggregate
      let s ← get
      set { s with pos := { s.pos with offset := s.pos.offset - 1 } }  -- Hackish
      parseAggFuncFromName name
    else
      -- Regular function
      let args ← sepBy (parseExprPrec .or_) comma
      rparen
      pure (.func name args)
where
  parseAggFuncFromName (name : String) : Parser Expr := do
    let upper := name.toUpper
    let distinct ← optional (keyword "DISTINCT")
    if upper == "COUNT" then
      let starOrExpr ← (star *> pure none) <|> (parseExprPrec .or_ >>= fun e => pure (some e))
      rparen
      match starOrExpr with
      | none => pure (.agg .countAll none (distinct.isSome))
      | some e => pure (.agg .count (some e) (distinct.isSome))
    else
      let e ← parseExprPrec .or_
      rparen
      let aggFunc := match upper with
        | "SUM" => AggFunc.sum
        | "AVG" => AggFunc.avg
        | "MIN" => AggFunc.min
        | "MAX" => AggFunc.max
        | "TOTAL" => AggFunc.total
        | _ => AggFunc.count  -- fallback
      pure (.agg aggFunc (some e) (distinct.isSome))

/-- Parse function call (already saw lparen) -/
def parseFuncCall (name : String) : Parser Expr := do
  if isAggFunc name then
    let distinct ← optional (keyword "DISTINCT")
    let upper := name.toUpper
    if upper == "COUNT" then
      let starOrExpr ← (star *> pure none) <|> (parseExprPrec .or_ >>= fun e => pure (some e))
      rparen
      match starOrExpr with
      | none => pure (.agg .countAll none (distinct.isSome))
      | some e => pure (.agg .count (some e) (distinct.isSome))
    else if upper == "GROUP_CONCAT" then
      let e ← parseExprPrec .or_
      let sep ← optional (comma *> stringLit)
      rparen
      pure (.agg (.groupConcat sep) (some e) (distinct.isSome))
    else
      let e ← parseExprPrec .or_
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
    let args ← sepBy (parseExprPrec .or_) comma
    rparen
    pure (.func name args)

/-- Parse primary expression (highest precedence) -/
def parsePrimary : Parser Expr := do
  skipWs
  -- Try subquery first: (SELECT ...)
  (tryP do
    lparen
    let sel ← parseSelectCore
    rparen
    pure (.subquery sel))
  -- Parenthesized expression
  <|> (tryP do
    lparen
    let e ← parseExprPrec .or_
    rparen
    pure e)
  -- CASE expression
  <|> (tryP parseCase)
  -- CAST expression
  <|> (tryP parseCast)
  -- NOT EXISTS
  <|> (tryP parseNotExists)
  -- EXISTS
  <|> (tryP parseExists)
  -- Parameter
  <|> (tryP parseParam)
  -- Literal
  <|> (tryP parseLiteral)
  -- table.* or just *
  <|> (tryP parseTableStar)
  <|> (tryP parseStar)
  -- Function call or column reference
  <|> (tryP do
    let name ← identOrKeyword
    let hasParen ← optional lparen
    match hasParen with
    | none =>
      -- Check for qualified column: name.something
      let qualified ← optional (dot *> ((star *> pure none) <|> (identOrKeyword >>= fun s => pure (some s))))
      match qualified with
      | some (some col) => pure (.qualified name col)
      | some none => pure (.tableStar name)  -- name.*
      | none => pure (.col name)
    | some () => parseFuncCall name)

/-- Parse unary operators -/
def parseUnary : Parser Expr := do
  skipWs
  -- NOT
  (tryP do
    let _ ← keyword "NOT"
    let e ← parseUnary
    pure (.unary .not e))
  -- Negative
  <|> (tryP do
    let _ ← lexeme (char '-')
    let e ← parseUnary
    pure (.unary .neg e))
  -- Primary
  <|> parsePrimary

/-- Parse binary expression with precedence climbing -/
partial def parseExprPrec (minPrec : Prec) : Parser Expr := do
  let mut left ← parseUnary

  while true do
    skipWs
    let s ← get

    -- Try to parse an operator at current precedence or higher
    let opResult ← tryParseOp minPrec
    match opResult with
    | none =>
      set s
      break
    | some (op, opPrec, rightAssoc) =>
      let nextPrec := if rightAssoc then opPrec else
        match opPrec with
        | .or_ => .and_
        | .and_ => .not_
        | .compare => .concat
        | .concat => .addSub
        | .addSub => .mulDiv
        | .mulDiv => .unaryNeg
        | _ => .primary
      let right ← parseExprPrec nextPrec
      left := applyOp op left right

  pure left
where
  tryParseOp (minPrec : Prec) : Parser (Option (String × Prec × Bool)) := do
    skipWs
    -- OR (precedence 1)
    if minPrec <= .or_ then
      let orOp ← optional (keyword "OR")
      if orOp.isSome then return some ("OR", .or_, false)

    -- AND (precedence 2)
    if minPrec <= .and_ then
      let andOp ← optional (keyword "AND")
      if andOp.isSome then return some ("AND", .and_, false)

    -- Comparison operators (precedence 4)
    if minPrec <= .compare then
      -- IS NOT NULL, IS NULL, IS NOT, IS
      let isOp ← optional (keyword "IS")
      if isOp.isSome then
        let notOp ← optional (keyword "NOT")
        let nullOp ← optional (keyword "NULL")
        match notOp, nullOp with
        | some _, some _ => return some ("IS NOT NULL", .compare, false)
        | some _, none => return some ("IS NOT", .compare, false)
        | none, some _ => return some ("IS NULL", .compare, false)
        | none, none => return some ("IS", .compare, false)

      -- NOT IN, NOT LIKE, NOT BETWEEN
      let notOp ← optional (keyword "NOT")
      if notOp.isSome then
        let inOp ← optional (keyword "IN")
        if inOp.isSome then return some ("NOT IN", .compare, false)
        let likeOp ← optional (keyword "LIKE")
        if likeOp.isSome then return some ("NOT LIKE", .compare, false)
        let betweenOp ← optional (keyword "BETWEEN")
        if betweenOp.isSome then return some ("NOT BETWEEN", .compare, false)
        let globOp ← optional (keyword "GLOB")
        if globOp.isSome then return some ("NOT GLOB", .compare, false)
        fail "expected IN, LIKE, BETWEEN, or GLOB after NOT"

      -- IN, LIKE, BETWEEN, GLOB
      let inOp ← optional (keyword "IN")
      if inOp.isSome then return some ("IN", .compare, false)
      let likeOp ← optional (keyword "LIKE")
      if likeOp.isSome then return some ("LIKE", .compare, false)
      let betweenOp ← optional (keyword "BETWEEN")
      if betweenOp.isSome then return some ("BETWEEN", .compare, false)
      let globOp ← optional (keyword "GLOB")
      if globOp.isSome then return some ("GLOB", .compare, false)

      -- <>, !=, <=, >=, <, >, =
      let neq ← optional (symbol "<>" <|> symbol "!=")
      if neq.isSome then return some ("<>", .compare, false)
      let lte ← optional (symbol "<=")
      if lte.isSome then return some ("<=", .compare, false)
      let gte ← optional (symbol ">=")
      if gte.isSome then return some (">=", .compare, false)
      let lt ← optional (symbol "<")
      if lt.isSome then return some ("<", .compare, false)
      let gt ← optional (symbol ">")
      if gt.isSome then return some (">", .compare, false)
      let eq ← optional (symbol "=")
      if eq.isSome then return some ("=", .compare, false)

    -- Concatenation (precedence 5)
    if minPrec <= .concat then
      let concatOp ← optional (symbol "||")
      if concatOp.isSome then return some ("||", .concat, false)

    -- Add/Sub (precedence 6)
    if minPrec <= .addSub then
      let addOp ← optional (symbol "+")
      if addOp.isSome then return some ("+", .addSub, false)
      let subOp ← optional (symbol "-")
      if subOp.isSome then return some ("-", .addSub, false)

    -- Mul/Div/Mod (precedence 7)
    if minPrec <= .mulDiv then
      let mulOp ← optional (symbol "*")
      if mulOp.isSome then return some ("*", .mulDiv, false)
      let divOp ← optional (symbol "/")
      if divOp.isSome then return some ("/", .mulDiv, false)
      let modOp ← optional (symbol "%")
      if modOp.isSome then return some ("%", .mulDiv, false)

    pure none

  applyOp (op : String) (left right : Expr) : Expr :=
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
    | "NOT GLOB" => .unary .not (.binary .glob left right)
    | "IS" => .binary .is left right
    | "IS NOT" => .binary .isNot left right
    | "IS NULL" => .unary .isNull left
    | "IS NOT NULL" => .unary .isNotNull left
    | _ => left  -- Fallback

/-- Parse BETWEEN expression continuation -/
def parseBetween (left : Expr) : Parser Expr := do
  let lower ← parseExprPrec .addSub
  let _ ← keyword "AND"
  let upper ← parseExprPrec .addSub
  pure (.between left lower upper)

/-- Parse IN expression continuation -/
def parseIn (left : Expr) : Parser Expr := do
  lparen
  -- Try subquery first
  let subq ← optional (tryP parseSelectCore)
  match subq with
  | some sel =>
    rparen
    pure (.inSubquery left sel)
  | none =>
    let values ← sepBy1 (parseExprPrec .or_) comma
    rparen
    pure (.inValues left values)

/-- Parse NOT IN expression continuation -/
def parseNotIn (left : Expr) : Parser Expr := do
  lparen
  let subq ← optional (tryP parseSelectCore)
  match subq with
  | some sel =>
    rparen
    pure (.notInSubquery left sel)
  | none =>
    let values ← sepBy1 (parseExprPrec .or_) comma
    rparen
    pure (.notInValues left values)

/-- Parse expression (entry point) -/
def parseExpr : Parser Expr := do
  skipWs
  parseExprPrec .or_

end ExprParser

/-- Parse SQL expression -/
def parseExpr : Parser Expr := ExprParser.parseExpr

end Chisel.Parser
