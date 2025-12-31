/-
  Chisel.Parser.Lexer
  SQL tokenization utilities
-/
import Chisel.Parser.Core

namespace Chisel.Parser

open Parser in

/-- SQL reserved keywords -/
def sqlKeywords : List String := [
  "SELECT", "FROM", "WHERE", "ORDER", "GROUP", "BY", "AS", "JOIN",
  "ON", "AND", "OR", "NOT", "NULL", "TRUE", "FALSE", "IN", "LIKE",
  "BETWEEN", "CASE", "WHEN", "THEN", "ELSE", "END", "CAST", "AS",
  "DISTINCT", "ALL", "UNION", "INTERSECT", "EXCEPT", "INSERT", "INTO",
  "VALUES", "UPDATE", "SET", "DELETE", "CREATE", "TABLE", "INDEX",
  "DROP", "ALTER", "PRIMARY", "KEY", "FOREIGN", "REFERENCES", "UNIQUE",
  "CHECK", "DEFAULT", "HAVING", "LIMIT", "OFFSET", "LEFT", "RIGHT",
  "INNER", "OUTER", "CROSS", "FULL", "NATURAL", "USING", "EXISTS",
  "IS", "ASC", "DESC", "NULLS", "FIRST", "LAST", "CONSTRAINT",
  "AUTOINCREMENT", "STRICT", "TEMPORARY", "TEMP", "IF", "GLOB",
  "RETURNING", "OR", "REPLACE", "IGNORE", "ABORT", "ROLLBACK", "FAIL",
  "ADD", "COLUMN", "RENAME", "TO", "CASCADE", "RESTRICT", "NO", "ACTION",
  "INTEGER", "REAL", "TEXT", "BLOB", "BOOLEAN", "DATETIME", "NUMERIC",
  "VARCHAR", "COLLATE", "COUNT", "SUM", "AVG", "MIN", "MAX", "TOTAL",
  "GROUP_CONCAT", "COALESCE", "NULLIF", "IFNULL", "TYPEOF", "ABS",
  "UPPER", "LOWER", "LENGTH", "SUBSTR", "TRIM", "LTRIM", "RTRIM",
  "REPLACE", "INSTR", "ROUND", "RANDOM", "DATE", "TIME", "STRFTIME",
  "JULIANDAY"
]

/-- Check if string is a SQL keyword -/
def isKeyword (s : String) : Bool :=
  sqlKeywords.contains s.toUpper

namespace Lexer

open Parser

/-- Skip whitespace and SQL comments -/
partial def skipWs : Parser Unit := do
  skipMany space
  let hasComment ← (do
    let s ← get
    let rem := s.remaining
    pure (rem.startsWith "--" || rem.startsWith "/*"))
  if hasComment then
    skipComment
    skipWs
  else
    pure ()
where
  skipComment : Parser Unit := do
    let s ← get
    let rem := s.remaining
    if rem.startsWith "--" then
      skipMany (satisfy (· != '\n') "")
      let _ ← optional (char '\n')
    else if rem.startsWith "/*" then
      let _ ← string "/*"
      skipUntilEnd
    else
      pure ()
  skipUntilEnd : Parser Unit := do
    let s ← get
    if s.remaining.startsWith "*/" then
      let _ ← string "*/"
    else if s.atEnd then
      pure ()
    else
      let _ ← anyChar
      skipUntilEnd

/-- Lexer combinator: parse with surrounding whitespace -/
def lexeme (p : Parser α) : Parser α := do
  let x ← p
  skipWs
  pure x

/-- Parse a symbol (punctuation) -/
def symbol (s : String) : Parser String :=
  lexeme (string s)

/-- Parse a keyword (case-insensitive, must not be followed by alphanumeric) -/
def keyword (kw : String) : Parser String := lexeme do
  let matched ← stringCI kw
  notFollowedBy alphaNum <|> notFollowedBy (char '_')
  pure matched

/-- Convert char list to string -/
private def charsToString (cs : List Char) : String :=
  cs.foldl (fun s c => s.push c) ""

/-- Parse unquoted identifier -/
def identRaw : Parser String := do
  let first ← letter <|> char '_'
  let rest ← many (alphaNum <|> char '_')
  pure (charsToString (first :: rest))

/-- Parse double-quoted identifier "name" -/
def quotedIdent : Parser String := do
  let _ ← char '"'
  let chars ← many (satisfy (· != '"') "")
  let _ ← char '"'
  pure (charsToString chars)

/-- Parse backtick-quoted identifier `name` -/
def backtickIdent : Parser String := do
  let _ ← char '`'
  let chars ← many (satisfy (· != '`') "")
  let _ ← char '`'
  pure (charsToString chars)

/-- Parse square-bracket quoted identifier [name] -/
def bracketIdent : Parser String := do
  let _ ← char '['
  let chars ← many (satisfy (· != ']') "")
  let _ ← char ']'
  pure (charsToString chars)

/-- Parse identifier (not a keyword) -/
def ident : Parser String := lexeme do
  quotedIdent <|> backtickIdent <|> bracketIdent <|> do
    let name ← identRaw
    if isKeyword name then
      fail s!"identifier (got keyword \"{name}\")"
    else
      pure name

/-- Parse identifier or keyword as identifier -/
def identOrKeyword : Parser String := lexeme do
  quotedIdent <|> backtickIdent <|> bracketIdent <|> identRaw

/-- Parse integer literal -/
def intLit : Parser Int := lexeme do
  let neg ← optional (char '-')
  let digits ← many1 digit
  let n := digits.foldl (fun acc d => acc * 10 + (d.toNat - '0'.toNat)) 0
  pure (match neg with | some _ => -n | none => n)

/-- Convert digit chars to Nat -/
private def digitsToNat (ds : List Char) : Nat :=
  ds.foldl (fun acc d => acc * 10 + (d.toNat - '0'.toNat)) 0

/-- Parse float literal -/
def floatLit : Parser Float := lexeme do
  let neg ← optional (char '-')
  let intPart ← many1 digit
  let _ ← char '.'
  let fracPart ← many1 digit
  let intVal := digitsToNat intPart
  let fracVal := digitsToNat fracPart
  let fracLen := fracPart.length
  let divisor := (10 : Float) ^ fracLen.toFloat
  let f := intVal.toFloat + fracVal.toFloat / divisor
  pure (match neg with | some _ => -f | none => f)

/-- Parse number (int or float) -/
def number : Parser (Int ⊕ Float) := lexeme do
  let neg ← optional (char '-')
  let intPart ← many1 digit
  let fracOpt ← optional (char '.' *> many1 digit)
  match fracOpt with
  | some fracPart =>
    let intVal := digitsToNat intPart
    let fracVal := digitsToNat fracPart
    let fracLen := fracPart.length
    let divisor := (10 : Float) ^ fracLen.toFloat
    let f := intVal.toFloat + fracVal.toFloat / divisor
    pure (.inr (match neg with | some _ => -f | none => f))
  | none =>
    let n := digitsToNat intPart
    pure (.inl (match neg with | some _ => -n | none => n))

/-- Parse string literal 'text' with '' escape -/
partial def stringLit : Parser String := lexeme do
  let _ ← char '\''
  let chars ← parseContent []
  pure (charsToString chars.reverse)
where
  parseContent (acc : List Char) : Parser (List Char) := do
    let c ← anyChar
    if c == '\'' then
      let next ← optional (char '\'')
      match next with
      | some _ => parseContent ('\'' :: acc)
      | none => pure acc
    else
      parseContent (c :: acc)

/-- Parse blob literal X'hex' -/
def blobLit : Parser ByteArray := lexeme do
  let _ ← char 'X' <|> char 'x'
  let _ ← char '\''
  let hexChars ← many (satisfy (fun c => c.isDigit || "abcdefABCDEF".any (· == c)) "hex digit")
  let _ ← char '\''
  let hexStr := charsToString hexChars
  pure (hexToBytes hexStr)
where
  hexToBytes (s : String) : ByteArray :=
    let chars := s.toList
    let rec go (i : Nat) (acc : Array UInt8) : Array UInt8 :=
      if h : i + 1 < chars.length then
        let c1 := chars[i]'(Nat.lt_of_succ_lt h)
        let c2 := chars[i + 1]
        go (i + 2) (acc.push (hexPair c1 c2))
      else acc
    ByteArray.mk (go 0 #[])
  hexPair (c1 c2 : Char) : UInt8 :=
    let n1 := hexDigit c1
    let n2 := hexDigit c2
    (n1 * 16 + n2).toUInt8
  -- TODO: Replace with Staple.Hex.hexCharToNat after staple release
  hexDigit (c : Char) : Nat :=
    if c.isDigit then c.toNat - '0'.toNat
    else if c >= 'a' && c <= 'f' then c.toNat - 'a'.toNat + 10
    else if c >= 'A' && c <= 'F' then c.toNat - 'A'.toNat + 10
    else 0

/-- Parse NULL literal -/
def nullLit : Parser Unit :=
  keyword "NULL" *> pure ()

/-- Parse TRUE literal -/
def trueLit : Parser Unit :=
  keyword "TRUE" *> pure ()

/-- Parse FALSE literal -/
def falseLit : Parser Unit :=
  keyword "FALSE" *> pure ()

/-- Parse positional parameter ? -/
def positionalParam : Parser Unit := do
  let _ ← lexeme (char '?')
  pure ()

/-- Parse indexed parameter $N -/
def indexedParam : Parser Nat := lexeme do
  let _ ← char '$'
  nat

/-- Parse named parameter :name -/
def colonParam : Parser String := lexeme do
  let _ ← char ':'
  identRaw

/-- Parse named parameter @name -/
def atParam : Parser String := lexeme do
  let _ ← char '@'
  identRaw

/-- Parse opening paren -/
def lparen : Parser Unit := symbol "(" *> pure ()

/-- Parse closing paren -/
def rparen : Parser Unit := symbol ")" *> pure ()

/-- Parse comma -/
def comma : Parser Unit := symbol "," *> pure ()

/-- Parse semicolon -/
def semicolon : Parser Unit := symbol ";" *> pure ()

/-- Parse dot -/
def dot : Parser Unit := symbol "." *> pure ()

/-- Parse star -/
def star : Parser Unit := symbol "*" *> pure ()

end Lexer

end Chisel.Parser
