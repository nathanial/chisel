/-
  Chisel.Parser.Core
  Parser monad and basic combinators
-/

namespace Chisel.Parser

/-- Position in source text -/
structure ParsePos where
  offset : Nat := 0
  line : Nat := 1
  column : Nat := 1
  deriving Repr, BEq, Inhabited

/-- Advance position by one character -/
def ParsePos.advance (pos : ParsePos) (c : Char) : ParsePos :=
  if c == '\n' then
    { offset := pos.offset + 1, line := pos.line + 1, column := 1 }
  else
    { offset := pos.offset + 1, line := pos.line, column := pos.column + 1 }

/-- Advance position by a string -/
def ParsePos.advanceStr (pos : ParsePos) (s : String) : ParsePos :=
  s.foldl ParsePos.advance pos

/-- Parse error with position and context -/
structure ParseError where
  pos : ParsePos
  expected : List String
  found : String
  deriving Repr, Inhabited

instance : ToString ParseError where
  toString e :=
    let expectedStr := String.intercalate " or " e.expected
    s!"Parse error at line {e.pos.line}, column {e.pos.column}: expected {expectedStr}, found {e.found}"

/-- Parser state -/
structure ParseState where
  input : String
  pos : ParsePos := {}
  deriving Inhabited

/-- Get remaining input -/
def ParseState.remaining (s : ParseState) : String :=
  s.input.drop s.pos.offset

/-- Check if at end of input -/
def ParseState.atEnd (s : ParseState) : Bool :=
  s.pos.offset >= s.input.length

/-- Peek at current character -/
def ParseState.peek (s : ParseState) : Option Char :=
  s.input.get? ⟨s.pos.offset⟩

/-- Parser monad -/
abbrev Parser (α : Type) := StateT ParseState (Except ParseError) α

namespace Parser

/-- Run a parser on input string -/
def run (p : Parser α) (input : String) : Except ParseError α :=
  match StateT.run p (ParseState.mk input {}) with
  | .ok (a, _) => .ok a
  | .error e => .error e

/-- Run parser, returning result and remaining state -/
def runFull (p : Parser α) (input : String) : Except ParseError (α × ParseState) :=
  StateT.run p (ParseState.mk input {})

/-- Get current position -/
def getPos : Parser ParsePos := do
  return (← get).pos

/-- Get remaining input -/
def remaining : Parser String := do
  return (← get).remaining

/-- Check if at end of input -/
def atEnd : Parser Bool := do
  return (← get).atEnd

/-- Fail with expected message -/
def fail (expected : String) : Parser α := do
  let s ← get
  let found := match s.peek with
    | some c => s!"\'{c}\'"
    | none => "end of input"
  throw { pos := s.pos, expected := [expected], found }

/-- Satisfy a character predicate -/
def satisfy (pred : Char → Bool) (desc : String := "character") : Parser Char := do
  let s ← get
  match s.peek with
  | none => fail desc
  | some c =>
    if pred c then
      set { s with pos := s.pos.advance c }
      pure c
    else
      fail desc

/-- Parse specific character -/
def char (c : Char) : Parser Char :=
  satisfy (· == c) s!"\'{c}\'"

/-- Parse specific string (case-sensitive) -/
def string (str : String) : Parser String := do
  let s ← get
  let rem := s.remaining
  if rem.startsWith str then
    set { s with pos := s.pos.advanceStr str }
    pure str
  else
    fail s!"\"{str}\""

/-- Parse string case-insensitively -/
def stringCI (str : String) : Parser String := do
  let s ← get
  let rem := s.remaining
  let upper := str.toUpper
  let pfx := rem.take str.length
  if pfx.toUpper == upper then
    set { s with pos := s.pos.advanceStr pfx }
    pure pfx
  else
    fail s!"\"{str}\""

/-- Try a parser, backtracking on failure -/
def tryP (p : Parser α) : Parser α := do
  let s ← get
  try
    p
  catch e =>
    set s
    throw e

/-- Try parser, return none on failure (with backtrack) -/
def optional (p : Parser α) : Parser (Option α) := do
  let s ← get
  try
    some <$> p
  catch _ =>
    set s
    pure none

/-- Choice between two parsers -/
def orElse (p1 : Parser α) (p2 : Unit → Parser α) : Parser α := do
  let s ← get
  try
    p1
  catch e1 =>
    set s
    try
      p2 ()
    catch e2 =>
      throw { e1 with expected := e1.expected ++ e2.expected }

instance : OrElse (Parser α) where
  orElse := orElse

/-- Choice between multiple parsers -/
def choice (ps : List (Parser α)) : Parser α :=
  match ps with
  | [] => fail "no alternatives"
  | [p] => p
  | p :: rest => p <|> choice rest

/-- Parse zero or more occurrences -/
partial def many (p : Parser α) : Parser (List α) := do
  let s ← get
  try
    let x ← p
    let xs ← many p
    pure (x :: xs)
  catch _ =>
    set s
    pure []

/-- Parse one or more occurrences -/
def many1 (p : Parser α) : Parser (List α) := do
  let x ← p
  let xs ← many p
  pure (x :: xs)

/-- Parse separated list (zero or more) -/
def sepBy (p : Parser α) (sep : Parser β) : Parser (List α) := do
  sepBy1 p sep <|> pure []
where
  sepBy1 (p : Parser α) (sep : Parser β) : Parser (List α) := do
    let x ← p
    let xs ← many (sep *> p)
    pure (x :: xs)

/-- Parse separated list (one or more) -/
def sepBy1 (p : Parser α) (sep : Parser β) : Parser (List α) := do
  let x ← p
  let xs ← many (sep *> p)
  pure (x :: xs)

/-- Parse between delimiters -/
def between (open_ : Parser α) (close : Parser β) (p : Parser γ) : Parser γ := do
  let _ ← open_
  let x ← p
  let _ ← close
  pure x

/-- Skip zero or more occurrences -/
partial def skipMany (p : Parser α) : Parser Unit := do
  let s ← get
  try
    let _ ← p
    skipMany p
  catch _ =>
    set s
    pure ()

/-- Skip one or more occurrences -/
def skipMany1 (p : Parser α) : Parser Unit := do
  let _ ← p
  skipMany p

/-- Look ahead without consuming -/
def lookAhead (p : Parser α) : Parser α := do
  let s ← get
  let result ← p
  set s
  pure result

/-- Negative lookahead -/
def notFollowedBy (p : Parser α) : Parser Unit := do
  let s ← get
  let succeeded ← (do let _ ← p; pure true) <|> pure false
  set s
  if succeeded then
    fail "unexpected"
  else
    pure ()

/-- End of input -/
def eof : Parser Unit := do
  if ← atEnd then
    pure ()
  else
    fail "end of input"

/-- Parse any single character -/
def anyChar : Parser Char :=
  satisfy (fun _ => true) "any character"

/-- Parse digit -/
def digit : Parser Char :=
  satisfy Char.isDigit "digit"

/-- Parse letter -/
def letter : Parser Char :=
  satisfy Char.isAlpha "letter"

/-- Parse alphanumeric -/
def alphaNum : Parser Char :=
  satisfy Char.isAlphanum "alphanumeric character"

/-- Parse whitespace character -/
def space : Parser Char :=
  satisfy Char.isWhitespace "whitespace"

/-- Skip whitespace -/
def spaces : Parser Unit :=
  skipMany space

/-- Parse natural number -/
def nat : Parser Nat := do
  let digits ← many1 digit
  pure (digits.foldl (fun acc d => acc * 10 + (d.toNat - '0'.toNat)) 0)

/-- Parse integer (with optional sign) -/
def int : Parser Int := do
  let sign ← optional (char '-')
  let n ← nat
  pure (match sign with | some _ => -n | none => n)

end Parser

end Chisel.Parser
