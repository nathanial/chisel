/-
  Chisel.Core.Literal
  SQL literal values
-/

namespace Chisel

/-- SQL literal values -/
inductive Literal where
  | null
  | bool (b : Bool)
  | int (n : Int)
  | float (f : Float)
  | string (s : String)
  | blob (b : ByteArray)
  deriving BEq, Inhabited

instance : Repr Literal where
  reprPrec
    | .null, _ => "Literal.null"
    | .bool b, _ => s!"Literal.bool {repr b}"
    | .int n, _ => s!"Literal.int {repr n}"
    | .float f, _ => s!"Literal.float {repr f}"
    | .string s, _ => s!"Literal.string {repr s}"
    | .blob b, _ => s!"Literal.blob (size={b.size})"

namespace Literal

/-- Escape single quotes in SQL strings -/
private def escapeString (s : String) : String :=
  s.replace "'" "''"

/-- Convert byte to hex digit -/
private def hexDigit (n : Nat) : Char :=
  if n < 10 then Char.ofNat (48 + n) else Char.ofNat (55 + n)

/-- Convert ByteArray to hex string -/
private def toHex (b : ByteArray) : String :=
  b.foldl (fun acc byte =>
    let hi := byte.toNat / 16
    let lo := byte.toNat % 16
    acc ++ String.singleton (hexDigit hi) ++ String.singleton (hexDigit lo)
  ) ""

/-- Render literal to SQL string -/
def render : Literal → String
  | .null => "NULL"
  | .bool true => "TRUE"
  | .bool false => "FALSE"
  | .int n => toString n
  | .float f => toString f
  | .string s => s!"'{escapeString s}'"
  | .blob b => s!"X'{toHex b}'"

end Literal

end Chisel
