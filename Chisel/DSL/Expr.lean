/-
  Chisel.DSL.Expr
  Convenient expression constructors and operators
-/
import Chisel.Core.Select

namespace Chisel

-- Column references
/-- Unqualified column reference -/
def col (name : String) : Expr := .col name

/-- Qualified column reference (table.column) -/
def col' (table : String) (column : String) : Expr := .qualified table column

-- Literals
/-- NULL literal -/
def null : Expr := .lit .null

/-- Integer literal -/
def val (n : Int) : Expr := .lit (.int n)

/-- Float literal -/
def valF (f : Float) : Expr := .lit (.float f)

/-- String literal -/
def str (s : String) : Expr := .lit (.string s)

/-- Boolean literal -/
def bool (b : Bool) : Expr := .lit (.bool b)

/-- Blob literal -/
def blob (b : ByteArray) : Expr := .lit (.blob b)

-- Parameters (for prepared statements)
/-- Positional parameter (?) -/
def param : Expr := .param none none

/-- Named parameter (:name or @name) -/
def namedParam (name : String) : Expr := .param (some name) none

/-- Indexed parameter ($1, $2, etc.) -/
def indexedParam (idx : Nat) : Expr := .param none (some idx)

-- Comparison operators
def eq (a b : Expr) : Expr := .binary .eq a b
def neq (a b : Expr) : Expr := .binary .neq a b
def lt (a b : Expr) : Expr := .binary .lt a b
def lte (a b : Expr) : Expr := .binary .lte a b
def gt (a b : Expr) : Expr := .binary .gt a b
def gte (a b : Expr) : Expr := .binary .gte a b

-- Infix comparison operators (prefixed with . to avoid conflicts)
scoped infixl:50 " .== " => eq
scoped infixl:50 " .!= " => neq
scoped infixl:50 " .< " => lt
scoped infixl:50 " .<= " => lte
scoped infixl:50 " .> " => gt
scoped infixl:50 " .>= " => gte

-- Logical operators
def and_ (a b : Expr) : Expr := .binary .and a b
def or_ (a b : Expr) : Expr := .binary .or a b
def not_ (a : Expr) : Expr := .unary .not a

scoped infixl:35 " .&& " => and_
scoped infixl:30 " .|| " => or_

-- Arithmetic operators
def add (a b : Expr) : Expr := .binary .add a b
def sub (a b : Expr) : Expr := .binary .sub a b
def mul (a b : Expr) : Expr := .binary .mul a b
def div (a b : Expr) : Expr := .binary .div a b
def mod (a b : Expr) : Expr := .binary .mod a b
def neg (a : Expr) : Expr := .unary .neg a

scoped infixl:65 " .+ " => add
scoped infixl:65 " .- " => sub
scoped infixl:70 " .* " => mul
scoped infixl:70 " ./ " => div
scoped infixl:70 " .% " => mod

-- String concatenation
def concat (a b : Expr) : Expr := .binary .concat a b

-- Note: SQL uses || for concatenation, but we use .|| for OR in this DSL.
-- Use the `concat` function explicitly for string concatenation.

-- Pattern matching
def like (a : Expr) (pattern : String) : Expr := .binary .like a (str pattern)
def notLike (a : Expr) (pattern : String) : Expr := .binary .notLike a (str pattern)
def glob (a : Expr) (pattern : String) : Expr := .binary .glob a (str pattern)

-- NULL handling
def isNull (a : Expr) : Expr := .unary .isNull a
def isNotNull (a : Expr) : Expr := .unary .isNotNull a
def coalesce (exprs : List Expr) : Expr := .func "COALESCE" exprs
def nullif (a b : Expr) : Expr := .func "NULLIF" [a, b]
def ifnull (a b : Expr) : Expr := .func "IFNULL" [a, b]

-- BETWEEN
def between (e lower upper : Expr) : Expr := .between e lower upper

-- IN (value list)
def in_ (e : Expr) (values : List Expr) : Expr := .inValues e values
def notIn (e : Expr) (values : List Expr) : Expr := .notInValues e values

-- CASE WHEN
def case_ (cases : List (Expr × Expr)) (else_ : Option Expr := none) : Expr :=
  .case_ cases else_

def caseWhen (cond : Expr) (then_ : Expr) : Expr × Expr := (cond, then_)

-- Aggregate functions
def count (e : Expr) : Expr := .agg .count (some e) false
def countAll : Expr := .agg .countAll none false
def countDistinct (e : Expr) : Expr := .agg .count (some e) true
def sum (e : Expr) : Expr := .agg .sum (some e) false
def sumDistinct (e : Expr) : Expr := .agg .sum (some e) true
def avg (e : Expr) : Expr := .agg .avg (some e) false
def avgDistinct (e : Expr) : Expr := .agg .avg (some e) true
def min_ (e : Expr) : Expr := .agg .min (some e) false
def max_ (e : Expr) : Expr := .agg .max (some e) false
def total (e : Expr) : Expr := .agg .total (some e) false
def groupConcat (e : Expr) (separator : Option String := none) : Expr :=
  .agg (.groupConcat separator) (some e) false

-- Common SQL functions
def abs (e : Expr) : Expr := .func "ABS" [e]
def upper (e : Expr) : Expr := .func "UPPER" [e]
def lower (e : Expr) : Expr := .func "LOWER" [e]
def length (e : Expr) : Expr := .func "LENGTH" [e]
def substr (e : Expr) (start : Expr) (len : Option Expr := none) : Expr :=
  match len with
  | some l => .func "SUBSTR" [e, start, l]
  | none => .func "SUBSTR" [e, start]
def trim (e : Expr) : Expr := .func "TRIM" [e]
def ltrim (e : Expr) : Expr := .func "LTRIM" [e]
def rtrim (e : Expr) : Expr := .func "RTRIM" [e]
def replace (e : Expr) (from_ to : String) : Expr := .func "REPLACE" [e, str from_, str to]
def instr (haystack needle : Expr) : Expr := .func "INSTR" [haystack, needle]

-- Date/time functions
def now : Expr := .func "DATETIME" [str "now"]
def date (e : Expr) : Expr := .func "DATE" [e]
def time (e : Expr) : Expr := .func "TIME" [e]
def datetime (e : Expr) : Expr := .func "DATETIME" [e]
def julianday (e : Expr) : Expr := .func "JULIANDAY" [e]
def strftime (format : String) (e : Expr) : Expr := .func "STRFTIME" [str format, e]

-- Math functions
def round (e : Expr) (digits : Option Nat := none) : Expr :=
  match digits with
  | some d => .func "ROUND" [e, val (Int.ofNat d)]
  | none => .func "ROUND" [e]
def random : Expr := .func "RANDOM" []
def max2 (a b : Expr) : Expr := .func "MAX" [a, b]
def min2 (a b : Expr) : Expr := .func "MIN" [a, b]

-- Type conversion
def cast (e : Expr) (typeName : String) : Expr := .cast e typeName
def typeof (e : Expr) : Expr := .func "TYPEOF" [e]

-- Generic function call
def func (name : String) (args : List Expr) : Expr := .func name args

-- Raw SQL escape hatch
def raw (sql : String) : Expr := .raw sql

-- Star expressions
def star : Expr := .star
def tableStar (table : String) : Expr := .tableStar table

end Chisel
