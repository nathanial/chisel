/-
  Chisel.Core.Expr
  SQL expressions (columns, operators, functions)
-/
import Chisel.Core.Literal

namespace Chisel

/-- Binary operators -/
inductive BinOp where
  | eq | neq | lt | lte | gt | gte
  | add | sub | mul | div | mod
  | and | or
  | like | notLike | glob
  | inList | notInList
  | is | isNot
  | concat
  deriving Repr, BEq, Inhabited

namespace BinOp

def render : BinOp → String
  | .eq => "="
  | .neq => "<>"
  | .lt => "<"
  | .lte => "<="
  | .gt => ">"
  | .gte => ">="
  | .add => "+"
  | .sub => "-"
  | .mul => "*"
  | .div => "/"
  | .mod => "%"
  | .and => "AND"
  | .or => "OR"
  | .like => "LIKE"
  | .notLike => "NOT LIKE"
  | .glob => "GLOB"
  | .inList => "IN"
  | .notInList => "NOT IN"
  | .is => "IS"
  | .isNot => "IS NOT"
  | .concat => "||"

end BinOp

/-- Unary operators -/
inductive UnaryOp where
  | not | neg | isNull | isNotNull
  deriving Repr, BEq, Inhabited

namespace UnaryOp

def render : UnaryOp → String
  | .not => "NOT"
  | .neg => "-"
  | .isNull => "IS NULL"
  | .isNotNull => "IS NOT NULL"

end UnaryOp

/-- Aggregate functions -/
inductive AggFunc where
  | count | countAll | sum | avg | min | max
  | groupConcat (separator : Option String)
  | total
  deriving Repr, BEq, Inhabited

/-- Sort direction -/
inductive SortDir where
  | asc | desc
  deriving Repr, BEq, Inhabited

/-- NULL handling in ORDER BY -/
inductive NullsOrder where
  | first | last
  deriving Repr, BEq, Inhabited

/-- JOIN type -/
inductive JoinType where
  | inner | left | right | full | cross
  deriving Repr, BEq, Inhabited

/-- SQL expression AST (without subqueries for simplicity) -/
inductive Expr where
  | lit (v : Literal)
  | col (name : String)
  | qualified (table : String) (column : String)
  | star
  | tableStar (table : String)
  | binary (op : BinOp) (left right : Expr)
  | unary (op : UnaryOp) (operand : Expr)
  | between (expr lower upper : Expr)
  | inValues (expr : Expr) (values : List Expr)
  | notInValues (expr : Expr) (values : List Expr)
  | case_ (cases : List (Expr × Expr)) (else_ : Option Expr)
  | cast (expr : Expr) (typeName : String)
  | func (name : String) (args : List Expr)
  | agg (func : AggFunc) (expr : Option Expr) (distinct : Bool)
  | param (name : Option String) (index : Option Nat)
  | raw (sql : String)
  deriving Inhabited

/-- ORDER BY clause item -/
structure OrderItem where
  expr : Expr
  dir : SortDir := .asc
  nulls : Option NullsOrder := none
  deriving Inhabited

/-- SELECT column item -/
structure SelectItem where
  expr : Expr
  alias_ : Option String := none
  deriving Inhabited

/-- Table reference (FROM clause) - without subquery support for now -/
inductive TableRef where
  | table (name : String) (alias_ : Option String := none)
  | join (type : JoinType) (left right : TableRef) (on : Option Expr)
  deriving Inhabited

/-- Core SELECT statement structure -/
structure SelectCore where
  distinct : Bool := false
  columns : List SelectItem := []
  from_ : Option TableRef := none
  where_ : Option Expr := none
  groupBy : List Expr := []
  having : Option Expr := none
  orderBy : List OrderItem := []
  limit : Option Nat := none
  offset : Option Nat := none
  deriving Inhabited

end Chisel
