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

/-!
## Mutually Recursive SQL AST Types

`Expr`, `TableRef`, and `SelectCore` are mutually recursive to support subqueries:
- `Expr.subquery` contains a `SelectCore` (scalar subquery: WHERE x = (SELECT ...))
- `TableRef.subquery` contains a `SelectCore` (derived table: FROM (SELECT ...) AS t)
- `SelectCore` contains `Expr` and `TableRef` in its clauses
-/

mutual

/-- SQL expression AST -/
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
  | inSubquery (expr : Expr) (subquery : SelectCore)
  | notInSubquery (expr : Expr) (subquery : SelectCore)
  | exists_ (subquery : SelectCore)
  | notExists (subquery : SelectCore)
  | case_ (cases : List (Expr × Expr)) (else_ : Option Expr)
  | cast (expr : Expr) (typeName : String)
  | func (name : String) (args : List Expr)
  | agg (func : AggFunc) (expr : Option Expr) (distinct : Bool)
  | param (name : Option String) (index : Option Nat)
  | raw (sql : String)
  | subquery (select : SelectCore)

/-- Table reference (FROM clause) -/
inductive TableRef where
  | table (name : String) (alias_ : Option String)
  | join (type : JoinType) (left right : TableRef) (on : Option Expr)
  | subquery (select : SelectCore) (alias_ : String)

/-- Core SELECT statement (as inductive for mutual recursion) -/
inductive SelectCore where
  | mk
    (distinct : Bool)
    (columns : List SelectItem)
    (from_ : Option TableRef)
    (where_ : Option Expr)
    (groupBy : List Expr)
    (having : Option Expr)
    (orderBy : List OrderItem)
    (limit : Option Nat)
    (offset : Option Nat)

/-- ORDER BY clause item -/
inductive OrderItem where
  | mk (expr : Expr) (dir : SortDir) (nulls : Option NullsOrder)

/-- SELECT column item -/
inductive SelectItem where
  | mk (expr : Expr) (alias_ : Option String)

end

instance : Inhabited Expr := ⟨.lit .null⟩
instance : Inhabited TableRef := ⟨.table "" none⟩
instance : Inhabited SelectCore := ⟨.mk false [] none none [] none [] none none⟩
instance : Inhabited OrderItem := ⟨.mk default .asc none⟩
instance : Inhabited SelectItem := ⟨.mk default none⟩

-- Accessor functions for SelectCore (structure-like interface)
namespace SelectCore

def distinct : SelectCore → Bool
  | .mk d _ _ _ _ _ _ _ _ => d

def columns : SelectCore → List SelectItem
  | .mk _ c _ _ _ _ _ _ _ => c

def from_ : SelectCore → Option TableRef
  | .mk _ _ f _ _ _ _ _ _ => f

def where_ : SelectCore → Option Expr
  | .mk _ _ _ w _ _ _ _ _ => w

def groupBy : SelectCore → List Expr
  | .mk _ _ _ _ g _ _ _ _ => g

def having : SelectCore → Option Expr
  | .mk _ _ _ _ _ h _ _ _ => h

def orderBy : SelectCore → List OrderItem
  | .mk _ _ _ _ _ _ o _ _ => o

def limit : SelectCore → Option Nat
  | .mk _ _ _ _ _ _ _ l _ => l

def offset : SelectCore → Option Nat
  | .mk _ _ _ _ _ _ _ _ o => o

def empty : SelectCore := .mk false [] none none [] none [] none none

-- Fluent setters for SelectCore
def setDistinct (s : SelectCore) (d : Bool) : SelectCore :=
  .mk d s.columns s.from_ s.where_ s.groupBy s.having s.orderBy s.limit s.offset

def setColumns (s : SelectCore) (c : List SelectItem) : SelectCore :=
  .mk s.distinct c s.from_ s.where_ s.groupBy s.having s.orderBy s.limit s.offset

def setFrom (s : SelectCore) (f : Option TableRef) : SelectCore :=
  .mk s.distinct s.columns f s.where_ s.groupBy s.having s.orderBy s.limit s.offset

def setWhere (s : SelectCore) (w : Option Expr) : SelectCore :=
  .mk s.distinct s.columns s.from_ w s.groupBy s.having s.orderBy s.limit s.offset

def setGroupBy (s : SelectCore) (g : List Expr) : SelectCore :=
  .mk s.distinct s.columns s.from_ s.where_ g s.having s.orderBy s.limit s.offset

def setHaving (s : SelectCore) (h : Option Expr) : SelectCore :=
  .mk s.distinct s.columns s.from_ s.where_ s.groupBy h s.orderBy s.limit s.offset

def setOrderBy (s : SelectCore) (o : List OrderItem) : SelectCore :=
  .mk s.distinct s.columns s.from_ s.where_ s.groupBy s.having o s.limit s.offset

def setLimit (s : SelectCore) (l : Option Nat) : SelectCore :=
  .mk s.distinct s.columns s.from_ s.where_ s.groupBy s.having s.orderBy l s.offset

def setOffset (s : SelectCore) (o : Option Nat) : SelectCore :=
  .mk s.distinct s.columns s.from_ s.where_ s.groupBy s.having s.orderBy s.limit o

end SelectCore

-- Accessor functions for OrderItem (structure-like interface)
namespace OrderItem

def expr : OrderItem → Expr
  | .mk e _ _ => e

def dir : OrderItem → SortDir
  | .mk _ d _ => d

def nulls : OrderItem → Option NullsOrder
  | .mk _ _ n => n

end OrderItem

-- Accessor functions for SelectItem (structure-like interface)
namespace SelectItem

def expr : SelectItem → Expr
  | .mk e _ => e

def alias_ : SelectItem → Option String
  | .mk _ a => a

end SelectItem

-- Convenience constructors with defaults
namespace TableRef

/-- Create a simple table reference -/
def simpleTable (name : String) : TableRef := .table name none

/-- Create a table reference with alias -/
def aliasedTable (name alias_ : String) : TableRef := .table name (some alias_)

end TableRef

namespace OrderItem

/-- Create an ascending order item -/
def asc (expr : Expr) : OrderItem := .mk expr .asc none

/-- Create a descending order item -/
def desc (expr : Expr) : OrderItem := .mk expr .desc none

end OrderItem

namespace SelectItem

/-- Create a select item without alias -/
def simple (expr : Expr) : SelectItem := .mk expr none

/-- Create a select item with alias -/
def aliased (expr : Expr) (alias_ : String) : SelectItem := .mk expr (some alias_)

end SelectItem

end Chisel
