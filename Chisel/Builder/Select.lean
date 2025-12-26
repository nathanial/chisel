/-
  Chisel.Builder.Select
  Monadic SELECT query builder
-/
import Chisel.Core.Select

namespace Chisel

/-- SELECT builder state -/
structure SelectState where
  stmt : SelectStmt := {}
  deriving Inhabited

/-- SELECT builder monad -/
abbrev SelectM := StateM SelectState

namespace SelectM

/-- Build SELECT statement from monadic builder -/
def build (m : SelectM Unit) : SelectStmt :=
  let (_, state) := m.run {}
  state.stmt

/-- Build SELECT statement, returning the final state -/
def buildWith (m : SelectM α) : α × SelectStmt :=
  let (a, state) := m.run {}
  (a, state.stmt)

end SelectM

/-- Add column to SELECT clause -/
def select_ (e : Expr) (alias_ : Option String := none) : SelectM Unit :=
  modify fun s => { s with stmt := { s.stmt with
    columns := s.stmt.columns ++ [{ expr := e, alias_ }] }}

/-- SELECT * -/
def selectAll : SelectM Unit :=
  select_ .star

/-- SELECT table.* -/
def selectTableStar (table : String) : SelectM Unit :=
  select_ (.tableStar table)

/-- Mark SELECT as DISTINCT -/
def distinct : SelectM Unit :=
  modify fun s => { s with stmt := { s.stmt with distinct := true }}

/-- Set FROM table -/
def from_ (table : String) (alias_ : Option String := none) : SelectM Unit :=
  modify fun s => { s with stmt := { s.stmt with
    from_ := some (.table table alias_) }}

/-- Add WHERE condition (ANDs with existing) -/
def where_ (cond : Expr) : SelectM Unit :=
  modify fun s => { s with stmt := { s.stmt with
    where_ := match s.stmt.where_ with
      | none => some cond
      | some existing => some (.binary .and existing cond) }}

/-- Set WHERE condition (replaces existing) -/
def whereReplace (cond : Expr) : SelectM Unit :=
  modify fun s => { s with stmt := { s.stmt with where_ := some cond }}

/-- Add OR condition to WHERE -/
def orWhere (cond : Expr) : SelectM Unit :=
  modify fun s => { s with stmt := { s.stmt with
    where_ := match s.stmt.where_ with
      | none => some cond
      | some existing => some (.binary .or existing cond) }}

/-- Add JOIN -/
def join_ (type : JoinType) (table : String) (alias_ : Option String := none)
    (on : Option Expr := none) : SelectM Unit :=
  modify fun s => { s with stmt := { s.stmt with
    from_ := match s.stmt.from_ with
      | none => some (.table table alias_)
      | some left => some (.join type left (.table table alias_) on) }}

/-- INNER JOIN shorthand -/
def innerJoin (table : String) (on : Expr) (alias_ : Option String := none) : SelectM Unit :=
  join_ .inner table alias_ (some on)

/-- LEFT JOIN shorthand -/
def leftJoin (table : String) (on : Expr) (alias_ : Option String := none) : SelectM Unit :=
  join_ .left table alias_ (some on)

/-- RIGHT JOIN shorthand -/
def rightJoin (table : String) (on : Expr) (alias_ : Option String := none) : SelectM Unit :=
  join_ .right table alias_ (some on)

/-- CROSS JOIN shorthand -/
def crossJoin (table : String) (alias_ : Option String := none) : SelectM Unit :=
  join_ .cross table alias_ none

/-- Add GROUP BY expressions -/
def groupBy_ (exprs : List Expr) : SelectM Unit :=
  modify fun s => { s with stmt := { s.stmt with
    groupBy := s.stmt.groupBy ++ exprs }}

/-- Add single GROUP BY expression -/
def groupBy1 (expr : Expr) : SelectM Unit :=
  groupBy_ [expr]

/-- Set HAVING condition -/
def having_ (cond : Expr) : SelectM Unit :=
  modify fun s => { s with stmt := { s.stmt with
    having := match s.stmt.having with
      | none => some cond
      | some existing => some (.binary .and existing cond) }}

/-- Add ORDER BY items -/
def orderBy_ (items : List OrderItem) : SelectM Unit :=
  modify fun s => { s with stmt := { s.stmt with
    orderBy := s.stmt.orderBy ++ items }}

/-- Add single ORDER BY expression -/
def orderBy1 (expr : Expr) (dir : SortDir := .asc) (nulls : Option NullsOrder := none) : SelectM Unit :=
  orderBy_ [{ expr, dir, nulls }]

/-- Order by ascending -/
def orderAsc (expr : Expr) : SelectM Unit :=
  orderBy1 expr .asc

/-- Order by descending -/
def orderDesc (expr : Expr) : SelectM Unit :=
  orderBy1 expr .desc

/-- Set LIMIT -/
def limit_ (n : Nat) : SelectM Unit :=
  modify fun s => { s with stmt := { s.stmt with limit := some n }}

/-- Set OFFSET -/
def offset_ (n : Nat) : SelectM Unit :=
  modify fun s => { s with stmt := { s.stmt with offset := some n }}

/-- Set LIMIT and OFFSET together -/
def paginate (pageSize : Nat) (page : Nat := 0) : SelectM Unit := do
  limit_ pageSize
  if page > 0 then
    offset_ (pageSize * page)

end Chisel
