/-
  Chisel.Builder.Update
  Fluent UPDATE statement builder
-/
import Chisel.Core.DML

namespace Chisel

/-- UPDATE builder with method chaining -/
structure UpdateBuilder where
  stmt : UpdateStmt
  deriving Inhabited

namespace UpdateBuilder

/-- Create new UPDATE builder -/
def new (table : String) : UpdateBuilder :=
  { stmt := { table } }

/-- Set table alias -/
def alias_ (ub : UpdateBuilder) (a : String) : UpdateBuilder :=
  { ub with stmt := { ub.stmt with alias_ := some a }}

/-- Add SET assignment -/
def set (ub : UpdateBuilder) (column : String) (value : Expr) : UpdateBuilder :=
  { ub with stmt := { ub.stmt with set := ub.stmt.set ++ [{ column, value }] }}

/-- Add multiple SET assignments -/
def setMany (ub : UpdateBuilder) (assignments : List (String × Expr)) : UpdateBuilder :=
  let asns := assignments.map fun (c, v) => { column := c, value := v : Assignment }
  { ub with stmt := { ub.stmt with set := ub.stmt.set ++ asns }}

/-- Set FROM clause (for UPDATE ... FROM) -/
def from_ (ub : UpdateBuilder) (table : TableRef) : UpdateBuilder :=
  { ub with stmt := { ub.stmt with from_ := some table }}

/-- Set FROM table -/
def fromTable (ub : UpdateBuilder) (table : String) (alias_ : Option String := none) : UpdateBuilder :=
  ub.from_ (.table table alias_)

/-- Add WHERE condition (ANDs with existing) -/
def where_ (ub : UpdateBuilder) (cond : Expr) : UpdateBuilder :=
  { ub with stmt := { ub.stmt with
    where_ := match ub.stmt.where_ with
      | none => some cond
      | some existing => some (.binary .and existing cond) }}

/-- Add RETURNING clause -/
def returning (ub : UpdateBuilder) (items : List SelectItem) : UpdateBuilder :=
  { ub with stmt := { ub.stmt with returning := ub.stmt.returning ++ items }}

/-- Add RETURNING column -/
def returning1 (ub : UpdateBuilder) (expr : Expr) (alias_ : Option String := none) : UpdateBuilder :=
  ub.returning [SelectItem.mk expr alias_]

/-- RETURNING * -/
def returningAll (ub : UpdateBuilder) : UpdateBuilder :=
  ub.returning1 .star

/-- Build the UPDATE statement -/
def build (ub : UpdateBuilder) : UpdateStmt := ub.stmt

end UpdateBuilder

/-- Create UPDATE builder -/
def update (table : String) : UpdateBuilder := UpdateBuilder.new table

end Chisel
