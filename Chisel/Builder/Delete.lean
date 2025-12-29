/-
  Chisel.Builder.Delete
  Fluent DELETE statement builder
-/
import Chisel.Core.DML

namespace Chisel

/-- DELETE builder with method chaining -/
structure DeleteBuilder where
  stmt : DeleteStmt
  deriving Inhabited

namespace DeleteBuilder

/-- Create new DELETE builder -/
def new (table : String) : DeleteBuilder :=
  { stmt := { table } }

/-- Set table alias -/
def alias_ (db : DeleteBuilder) (a : String) : DeleteBuilder :=
  { db with stmt := { db.stmt with alias_ := some a }}

/-- Add WHERE condition (ANDs with existing) -/
def where_ (db : DeleteBuilder) (cond : Expr) : DeleteBuilder :=
  { db with stmt := { db.stmt with
    where_ := match db.stmt.where_ with
      | none => some cond
      | some existing => some (.binary .and existing cond) }}

/-- Add RETURNING clause -/
def returning (db : DeleteBuilder) (items : List SelectItem) : DeleteBuilder :=
  { db with stmt := { db.stmt with returning := db.stmt.returning ++ items }}

/-- Add RETURNING column -/
def returning1 (db : DeleteBuilder) (expr : Expr) (alias_ : Option String := none) : DeleteBuilder :=
  db.returning [SelectItem.mk expr alias_]

/-- RETURNING * -/
def returningAll (db : DeleteBuilder) : DeleteBuilder :=
  db.returning1 .star

/-- Build the DELETE statement -/
def build (db : DeleteBuilder) : DeleteStmt := db.stmt

end DeleteBuilder

/-- Create DELETE builder -/
def deleteFrom (table : String) : DeleteBuilder := DeleteBuilder.new table

end Chisel
