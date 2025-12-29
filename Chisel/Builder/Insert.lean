/-
  Chisel.Builder.Insert
  Fluent INSERT statement builder
-/
import Chisel.Core.DML

namespace Chisel

/-- INSERT builder with method chaining -/
structure InsertBuilder where
  stmt : InsertStmt
  deriving Inhabited

namespace InsertBuilder

/-- Create new INSERT builder -/
def new (table : String) : InsertBuilder :=
  { stmt := { table } }

/-- Set columns to insert -/
def columns (ib : InsertBuilder) (cols : List String) : InsertBuilder :=
  { ib with stmt := { ib.stmt with columns := cols }}

/-- Add a row of values -/
def values (ib : InsertBuilder) (vals : List Expr) : InsertBuilder :=
  { ib with stmt := { ib.stmt with values := ib.stmt.values ++ [vals] }}

/-- Add multiple rows -/
def valuesMany (ib : InsertBuilder) (rows : List (List Expr)) : InsertBuilder :=
  { ib with stmt := { ib.stmt with values := ib.stmt.values ++ rows }}

/-- Insert from SELECT -/
def fromSelect (ib : InsertBuilder) (sel : SelectStmt) : InsertBuilder :=
  { ib with stmt := { ib.stmt with fromSelect := some sel }}

/-- Set conflict action (OR IGNORE, OR REPLACE, etc.) -/
def onConflict (ib : InsertBuilder) (action : ConflictAction) : InsertBuilder :=
  { ib with stmt := { ib.stmt with onConflict := some action }}

/-- OR IGNORE shorthand -/
def orIgnore (ib : InsertBuilder) : InsertBuilder :=
  ib.onConflict .ignore

/-- OR REPLACE shorthand -/
def orReplace (ib : InsertBuilder) : InsertBuilder :=
  ib.onConflict .replace

/-- Add RETURNING clause -/
def returning (ib : InsertBuilder) (items : List SelectItem) : InsertBuilder :=
  { ib with stmt := { ib.stmt with returning := ib.stmt.returning ++ items }}

/-- Add RETURNING column -/
def returning1 (ib : InsertBuilder) (expr : Expr) (alias_ : Option String := none) : InsertBuilder :=
  ib.returning [SelectItem.mk expr alias_]

/-- RETURNING * -/
def returningAll (ib : InsertBuilder) : InsertBuilder :=
  ib.returning1 .star

/-- Build the INSERT statement -/
def build (ib : InsertBuilder) : InsertStmt := ib.stmt

end InsertBuilder

/-- Create INSERT builder -/
def insertInto (table : String) : InsertBuilder := InsertBuilder.new table

end Chisel
