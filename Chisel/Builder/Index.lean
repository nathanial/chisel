/-
  Chisel.Builder.Index
  Fluent CREATE INDEX builder
-/
import Chisel.Core.DDL

namespace Chisel

/-- CREATE INDEX builder with method chaining -/
structure IndexBuilder where
  stmt : CreateIndexStmt
  deriving Inhabited

namespace IndexBuilder

/-- Create new INDEX builder -/
def new (name : String) (table : String) : IndexBuilder :=
  { stmt := { name, table, columns := [] } }

/-- Add IF NOT EXISTS -/
def ifNotExists (ib : IndexBuilder) : IndexBuilder :=
  { ib with stmt := { ib.stmt with ifNotExists := true }}

/-- Make index UNIQUE -/
def unique (ib : IndexBuilder) : IndexBuilder :=
  { ib with stmt := { ib.stmt with unique := true }}

/-- Add column (with optional sort direction) -/
def column (ib : IndexBuilder) (name : String) (dir : Option SortDir := none) : IndexBuilder :=
  { ib with stmt := { ib.stmt with columns := ib.stmt.columns ++ [(name, dir)] }}

/-- Add column ascending -/
def columnAsc (ib : IndexBuilder) (name : String) : IndexBuilder :=
  ib.column name (some .asc)

/-- Add column descending -/
def columnDesc (ib : IndexBuilder) (name : String) : IndexBuilder :=
  ib.column name (some .desc)

/-- Add multiple columns -/
def columns (ib : IndexBuilder) (cols : List String) : IndexBuilder :=
  let newCols := cols.map (·, none)
  { ib with stmt := { ib.stmt with columns := ib.stmt.columns ++ newCols }}

/-- Add WHERE clause (partial index) -/
def where_ (ib : IndexBuilder) (cond : Expr) : IndexBuilder :=
  { ib with stmt := { ib.stmt with where_ := some cond }}

/-- Build the CREATE INDEX statement -/
def build (ib : IndexBuilder) : CreateIndexStmt := ib.stmt

end IndexBuilder

/-- Create INDEX builder -/
def createIndex (name : String) (table : String) : IndexBuilder :=
  IndexBuilder.new name table

/-- Create unique INDEX builder -/
def createUniqueIndex (name : String) (table : String) : IndexBuilder :=
  (IndexBuilder.new name table).unique

/-- Create DropIndexStmt -/
def dropIndex (name : String) (ifExists : Bool := false) : DropIndexStmt :=
  { name, ifExists }

end Chisel
