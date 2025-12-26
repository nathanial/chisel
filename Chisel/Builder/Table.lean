/-
  Chisel.Builder.Table
  Fluent CREATE TABLE builder
-/
import Chisel.Core.DDL

namespace Chisel

/-- CREATE TABLE builder with method chaining -/
structure TableBuilder where
  stmt : CreateTableStmt
  deriving Inhabited

namespace TableBuilder

/-- Create new TABLE builder -/
def new (name : String) : TableBuilder :=
  { stmt := { name } }

/-- Add IF NOT EXISTS -/
def ifNotExists (tb : TableBuilder) : TableBuilder :=
  { tb with stmt := { tb.stmt with ifNotExists := true }}

/-- Make table TEMPORARY -/
def temporary (tb : TableBuilder) : TableBuilder :=
  { tb with stmt := { tb.stmt with temporary := true }}

/-- Enable STRICT mode (SQLite) -/
def strict (tb : TableBuilder) : TableBuilder :=
  { tb with stmt := { tb.stmt with strict := true }}

/-- Add column -/
def column (tb : TableBuilder) (name : String) (type : ColumnType)
    (constraints : List ColumnConstraint := []) : TableBuilder :=
  { tb with stmt := { tb.stmt with
    columns := tb.stmt.columns ++ [{ name, type, constraints }] }}

/-- Add INTEGER column -/
def intColumn (tb : TableBuilder) (name : String)
    (constraints : List ColumnConstraint := []) : TableBuilder :=
  tb.column name .integer constraints

/-- Add TEXT column -/
def textColumn (tb : TableBuilder) (name : String)
    (constraints : List ColumnConstraint := []) : TableBuilder :=
  tb.column name .text constraints

/-- Add REAL column -/
def realColumn (tb : TableBuilder) (name : String)
    (constraints : List ColumnConstraint := []) : TableBuilder :=
  tb.column name .real constraints

/-- Add BLOB column -/
def blobColumn (tb : TableBuilder) (name : String)
    (constraints : List ColumnConstraint := []) : TableBuilder :=
  tb.column name .blob constraints

/-- Add BOOLEAN column -/
def boolColumn (tb : TableBuilder) (name : String)
    (constraints : List ColumnConstraint := []) : TableBuilder :=
  tb.column name .boolean constraints

/-- Add DATETIME column -/
def datetimeColumn (tb : TableBuilder) (name : String)
    (constraints : List ColumnConstraint := []) : TableBuilder :=
  tb.column name .datetime constraints

/-- Add INTEGER PRIMARY KEY AUTOINCREMENT column -/
def idColumn (tb : TableBuilder) (name : String := "id") : TableBuilder :=
  tb.column name .integer [.primaryKey true]

/-- Add table-level PRIMARY KEY constraint -/
def primaryKey (tb : TableBuilder) (columns : List String) : TableBuilder :=
  { tb with stmt := { tb.stmt with
    constraints := tb.stmt.constraints ++ [.primaryKey columns] }}

/-- Add table-level UNIQUE constraint -/
def unique (tb : TableBuilder) (columns : List String) : TableBuilder :=
  { tb with stmt := { tb.stmt with
    constraints := tb.stmt.constraints ++ [.unique columns] }}

/-- Add table-level CHECK constraint -/
def check (tb : TableBuilder) (expr : Expr) : TableBuilder :=
  { tb with stmt := { tb.stmt with
    constraints := tb.stmt.constraints ++ [.check expr] }}

/-- Add table-level FOREIGN KEY constraint -/
def foreignKey (tb : TableBuilder) (columns : List String)
    (refTable : String) (refColumns : List String)
    (onDelete : Option ForeignKeyAction := none)
    (onUpdate : Option ForeignKeyAction := none) : TableBuilder :=
  { tb with stmt := { tb.stmt with
    constraints := tb.stmt.constraints ++
      [.foreignKey columns refTable refColumns onDelete onUpdate] }}

/-- Build the CREATE TABLE statement -/
def build (tb : TableBuilder) : CreateTableStmt := tb.stmt

end TableBuilder

/-- Create TABLE builder -/
def createTable (name : String) : TableBuilder := TableBuilder.new name

/-- Create DropTableStmt -/
def dropTable (name : String) (ifExists : Bool := false) : DropTableStmt :=
  { table := name, ifExists }

/-- Create AlterTableStmt builder -/
structure AlterTableBuilder where
  stmt : AlterTableStmt
  deriving Inhabited

namespace AlterTableBuilder

/-- Create new ALTER TABLE builder -/
def new (table : String) : AlterTableBuilder :=
  { stmt := { table } }

/-- Add column -/
def addColumn (ab : AlterTableBuilder) (col : ColumnDef) : AlterTableBuilder :=
  { ab with stmt := { ab.stmt with operations := ab.stmt.operations ++ [.addColumn col] }}

/-- Add column with inline definition -/
def addColumn' (ab : AlterTableBuilder) (name : String) (type : ColumnType)
    (constraints : List ColumnConstraint := []) : AlterTableBuilder :=
  ab.addColumn { name, type, constraints }

/-- Drop column -/
def dropColumn (ab : AlterTableBuilder) (name : String) : AlterTableBuilder :=
  { ab with stmt := { ab.stmt with operations := ab.stmt.operations ++ [.dropColumn name] }}

/-- Rename column -/
def renameColumn (ab : AlterTableBuilder) (oldName newName : String) : AlterTableBuilder :=
  { ab with stmt := { ab.stmt with operations := ab.stmt.operations ++ [.renameColumn oldName newName] }}

/-- Rename table -/
def renameTable (ab : AlterTableBuilder) (newName : String) : AlterTableBuilder :=
  { ab with stmt := { ab.stmt with operations := ab.stmt.operations ++ [.renameTable newName] }}

/-- Build the ALTER TABLE statement -/
def build (ab : AlterTableBuilder) : AlterTableStmt := ab.stmt

end AlterTableBuilder

/-- Create ALTER TABLE builder -/
def alterTable (table : String) : AlterTableBuilder := AlterTableBuilder.new table

end Chisel
