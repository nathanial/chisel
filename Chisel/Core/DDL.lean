/-
  Chisel.Core.DDL
  CREATE TABLE, ALTER, DROP, INDEX statements
-/
import Chisel.Core.Expr

namespace Chisel

/-- SQL column types -/
inductive ColumnType where
  | integer
  | real
  | text
  | blob
  | boolean
  | datetime
  | numeric (precision : Option Nat) (scale : Option Nat)
  | varchar (length : Option Nat)
  | custom (name : String)
  deriving Repr, BEq, Inhabited

namespace ColumnType

def render : ColumnType → String
  | .integer => "INTEGER"
  | .real => "REAL"
  | .text => "TEXT"
  | .blob => "BLOB"
  | .boolean => "BOOLEAN"
  | .datetime => "DATETIME"
  | .numeric p s =>
    match p, s with
    | some p, some s => s!"NUMERIC({p}, {s})"
    | some p, none => s!"NUMERIC({p})"
    | none, _ => "NUMERIC"
  | .varchar len =>
    match len with
    | some l => s!"VARCHAR({l})"
    | none => "VARCHAR"
  | .custom name => name

end ColumnType

/-- Foreign key action -/
inductive ForeignKeyAction where
  | noAction | restrict | cascade | setNull | setDefault
  deriving Repr, BEq, Inhabited

namespace ForeignKeyAction

def render : ForeignKeyAction → String
  | .noAction => "NO ACTION"
  | .restrict => "RESTRICT"
  | .cascade => "CASCADE"
  | .setNull => "SET NULL"
  | .setDefault => "SET DEFAULT"

end ForeignKeyAction

/-- Column constraint -/
inductive ColumnConstraint where
  | primaryKey (autoincrement : Bool := false)
  | notNull
  | unique
  | default (value : Expr)
  | check (expr : Expr)
  | references (table : String) (column : String)
      (onDelete : Option ForeignKeyAction := none)
      (onUpdate : Option ForeignKeyAction := none)
  | collate (name : String)
  deriving Inhabited

/-- Column definition -/
structure ColumnDef where
  name : String
  type : ColumnType
  constraints : List ColumnConstraint := []
  deriving Inhabited

/-- Table constraint -/
inductive TableConstraint where
  | primaryKey (columns : List String)
  | unique (columns : List String)
  | check (expr : Expr)
  | foreignKey (columns : List String) (refTable : String) (refColumns : List String)
      (onDelete : Option ForeignKeyAction := none)
      (onUpdate : Option ForeignKeyAction := none)
  deriving Inhabited

/-- CREATE TABLE statement -/
structure CreateTableStmt where
  name : String
  ifNotExists : Bool := false
  columns : List ColumnDef := []
  constraints : List TableConstraint := []
  temporary : Bool := false
  strict : Bool := false
  deriving Inhabited

/-- ALTER TABLE operation -/
inductive AlterOp where
  | addColumn (col : ColumnDef)
  | dropColumn (name : String)
  | renameColumn (oldName newName : String)
  | renameTable (newName : String)
  deriving Inhabited

/-- ALTER TABLE statement -/
structure AlterTableStmt where
  table : String
  operations : List AlterOp := []
  deriving Inhabited

/-- DROP TABLE statement -/
structure DropTableStmt where
  table : String
  ifExists : Bool := false
  deriving Inhabited

/-- CREATE INDEX statement -/
structure CreateIndexStmt where
  name : String
  table : String
  columns : List (String × Option SortDir)
  unique : Bool := false
  ifNotExists : Bool := false
  where_ : Option Expr := none
  deriving Inhabited

/-- DROP INDEX statement -/
structure DropIndexStmt where
  name : String
  ifExists : Bool := false
  deriving Inhabited

end Chisel
