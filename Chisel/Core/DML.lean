/-
  Chisel.Core.DML
  INSERT, UPDATE, DELETE statements
-/
import Chisel.Core.Select

namespace Chisel

/-- Conflict resolution for INSERT -/
inductive ConflictAction where
  | abort | rollback | fail | ignore | replace
  deriving Repr, BEq, Inhabited

namespace ConflictAction

def render : ConflictAction → String
  | .abort => "OR ABORT"
  | .rollback => "OR ROLLBACK"
  | .fail => "OR FAIL"
  | .ignore => "OR IGNORE"
  | .replace => "OR REPLACE"

end ConflictAction

/-- INSERT statement -/
structure InsertStmt where
  table : String
  columns : List String := []
  values : List (List Expr) := []
  fromSelect : Option SelectStmt := none
  onConflict : Option ConflictAction := none
  returning : List SelectItem := []
  deriving Inhabited

/-- UPDATE assignment -/
structure Assignment where
  column : String
  value : Expr
  deriving Inhabited

/-- UPDATE statement -/
structure UpdateStmt where
  table : String
  alias_ : Option String := none
  set : List Assignment := []
  from_ : Option TableRef := none
  where_ : Option Expr := none
  returning : List SelectItem := []
  deriving Inhabited

/-- DELETE statement -/
structure DeleteStmt where
  table : String
  alias_ : Option String := none
  where_ : Option Expr := none
  returning : List SelectItem := []
  deriving Inhabited

end Chisel
