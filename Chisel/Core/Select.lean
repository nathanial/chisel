/-
  Chisel.Core.Select
  SELECT statement types (re-exports from Expr for convenience)
-/
import Chisel.Core.Expr

namespace Chisel

/-- SELECT statement (alias for SelectCore) -/
abbrev SelectStmt := SelectCore

/-- Create an empty SELECT statement -/
def SelectStmt.empty : SelectStmt := SelectCore.empty

end Chisel
