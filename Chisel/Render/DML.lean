/-
  Chisel.Render.DML
  Render INSERT, UPDATE, DELETE to SQL strings
-/
import Chisel.Core.DML
import Chisel.Render.Expr

namespace Chisel

/-- Render INSERT statement -/
def renderInsert (ctx : RenderContext) (stmt : InsertStmt) : String :=
  let conflictStr := stmt.onConflict.map (fun c => s!"{c.render} ") |>.getD ""
  let columnsStr := if stmt.columns.isEmpty
    then ""
    else s!" ({stmt.columns.map quoteIdent |> String.intercalate ", "})"
  let valuesStr := match stmt.fromSelect with
    | some sel => s!" {renderSelect ctx sel}"
    | none =>
      if stmt.values.isEmpty then
        " DEFAULT VALUES"
      else
        let rows := stmt.values.map fun (row : List Expr) =>
          s!"({row.map (renderExpr ctx) |> String.intercalate ", "})"
        s!" VALUES {String.intercalate ", " rows}"
  let returningStr := if stmt.returning.isEmpty
    then ""
    else s!" RETURNING {stmt.returning.map (renderSelectItem ctx) |> String.intercalate ", "}"
  s!"INSERT {conflictStr}INTO {quoteIdent stmt.table}{columnsStr}{valuesStr}{returningStr}"

/-- Render UPDATE statement -/
def renderUpdate (ctx : RenderContext) (stmt : UpdateStmt) : String :=
  let tableStr := match stmt.alias_ with
    | some a => s!"{quoteIdent stmt.table} AS {quoteIdent a}"
    | none => quoteIdent stmt.table
  let setStr := stmt.set.map (fun a =>
    s!"{quoteIdent a.column} = {renderExpr ctx a.value}") |> String.intercalate ", "
  let fromStr := stmt.from_.map (fun t => s!" FROM {renderTableRef ctx t}") |>.getD ""
  let whereStr := stmt.where_.map (fun e => s!" WHERE {renderExpr ctx e}") |>.getD ""
  let returningStr := if stmt.returning.isEmpty
    then ""
    else s!" RETURNING {stmt.returning.map (renderSelectItem ctx) |> String.intercalate ", "}"
  s!"UPDATE {tableStr} SET {setStr}{fromStr}{whereStr}{returningStr}"

/-- Render DELETE statement -/
def renderDelete (ctx : RenderContext) (stmt : DeleteStmt) : String :=
  let tableStr := match stmt.alias_ with
    | some a => s!"{quoteIdent stmt.table} AS {quoteIdent a}"
    | none => quoteIdent stmt.table
  let whereStr := stmt.where_.map (fun e => s!" WHERE {renderExpr ctx e}") |>.getD ""
  let returningStr := if stmt.returning.isEmpty
    then ""
    else s!" RETURNING {stmt.returning.map (renderSelectItem ctx) |> String.intercalate ", "}"
  s!"DELETE FROM {tableStr}{whereStr}{returningStr}"

end Chisel
