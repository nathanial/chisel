/-
  Chisel.Render.DDL
  Render CREATE TABLE, ALTER, DROP, INDEX to SQL strings
-/
import Chisel.Core.DDL
import Chisel.Render.Expr

namespace Chisel

/-- Render column constraint -/
def renderColumnConstraint (ctx : RenderContext) : ColumnConstraint → String
  | .primaryKey auto =>
    if auto then "PRIMARY KEY AUTOINCREMENT" else "PRIMARY KEY"
  | .notNull => "NOT NULL"
  | .unique => "UNIQUE"
  | .default value => s!"DEFAULT {renderExpr ctx value}"
  | .check expr => s!"CHECK ({renderExpr ctx expr})"
  | .references table column onDel onUpd =>
    let onDelStr := onDel.map (fun a => s!" ON DELETE {a.render}") |>.getD ""
    let onUpdStr := onUpd.map (fun a => s!" ON UPDATE {a.render}") |>.getD ""
    s!"REFERENCES {quoteIdent table}({quoteIdent column}){onDelStr}{onUpdStr}"
  | .collate name => s!"COLLATE {name}"

/-- Render column definition -/
def renderColumnDef (ctx : RenderContext) (col : ColumnDef) : String :=
  let constraintsStr := if col.constraints.isEmpty
    then ""
    else " " ++ (col.constraints.map (renderColumnConstraint ctx) |> String.intercalate " ")
  s!"{quoteIdent col.name} {col.type.render}{constraintsStr}"

/-- Render table constraint -/
def renderTableConstraint (ctx : RenderContext) : TableConstraint → String
  | .primaryKey cols =>
    s!"PRIMARY KEY ({cols.map quoteIdent |> String.intercalate ", "})"
  | .unique cols =>
    s!"UNIQUE ({cols.map quoteIdent |> String.intercalate ", "})"
  | .check expr => s!"CHECK ({renderExpr ctx expr})"
  | .foreignKey cols refTable refCols onDel onUpd =>
    let onDelStr := onDel.map (fun a => s!" ON DELETE {a.render}") |>.getD ""
    let onUpdStr := onUpd.map (fun a => s!" ON UPDATE {a.render}") |>.getD ""
    s!"FOREIGN KEY ({cols.map quoteIdent |> String.intercalate ", "}) REFERENCES {quoteIdent refTable}({refCols.map quoteIdent |> String.intercalate ", "}){onDelStr}{onUpdStr}"

/-- Render CREATE TABLE statement -/
def renderCreateTable (ctx : RenderContext) (stmt : CreateTableStmt) : String :=
  let tempStr := if stmt.temporary then "TEMPORARY " else ""
  let ifNotExistsStr := if stmt.ifNotExists then "IF NOT EXISTS " else ""
  let allDefs := stmt.columns.map (renderColumnDef ctx) ++
                 stmt.constraints.map (renderTableConstraint ctx)
  let defsStr := String.intercalate ", " allDefs
  let strictStr := if stmt.strict then " STRICT" else ""
  s!"CREATE {tempStr}TABLE {ifNotExistsStr}{quoteIdent stmt.name} ({defsStr}){strictStr}"

/-- Render single ALTER TABLE operation -/
def renderAlterOp (ctx : RenderContext) (table : String) : AlterOp → String
  | .addColumn col => s!"ALTER TABLE {quoteIdent table} ADD COLUMN {renderColumnDef ctx col}"
  | .dropColumn name => s!"ALTER TABLE {quoteIdent table} DROP COLUMN {quoteIdent name}"
  | .renameColumn old new => s!"ALTER TABLE {quoteIdent table} RENAME COLUMN {quoteIdent old} TO {quoteIdent new}"
  | .renameTable new => s!"ALTER TABLE {quoteIdent table} RENAME TO {quoteIdent new}"

/-- Render ALTER TABLE statement (may produce multiple statements) -/
def renderAlterTable (ctx : RenderContext) (stmt : AlterTableStmt) : String :=
  stmt.operations.map (renderAlterOp ctx stmt.table) |> String.intercalate ";\n"

/-- Render DROP TABLE statement -/
def renderDropTable (_ctx : RenderContext) (stmt : DropTableStmt) : String :=
  let ifExistsStr := if stmt.ifExists then "IF EXISTS " else ""
  s!"DROP TABLE {ifExistsStr}{quoteIdent stmt.table}"

/-- Render CREATE INDEX statement -/
def renderCreateIndex (ctx : RenderContext) (stmt : CreateIndexStmt) : String :=
  let uniqueStr := if stmt.unique then "UNIQUE " else ""
  let ifNotExistsStr := if stmt.ifNotExists then "IF NOT EXISTS " else ""
  let columnsStr := stmt.columns.map (fun (name, dir) =>
    match dir with
    | some .asc => s!"{quoteIdent name} ASC"
    | some .desc => s!"{quoteIdent name} DESC"
    | none => quoteIdent name) |> String.intercalate ", "
  let whereStr := stmt.where_.map (fun e => s!" WHERE {renderExpr ctx e}") |>.getD ""
  s!"CREATE {uniqueStr}INDEX {ifNotExistsStr}{quoteIdent stmt.name} ON {quoteIdent stmt.table} ({columnsStr}){whereStr}"

/-- Render DROP INDEX statement -/
def renderDropIndex (_ctx : RenderContext) (stmt : DropIndexStmt) : String :=
  let ifExistsStr := if stmt.ifExists then "IF EXISTS " else ""
  s!"DROP INDEX {ifExistsStr}{quoteIdent stmt.name}"

end Chisel
