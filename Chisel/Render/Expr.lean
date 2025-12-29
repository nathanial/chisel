/-
  Chisel.Render.Expr
  Render expressions and SELECT statements to SQL strings
-/
import Chisel.Core.Select

namespace Chisel

/-- SQL dialect -/
inductive Dialect where
  | sqlite | postgres | mysql
  deriving Repr, BEq, Inhabited

/-- Parameter placeholder style -/
inductive ParamStyle where
  | question   -- ?
  | dollar     -- $1, $2
  | colon      -- :name
  | at_        -- @name
  deriving Repr, BEq, Inhabited

/-- Render context for SQL dialect differences -/
structure RenderContext where
  dialect : Dialect := .sqlite
  paramStyle : ParamStyle := .question
  paramIndex : Nat := 1
  deriving Inhabited

/-- Check if identifier needs quoting -/
private def needsQuoting (s : String) : Bool :=
  s.any (fun c => !c.isAlphanum && c != '_') || isReserved s
where
  isReserved (s : String) : Bool :=
    let upper := s.toUpper
    ["SELECT", "FROM", "WHERE", "ORDER", "GROUP", "BY", "AS", "JOIN",
     "ON", "AND", "OR", "NOT", "NULL", "TRUE", "FALSE", "IN", "LIKE",
     "BETWEEN", "CASE", "WHEN", "THEN", "ELSE", "END", "CAST", "AS",
     "DISTINCT", "ALL", "UNION", "INTERSECT", "EXCEPT", "INSERT", "INTO",
     "VALUES", "UPDATE", "SET", "DELETE", "CREATE", "TABLE", "INDEX",
     "DROP", "ALTER", "PRIMARY", "KEY", "FOREIGN", "REFERENCES", "UNIQUE",
     "CHECK", "DEFAULT", "HAVING", "LIMIT", "OFFSET", "LEFT", "RIGHT",
     "INNER", "OUTER", "CROSS", "FULL", "NATURAL", "USING", "EXISTS",
     "IS", "ASC", "DESC", "NULLS", "FIRST", "LAST", "CONSTRAINT",
     "AUTOINCREMENT", "STRICT", "TEMPORARY", "TEMP", "IF"].contains upper

/-- Quote identifier if needed -/
def quoteIdent (name : String) : String :=
  if needsQuoting name then s!"\"{name}\"" else name

/-- Render aggregate function name -/
private def renderAggFunc : AggFunc → String
  | .count => "COUNT"
  | .countAll => "COUNT"
  | .sum => "SUM"
  | .avg => "AVG"
  | .min => "MIN"
  | .max => "MAX"
  | .total => "TOTAL"
  | .groupConcat _ => "GROUP_CONCAT"

mutual

/-- Render expression to SQL string -/
partial def renderExpr (ctx : RenderContext) : Expr → String
  | .lit v => v.render
  | .col name => quoteIdent name
  | .qualified table column => s!"{quoteIdent table}.{quoteIdent column}"
  | .star => "*"
  | .tableStar table => s!"{quoteIdent table}.*"
  | .binary op left right =>
    let l := renderExpr ctx left
    let r := renderExpr ctx right
    s!"({l} {op.render} {r})"
  | .unary op operand =>
    let e := renderExpr ctx operand
    match op with
    | .not => s!"(NOT {e})"
    | .neg => s!"(-{e})"
    | .isNull => s!"({e} IS NULL)"
    | .isNotNull => s!"({e} IS NOT NULL)"
  | .between expr lower upper =>
    let e := renderExpr ctx expr
    let l := renderExpr ctx lower
    let u := renderExpr ctx upper
    s!"({e} BETWEEN {l} AND {u})"
  | .inValues expr values =>
    let e := renderExpr ctx expr
    let vs := values.map (renderExpr ctx) |> String.intercalate ", "
    s!"({e} IN ({vs}))"
  | .notInValues expr values =>
    let e := renderExpr ctx expr
    let vs := values.map (renderExpr ctx) |> String.intercalate ", "
    s!"({e} NOT IN ({vs}))"
  | .inSubquery expr subq =>
    let e := renderExpr ctx expr
    let sq := renderSelect ctx subq
    s!"({e} IN ({sq}))"
  | .notInSubquery expr subq =>
    let e := renderExpr ctx expr
    let sq := renderSelect ctx subq
    s!"({e} NOT IN ({sq}))"
  | .exists_ subq =>
    let sq := renderSelect ctx subq
    s!"EXISTS ({sq})"
  | .notExists subq =>
    let sq := renderSelect ctx subq
    s!"NOT EXISTS ({sq})"
  | .case_ cases else_ =>
    let whenClauses := cases.map fun (cond, result) =>
      s!"WHEN {renderExpr ctx cond} THEN {renderExpr ctx result}"
    let elseClause := else_.map (fun e => s!" ELSE {renderExpr ctx e}") |>.getD ""
    s!"(CASE {String.intercalate " " whenClauses}{elseClause} END)"
  | .cast expr typeName =>
    s!"CAST({renderExpr ctx expr} AS {typeName})"
  | .func name args =>
    let argList := args.map (renderExpr ctx) |> String.intercalate ", "
    s!"{name}({argList})"
  | .agg func expr distinct =>
    let distinctStr := if distinct then "DISTINCT " else ""
    match func with
    | .countAll => "COUNT(*)"
    | .groupConcat sep =>
      let sepStr := sep.map (fun s => s!", '{s}'") |>.getD ""
      s!"GROUP_CONCAT({distinctStr}{expr.map (renderExpr ctx) |>.getD ""}{sepStr})"
    | _ =>
      let funcName := renderAggFunc func
      s!"{funcName}({distinctStr}{expr.map (renderExpr ctx) |>.getD ""})"
  | .param name idx =>
    match ctx.paramStyle with
    | .question => "?"
    | .dollar => "$" ++ toString (idx.getD ctx.paramIndex)
    | .colon => s!":{name.getD "param"}"
    | .at_ => s!"@{name.getD "param"}"
  | .raw sql => sql
  | .subquery subq =>
    let sq := renderSelect ctx subq
    s!"({sq})"

/-- Render SELECT statement to SQL string -/
partial def renderSelect (ctx : RenderContext) (stmt : SelectStmt) : String :=
  let distinctStr := if stmt.distinct then "DISTINCT " else ""
  let columnsStr := if stmt.columns.isEmpty
    then "*"
    else stmt.columns.map (renderSelectItem ctx) |> String.intercalate ", "
  let fromStr := stmt.from_.map (fun t => s!" FROM {renderTableRef ctx t}") |>.getD ""
  let whereStr := stmt.where_.map (fun e => s!" WHERE {renderExpr ctx e}") |>.getD ""
  let groupStr := if stmt.groupBy.isEmpty
    then ""
    else s!" GROUP BY {stmt.groupBy.map (renderExpr ctx) |> String.intercalate ", "}"
  let havingStr := stmt.having.map (fun e => s!" HAVING {renderExpr ctx e}") |>.getD ""
  let orderStr := if stmt.orderBy.isEmpty
    then ""
    else s!" ORDER BY {stmt.orderBy.map (renderOrderItem ctx) |> String.intercalate ", "}"
  let limitStr := stmt.limit.map (fun n => s!" LIMIT {n}") |>.getD ""
  let offsetStr := stmt.offset.map (fun n => s!" OFFSET {n}") |>.getD ""
  s!"SELECT {distinctStr}{columnsStr}{fromStr}{whereStr}{groupStr}{havingStr}{orderStr}{limitStr}{offsetStr}"

/-- Render table reference -/
partial def renderTableRef (ctx : RenderContext) : TableRef → String
  | .table name alias_ =>
    match alias_ with
    | some a => s!"{quoteIdent name} AS {quoteIdent a}"
    | none => quoteIdent name
  | .join type left right on =>
    let typeStr := match type with
      | .inner => "INNER JOIN"
      | .left => "LEFT JOIN"
      | .right => "RIGHT JOIN"
      | .full => "FULL JOIN"
      | .cross => "CROSS JOIN"
    let onStr := on.map (fun e => s!" ON {renderExpr ctx e}") |>.getD ""
    s!"{renderTableRef ctx left} {typeStr} {renderTableRef ctx right}{onStr}"
  | .subquery select alias_ =>
    let sq := renderSelect ctx select
    s!"({sq}) AS {quoteIdent alias_}"

/-- Render SELECT item (column with optional alias) -/
partial def renderSelectItem (ctx : RenderContext) (item : SelectItem) : String :=
  let exprStr := renderExpr ctx item.expr
  match item.alias_ with
  | some a => s!"{exprStr} AS {quoteIdent a}"
  | none => exprStr

/-- Render ORDER BY item -/
partial def renderOrderItem (ctx : RenderContext) (item : OrderItem) : String :=
  let exprStr := renderExpr ctx item.expr
  let dirStr := match item.dir with | .asc => "" | .desc => " DESC"
  let nullsStr := item.nulls.map (fun n =>
    match n with | .first => " NULLS FIRST" | .last => " NULLS LAST") |>.getD ""
  s!"{exprStr}{dirStr}{nullsStr}"

end

end Chisel
