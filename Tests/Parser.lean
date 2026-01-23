/-
  Tests for Chisel SQL Parser
-/
import Crucible
import Chisel

namespace Tests.Parser

open Crucible
open Chisel
open Chisel.Parser

-- Helper to check parse results
def checkExpr (sql : String) (pred : Expr → Bool) (desc : String) : IO Unit := do
  match Expr.parse sql with
  | .ok e => shouldSatisfy (pred e) desc
  | .error e => throw <| IO.userError s!"parse error: {e}"

def checkSelect (sql : String) (pred : SelectCore → Bool) (desc : String) : IO Unit := do
  match SelectCore.parse sql with
  | .ok s => shouldSatisfy (pred s) desc
  | .error e => throw <| IO.userError s!"parse error: {e}"

testSuite "Chisel Parser - Expressions"

test "parse integer literal" := do
  checkExpr "42" (fun e => match e with | .lit (.int 42) => true | _ => false) "integer 42"

test "parse negative integer" := do
  checkExpr "-123" (fun e => match e with | .unary .neg (.lit (.int 123)) => true | _ => false) "negative integer"

test "parse float literal" := do
  match Expr.parse "3.14" with
  | .ok (.lit (.float f)) => shouldSatisfy (f > 3.13 && f < 3.15) "float ~3.14"
  | .ok _ => throw <| IO.userError "expected float literal"
  | .error e => throw <| IO.userError s!"parse error: {e}"

test "parse string literal" := do
  checkExpr "'hello'" (fun e => match e with | .lit (.string "hello") => true | _ => false) "string hello"

test "parse string with escaped quote" := do
  checkExpr "'it''s'" (fun e => match e with | .lit (.string "it's") => true | _ => false) "escaped quote"

test "parse NULL" := do
  checkExpr "NULL" (fun e => match e with | .lit .null => true | _ => false) "NULL"

test "parse TRUE" := do
  checkExpr "TRUE" (fun e => match e with | .lit (.bool true) => true | _ => false) "TRUE"

test "parse FALSE" := do
  checkExpr "FALSE" (fun e => match e with | .lit (.bool false) => true | _ => false) "FALSE"

test "parse column reference" := do
  checkExpr "name" (fun e => match e with | .col "name" => true | _ => false) "column name"

test "parse qualified column" := do
  checkExpr "users.name" (fun e => match e with | .qualified "users" "name" => true | _ => false) "qualified column"

test "parse star" := do
  checkExpr "*" (fun e => match e with | .star => true | _ => false) "star"

test "parse table star" := do
  checkExpr "users.*" (fun e => match e with | .tableStar "users" => true | _ => false) "table star"

test "parse equality" := do
  checkExpr "a = 1" (fun e => match e with | .binary .eq _ _ => true | _ => false) "equality"

test "parse comparison operators" := do
  checkExpr "a < b" (fun e => match e with | .binary .lt _ _ => true | _ => false) "less than"

test "parse AND expression" := do
  checkExpr "a = 1 AND b = 2" (fun e => match e with | .binary .and _ _ => true | _ => false) "AND"

test "parse OR expression" := do
  checkExpr "a = 1 OR b = 2" (fun e => match e with | .binary .or _ _ => true | _ => false) "OR"

test "parse NOT expression" := do
  checkExpr "NOT active" (fun e => match e with | .unary .not _ => true | _ => false) "NOT"

test "parse arithmetic" := do
  checkExpr "a + b * c" (fun e => match e with | .binary .add _ (.binary .mul _ _) => true | _ => false) "precedence"

test "parse BETWEEN" := do
  checkExpr "x BETWEEN 1 AND 10" (fun e => match e with | .between _ _ _ => true | _ => false) "BETWEEN"

test "parse IN values" := do
  checkExpr "x IN (1, 2, 3)" (fun e => match e with | .inValues _ _ => true | _ => false) "IN values"

test "parse LIKE" := do
  checkExpr "name LIKE '%test%'" (fun e => match e with | .binary .like _ _ => true | _ => false) "LIKE"

test "parse IS NULL" := do
  checkExpr "x IS NULL" (fun e => match e with | .unary .isNull _ => true | _ => false) "IS NULL"

test "parse IS NOT NULL" := do
  checkExpr "x IS NOT NULL" (fun e => match e with | .unary .isNotNull _ => true | _ => false) "IS NOT NULL"

test "parse function call" := do
  checkExpr "UPPER(name)" (fun e => match e with | .func "UPPER" _ => true | _ => false) "function"

test "parse COUNT(*)" := do
  checkExpr "COUNT(*)" (fun e => match e with | .agg .countAll none _ => true | _ => false) "COUNT(*)"

test "parse SUM" := do
  checkExpr "SUM(amount)" (fun e => match e with | .agg .sum (some _) _ => true | _ => false) "SUM"

test "parse COUNT DISTINCT" := do
  checkExpr "COUNT(DISTINCT user_id)" (fun e => match e with | .agg .count (some _) true => true | _ => false) "COUNT DISTINCT"

test "parse CASE expression" := do
  checkExpr "CASE WHEN x = 1 THEN 'one' ELSE 'other' END" (fun e => match e with | .case_ _ (some _) => true | _ => false) "CASE"

test "parse CAST" := do
  checkExpr "CAST(x AS INTEGER)" (fun e => match e with | .cast _ "INTEGER" => true | _ => false) "CAST"

test "parse positional parameter" := do
  checkExpr "?" (fun e => match e with | .param none none => true | _ => false) "positional param"

test "parse indexed parameter" := do
  checkExpr "$1" (fun e => match e with | .param none (some 1) => true | _ => false) "indexed param"

test "parse named parameter (colon)" := do
  checkExpr ":name" (fun e => match e with | .param (some "name") none => true | _ => false) "named param"

test "parse named parameter (at)" := do
  checkExpr "@value" (fun e => match e with | .param (some "value") none => true | _ => false) "@ param"

test "parse parenthesized expression" := do
  checkExpr "(a + b) * c" (fun e => match e with | .binary .mul (.binary .add _ _) _ => true | _ => false) "parenthesized"

testSuite "Chisel Parser - SELECT"

test "parse simple SELECT" := do
  checkSelect "SELECT name FROM users" (fun s => s.columns.length == 1 && s.from_.isSome) "simple select"

test "parse SELECT *" := do
  match SelectCore.parse "SELECT * FROM users" with
  | .ok s =>
    match s.columns.head?.map SelectItem.expr with
    | some .star => pure ()
    | _ => throw <| IO.userError "expected star"
  | .error e => throw <| IO.userError s!"parse error: {e}"

test "parse SELECT with WHERE" := do
  checkSelect "SELECT * FROM users WHERE active = TRUE" (fun s => s.where_.isSome) "with WHERE"

test "parse SELECT with multiple columns" := do
  checkSelect "SELECT id, name, email FROM users" (fun s => s.columns.length == 3) "3 columns"

test "parse SELECT with alias" := do
  match SelectCore.parse "SELECT name AS user_name FROM users" with
  | .ok s =>
    match s.columns.head?.map SelectItem.alias_ with
    | some (some "user_name") => pure ()
    | _ => throw <| IO.userError "expected alias"
  | .error e => throw <| IO.userError s!"parse error: {e}"

test "parse SELECT DISTINCT" := do
  checkSelect "SELECT DISTINCT category FROM products" (fun s => s.distinct) "DISTINCT"

test "parse SELECT with JOIN" := do
  match SelectCore.parse "SELECT * FROM users JOIN orders ON users.id = orders.user_id" with
  | .ok s =>
    match s.from_ with
    | some (.join .inner _ _ _) => pure ()
    | _ => throw <| IO.userError "expected JOIN"
  | .error e => throw <| IO.userError s!"parse error: {e}"

test "parse SELECT with LEFT JOIN" := do
  match SelectCore.parse "SELECT * FROM users LEFT JOIN orders ON users.id = orders.user_id" with
  | .ok s =>
    match s.from_ with
    | some (.join .left _ _ _) => pure ()
    | _ => throw <| IO.userError "expected LEFT JOIN"
  | .error e => throw <| IO.userError s!"parse error: {e}"

test "parse SELECT with GROUP BY" := do
  checkSelect "SELECT category, COUNT(*) FROM products GROUP BY category" (fun s => s.groupBy.length > 0) "GROUP BY"

test "parse SELECT with HAVING" := do
  checkSelect "SELECT category, COUNT(*) FROM products GROUP BY category HAVING COUNT(*) > 5" (fun s => s.having.isSome) "HAVING"

test "parse SELECT with ORDER BY" := do
  checkSelect "SELECT * FROM users ORDER BY created_at DESC" (fun s => s.orderBy.length > 0) "ORDER BY"

test "parse SELECT with LIMIT" := do
  checkSelect "SELECT * FROM users LIMIT 10" (fun s => s.limit == some 10) "LIMIT"

test "parse SELECT with OFFSET" := do
  checkSelect "SELECT * FROM users LIMIT 10 OFFSET 20" (fun s => s.limit == some 10 && s.offset == some 20) "OFFSET"

test "parse SELECT with subquery in FROM" := do
  match SelectCore.parse "SELECT * FROM (SELECT id FROM users) AS sub" with
  | .ok s =>
    match s.from_ with
    | some (.subquery _ _) => pure ()
    | _ => throw <| IO.userError "expected subquery in FROM"
  | .error e => throw <| IO.userError s!"parse error: {e}"

test "parse SELECT with IN subquery" := do
  match SelectCore.parse "SELECT * FROM orders WHERE user_id IN (SELECT id FROM users)" with
  | .ok s =>
    match s.where_ with
    | some (.inSubquery _ _) => pure ()
    | _ => throw <| IO.userError "expected IN subquery"
  | .error e => throw <| IO.userError s!"parse error: {e}"

test "parse SELECT with EXISTS" := do
  match SelectCore.parse "SELECT * FROM users WHERE EXISTS (SELECT 1 FROM orders WHERE orders.user_id = users.id)" with
  | .ok s =>
    match s.where_ with
    | some (.exists_ _) => pure ()
    | _ => throw <| IO.userError "expected EXISTS"
  | .error e => throw <| IO.userError s!"parse error: {e}"

testSuite "Chisel Parser - DML"

test "parse simple INSERT" := do
  match InsertStmt.parse "INSERT INTO users (name, email) VALUES ('John', 'john@example.com')" with
  | .ok s => shouldSatisfy (s.table == "users" && s.columns.length == 2) "INSERT structure"
  | .error e => throw <| IO.userError s!"parse error: {e}"

test "parse INSERT OR IGNORE" := do
  match InsertStmt.parse "INSERT OR IGNORE INTO users (name) VALUES ('John')" with
  | .ok s =>
    match s.onConflict with
    | some .ignore => pure ()
    | _ => throw <| IO.userError "expected OR IGNORE"
  | .error e => throw <| IO.userError s!"parse error: {e}"

test "parse INSERT with RETURNING" := do
  match InsertStmt.parse "INSERT INTO users (name) VALUES ('John') RETURNING id" with
  | .ok s => shouldSatisfy (s.returning.length > 0) "RETURNING"
  | .error e => throw <| IO.userError s!"parse error: {e}"

test "parse UPDATE" := do
  match UpdateStmt.parse "UPDATE users SET name = 'Jane' WHERE id = 1" with
  | .ok s => shouldSatisfy (s.table == "users" && s.set.length == 1 && s.where_.isSome) "UPDATE"
  | .error e => throw <| IO.userError s!"parse error: {e}"

test "parse DELETE" := do
  match DeleteStmt.parse "DELETE FROM users WHERE id = 1" with
  | .ok s => shouldSatisfy (s.table == "users" && s.where_.isSome) "DELETE"
  | .error e => throw <| IO.userError s!"parse error: {e}"

testSuite "Chisel Parser - DDL"

test "parse CREATE TABLE" := do
  match CreateTableStmt.parse "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT NOT NULL)" with
  | .ok s => shouldSatisfy (s.name == "users" && s.columns.length == 2) "CREATE TABLE"
  | .error e => throw <| IO.userError s!"parse error: {e}"

test "parse CREATE TABLE IF NOT EXISTS" := do
  match CreateTableStmt.parse "CREATE TABLE IF NOT EXISTS users (id INTEGER)" with
  | .ok s => shouldSatisfy s.ifNotExists "IF NOT EXISTS"
  | .error e => throw <| IO.userError s!"parse error: {e}"

test "parse CREATE TEMPORARY TABLE" := do
  match CreateTableStmt.parse "CREATE TEMPORARY TABLE temp_users (id INTEGER)" with
  | .ok s => shouldSatisfy s.temporary "TEMPORARY"
  | .error e => throw <| IO.userError s!"parse error: {e}"

test "parse DROP TABLE" := do
  match DropTableStmt.parse "DROP TABLE users" with
  | .ok s => shouldSatisfy (s.table == "users") "DROP TABLE"
  | .error e => throw <| IO.userError s!"parse error: {e}"

test "parse DROP TABLE IF EXISTS" := do
  match DropTableStmt.parse "DROP TABLE IF EXISTS users" with
  | .ok s => shouldSatisfy s.ifExists "IF EXISTS"
  | .error e => throw <| IO.userError s!"parse error: {e}"

test "parse CREATE INDEX" := do
  match CreateIndexStmt.parse "CREATE INDEX idx_users_email ON users (email)" with
  | .ok s => shouldSatisfy (s.name == "idx_users_email" && s.table == "users") "CREATE INDEX"
  | .error e => throw <| IO.userError s!"parse error: {e}"

test "parse CREATE UNIQUE INDEX" := do
  match CreateIndexStmt.parse "CREATE UNIQUE INDEX idx_users_email ON users (email)" with
  | .ok s => shouldSatisfy s.unique "UNIQUE"
  | .error e => throw <| IO.userError s!"parse error: {e}"

test "parse DROP INDEX" := do
  match DropIndexStmt.parse "DROP INDEX idx_users_email" with
  | .ok s => shouldSatisfy (s.name == "idx_users_email") "DROP INDEX"
  | .error e => throw <| IO.userError s!"parse error: {e}"

test "parse ALTER TABLE ADD COLUMN" := do
  match AlterTableStmt.parse "ALTER TABLE users ADD COLUMN age INTEGER" with
  | .ok s =>
    if s.table == "users" then
      match s.operations.head? with
      | some (.addColumn _) => pure ()
      | _ => throw <| IO.userError "expected ADD COLUMN"
    else throw <| IO.userError "wrong table"
  | .error e => throw <| IO.userError s!"parse error: {e}"

testSuite "Chisel Parser - Parameter Binding"

test "bind positional parameters" := do
  match Expr.parse "x = ? AND y = ?" with
  | .ok expr =>
    match bindPositional expr [.int 1, .int 2] with
    | .ok _ => pure ()
    | .error e => throw <| IO.userError s!"bind error: {e}"
  | .error e => throw <| IO.userError s!"parse error: {e}"

test "bind named parameters" := do
  match Expr.parse "x = :val" with
  | .ok expr =>
    match bindNamed expr [("val", .int 42)] with
    | .ok _ => pure ()
    | .error e => throw <| IO.userError s!"bind error: {e}"
  | .error e => throw <| IO.userError s!"parse error: {e}"

test "bind indexed parameters" := do
  match Expr.parse "x = $1 AND y = $2" with
  | .ok expr =>
    match bindIndexed expr #[.int 10, .int 20] with
    | .ok _ => pure ()
    | .error e => throw <| IO.userError s!"bind error: {e}"
  | .error e => throw <| IO.userError s!"parse error: {e}"

test "missing positional parameter fails" := do
  match Expr.parse "x = ? AND y = ?" with
  | .ok expr =>
    match bindPositional expr [.int 1] with
    | .ok _ => throw <| IO.userError "should have failed"
    | .error _ => pure ()
  | .error e => throw <| IO.userError s!"parse error: {e}"

test "missing named parameter fails" := do
  match Expr.parse "x = :missing" with
  | .ok expr =>
    match bindNamed expr [] with
    | .ok _ => throw <| IO.userError "should have failed"
    | .error _ => pure ()
  | .error e => throw <| IO.userError s!"parse error: {e}"
