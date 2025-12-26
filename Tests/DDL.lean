/-
  DDL (CREATE TABLE, ALTER, DROP, INDEX) tests
-/
import Chisel
import Crucible

namespace Tests.DDL

open Crucible
open Chisel

/-- Check if a string contains a substring -/
def containsSubstr (haystack needle : String) : Bool :=
  if needle.isEmpty then true
  else
    let haystackLen := haystack.length
    let needleLen := needle.length
    if needleLen > haystackLen then false
    else
      let rec loop (i : Nat) (fuel : Nat) : Bool :=
        match fuel with
        | 0 => false
        | fuel' + 1 =>
          if i + needleLen > haystackLen then false
          else if (haystack.drop i |>.take needleLen) == needle then true
          else loop (i + 1) fuel'
      loop 0 (haystackLen + 1)

testSuite "Chisel DDL"

-- CREATE TABLE tests

test "simple create table" := do
  let stmt := createTable "users"
    |>.column "id" .integer [.primaryKey true]
    |>.column "name" .text [.notNull]
    |>.build
  let sql := renderCreateTable {} stmt
  ensure (containsSubstr sql "CREATE TABLE users") "should have CREATE TABLE"
  ensure (containsSubstr sql "id INTEGER PRIMARY KEY AUTOINCREMENT") "should have id column"
  ensure (containsSubstr sql "name TEXT NOT NULL") "should have name column"

test "create table if not exists" := do
  let stmt := createTable "users"
    |>.ifNotExists
    |>.column "id" .integer []
    |>.build
  let sql := renderCreateTable {} stmt
  ensure (containsSubstr sql "IF NOT EXISTS") "should have IF NOT EXISTS"

test "create temporary table" := do
  let stmt := createTable "temp_data"
    |>.temporary
    |>.column "id" .integer []
    |>.build
  let sql := renderCreateTable {} stmt
  ensure (containsSubstr sql "TEMPORARY TABLE") "should have TEMPORARY"

test "create strict table" := do
  let stmt := createTable "strict_data"
    |>.strict
    |>.column "id" .integer []
    |>.build
  let sql := renderCreateTable {} stmt
  ensure (containsSubstr sql "STRICT") "should have STRICT"

test "column with default value" := do
  let stmt := createTable "users"
    |>.column "created_at" .datetime [.default now]
    |>.build
  let sql := renderCreateTable {} stmt
  ensure (containsSubstr sql "DEFAULT DATETIME('now')") "should have DEFAULT"

test "column with check constraint" := do
  let stmt := createTable "users"
    |>.column "age" .integer [.check (col "age" .>= val 0)]
    |>.build
  let sql := renderCreateTable {} stmt
  ensure (containsSubstr sql "CHECK") "should have CHECK"

test "column with foreign key" := do
  let stmt := createTable "orders"
    |>.column "user_id" .integer [.references "users" "id" (some .cascade) none]
    |>.build
  let sql := renderCreateTable {} stmt
  ensure (containsSubstr sql "REFERENCES users(id)") "should have REFERENCES"
  ensure (containsSubstr sql "ON DELETE CASCADE") "should have ON DELETE CASCADE"

test "table with composite primary key" := do
  let stmt := createTable "order_items"
    |>.column "order_id" .integer []
    |>.column "product_id" .integer []
    |>.primaryKey ["order_id", "product_id"]
    |>.build
  let sql := renderCreateTable {} stmt
  ensure (containsSubstr sql "PRIMARY KEY (order_id, product_id)") "should have composite PK"

test "table with unique constraint" := do
  let stmt := createTable "users"
    |>.column "email" .text []
    |>.unique ["email"]
    |>.build
  let sql := renderCreateTable {} stmt
  ensure (containsSubstr sql "UNIQUE (email)") "should have UNIQUE"

test "idColumn helper" := do
  let stmt := createTable "users"
    |>.idColumn
    |>.build
  let sql := renderCreateTable {} stmt
  ensure (containsSubstr sql "id INTEGER PRIMARY KEY AUTOINCREMENT") "should have id column"

-- DROP TABLE tests

test "drop table" := do
  let stmt := dropTable "users"
  renderDropTable {} stmt ≡ "DROP TABLE users"

test "drop table if exists" := do
  let stmt := dropTable "users" true
  renderDropTable {} stmt ≡ "DROP TABLE IF EXISTS users"

-- ALTER TABLE tests

test "alter table add column" := do
  let stmt := alterTable "users"
    |>.addColumn' "phone" .text [.notNull]
    |>.build
  let sql := renderAlterTable {} stmt
  ensure (containsSubstr sql "ADD COLUMN phone TEXT NOT NULL") "should have ADD COLUMN"

test "alter table drop column" := do
  let stmt := alterTable "users"
    |>.dropColumn "phone"
    |>.build
  let sql := renderAlterTable {} stmt
  ensure (containsSubstr sql "DROP COLUMN phone") "should have DROP COLUMN"

test "alter table rename column" := do
  let stmt := alterTable "users"
    |>.renameColumn "phone" "mobile"
    |>.build
  let sql := renderAlterTable {} stmt
  ensure (containsSubstr sql "RENAME COLUMN phone TO mobile") "should have RENAME COLUMN"

test "alter table rename table" := do
  let stmt := alterTable "users"
    |>.renameTable "people"
    |>.build
  let sql := renderAlterTable {} stmt
  ensure (containsSubstr sql "RENAME TO people") "should have RENAME TO"

-- CREATE INDEX tests

test "simple create index" := do
  let stmt := createIndex "idx_users_email" "users"
    |>.column "email"
    |>.build
  let sql := renderCreateIndex {} stmt
  ensure (containsSubstr sql "CREATE INDEX idx_users_email ON users (email)") "should have CREATE INDEX"

test "create unique index" := do
  let stmt := createUniqueIndex "idx_users_email" "users"
    |>.column "email"
    |>.build
  let sql := renderCreateIndex {} stmt
  ensure (containsSubstr sql "UNIQUE INDEX") "should have UNIQUE"

test "create index if not exists" := do
  let stmt := createIndex "idx_test" "test"
    |>.ifNotExists
    |>.column "col"
    |>.build
  let sql := renderCreateIndex {} stmt
  ensure (containsSubstr sql "IF NOT EXISTS") "should have IF NOT EXISTS"

test "create index with sort direction" := do
  let stmt := createIndex "idx_test" "test"
    |>.columnDesc "created_at"
    |>.build
  let sql := renderCreateIndex {} stmt
  ensure (containsSubstr sql "DESC") "should have DESC"

test "create partial index" := do
  let stmt := createIndex "idx_active_users" "users"
    |>.column "email"
    |>.where_ (col "active" .== bool true)
    |>.build
  let sql := renderCreateIndex {} stmt
  ensure (containsSubstr sql "WHERE") "should have WHERE"

test "drop index" := do
  let stmt := dropIndex "idx_test"
  renderDropIndex {} stmt ≡ "DROP INDEX idx_test"

test "drop index if exists" := do
  let stmt := dropIndex "idx_test" true
  renderDropIndex {} stmt ≡ "DROP INDEX IF EXISTS idx_test"

#generate_tests

end Tests.DDL
