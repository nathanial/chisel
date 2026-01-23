/-
  DML (INSERT, UPDATE, DELETE) tests
-/
import Chisel
import Crucible
import Staple

namespace Tests.DML

open Crucible
open Chisel
open Staple (String.containsSubstr)

testSuite "Chisel DML"

-- INSERT tests

test "simple insert renders correctly" := do
  let stmt := insertInto "users"
    |>.columns ["name", "email"]
    |>.values [str "Alice", str "alice@example.com"]
    |>.build
  let sql := renderInsert {} stmt
  ensure (String.containsSubstr sql "INSERT INTO users") "should have INSERT INTO"
  ensure (String.containsSubstr sql "(name, email)") "should have columns"
  ensure (String.containsSubstr sql "VALUES") "should have VALUES"

test "insert multiple rows" := do
  let stmt := insertInto "users"
    |>.columns ["name"]
    |>.values [str "Alice"]
    |>.values [str "Bob"]
    |>.build
  let sql := renderInsert {} stmt
  ensure (String.containsSubstr sql "'Alice'") "should have Alice"
  ensure (String.containsSubstr sql "'Bob'") "should have Bob"

test "insert or ignore" := do
  let stmt := insertInto "users"
    |>.orIgnore
    |>.columns ["name"]
    |>.values [str "Alice"]
    |>.build
  let sql := renderInsert {} stmt
  ensure (String.containsSubstr sql "OR IGNORE") "should have OR IGNORE"

test "insert or replace" := do
  let stmt := insertInto "users"
    |>.orReplace
    |>.columns ["name"]
    |>.values [str "Alice"]
    |>.build
  let sql := renderInsert {} stmt
  ensure (String.containsSubstr sql "OR REPLACE") "should have OR REPLACE"

test "insert with returning" := do
  let stmt := insertInto "users"
    |>.columns ["name"]
    |>.values [str "Alice"]
    |>.returningAll
    |>.build
  let sql := renderInsert {} stmt
  ensure (String.containsSubstr sql "RETURNING *") "should have RETURNING *"

-- UPDATE tests

test "simple update renders correctly" := do
  let stmt := update "users"
    |>.set "name" (str "Alice")
    |>.where_ (col "id" .== val 1)
    |>.build
  let sql := renderUpdate {} stmt
  ensure (String.containsSubstr sql "UPDATE users SET") "should have UPDATE SET"
  ensure (String.containsSubstr sql "name = 'Alice'") "should have assignment"
  ensure (String.containsSubstr sql "WHERE") "should have WHERE"

test "update multiple columns" := do
  let stmt := update "users"
    |>.set "name" (str "Alice")
    |>.set "age" (val 30)
    |>.where_ (col "id" .== val 1)
    |>.build
  let sql := renderUpdate {} stmt
  ensure (String.containsSubstr sql "name = 'Alice'") "should have name"
  ensure (String.containsSubstr sql "age = 30") "should have age"

test "update with returning" := do
  let stmt := update "users"
    |>.set "name" (str "Alice")
    |>.where_ (col "id" .== val 1)
    |>.returningAll
    |>.build
  let sql := renderUpdate {} stmt
  ensure (String.containsSubstr sql "RETURNING *") "should have RETURNING"

-- DELETE tests

test "simple delete renders correctly" := do
  let stmt := deleteFrom "users"
    |>.where_ (col "id" .== val 1)
    |>.build
  let sql := renderDelete {} stmt
  ensure (String.containsSubstr sql "DELETE FROM users") "should have DELETE FROM"
  ensure (String.containsSubstr sql "WHERE") "should have WHERE"

test "delete all rows" := do
  let stmt := deleteFrom "temp_data"
    |>.build
  renderDelete {} stmt ≡ "DELETE FROM temp_data"

test "delete with returning" := do
  let stmt := deleteFrom "users"
    |>.where_ (col "id" .== val 1)
    |>.returningAll
    |>.build
  let sql := renderDelete {} stmt
  ensure (String.containsSubstr sql "RETURNING *") "should have RETURNING"

end Tests.DML
