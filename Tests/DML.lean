/-
  DML (INSERT, UPDATE, DELETE) tests
-/
import Chisel
import Crucible

namespace Tests.DML

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

testSuite "Chisel DML"

-- INSERT tests

test "simple insert renders correctly" := do
  let stmt := insertInto "users"
    |>.columns ["name", "email"]
    |>.values [str "Alice", str "alice@example.com"]
    |>.build
  let sql := renderInsert {} stmt
  ensure (containsSubstr sql "INSERT INTO users") "should have INSERT INTO"
  ensure (containsSubstr sql "(name, email)") "should have columns"
  ensure (containsSubstr sql "VALUES") "should have VALUES"

test "insert multiple rows" := do
  let stmt := insertInto "users"
    |>.columns ["name"]
    |>.values [str "Alice"]
    |>.values [str "Bob"]
    |>.build
  let sql := renderInsert {} stmt
  ensure (containsSubstr sql "'Alice'") "should have Alice"
  ensure (containsSubstr sql "'Bob'") "should have Bob"

test "insert or ignore" := do
  let stmt := insertInto "users"
    |>.orIgnore
    |>.columns ["name"]
    |>.values [str "Alice"]
    |>.build
  let sql := renderInsert {} stmt
  ensure (containsSubstr sql "OR IGNORE") "should have OR IGNORE"

test "insert or replace" := do
  let stmt := insertInto "users"
    |>.orReplace
    |>.columns ["name"]
    |>.values [str "Alice"]
    |>.build
  let sql := renderInsert {} stmt
  ensure (containsSubstr sql "OR REPLACE") "should have OR REPLACE"

test "insert with returning" := do
  let stmt := insertInto "users"
    |>.columns ["name"]
    |>.values [str "Alice"]
    |>.returningAll
    |>.build
  let sql := renderInsert {} stmt
  ensure (containsSubstr sql "RETURNING *") "should have RETURNING *"

-- UPDATE tests

test "simple update renders correctly" := do
  let stmt := update "users"
    |>.set "name" (str "Alice")
    |>.where_ (col "id" .== val 1)
    |>.build
  let sql := renderUpdate {} stmt
  ensure (containsSubstr sql "UPDATE users SET") "should have UPDATE SET"
  ensure (containsSubstr sql "name = 'Alice'") "should have assignment"
  ensure (containsSubstr sql "WHERE") "should have WHERE"

test "update multiple columns" := do
  let stmt := update "users"
    |>.set "name" (str "Alice")
    |>.set "age" (val 30)
    |>.where_ (col "id" .== val 1)
    |>.build
  let sql := renderUpdate {} stmt
  ensure (containsSubstr sql "name = 'Alice'") "should have name"
  ensure (containsSubstr sql "age = 30") "should have age"

test "update with returning" := do
  let stmt := update "users"
    |>.set "name" (str "Alice")
    |>.where_ (col "id" .== val 1)
    |>.returningAll
    |>.build
  let sql := renderUpdate {} stmt
  ensure (containsSubstr sql "RETURNING *") "should have RETURNING"

-- DELETE tests

test "simple delete renders correctly" := do
  let stmt := deleteFrom "users"
    |>.where_ (col "id" .== val 1)
    |>.build
  let sql := renderDelete {} stmt
  ensure (containsSubstr sql "DELETE FROM users") "should have DELETE FROM"
  ensure (containsSubstr sql "WHERE") "should have WHERE"

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
  ensure (containsSubstr sql "RETURNING *") "should have RETURNING"

#generate_tests

end Tests.DML
