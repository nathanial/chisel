/-
  SELECT builder tests
-/
import Chisel
import Crucible
import Staple

namespace Tests.Select

open Crucible
open Chisel
open Staple (String.containsSubstr)

testSuite "Chisel SELECT Builder"

test "simple select renders correctly" := do
  let query := SelectM.build do
    select_ (col "name")
    from_ "users"
  renderSelect {} query ≡ "SELECT name FROM users"

test "select star renders correctly" := do
  let query := SelectM.build do
    selectAll
    from_ "users"
  renderSelect {} query ≡ "SELECT * FROM users"

test "select with alias renders correctly" := do
  let query := SelectM.build do
    select_ (col "name") (some "user_name")
    from_ "users"
  renderSelect {} query ≡ "SELECT name AS user_name FROM users"

test "select multiple columns" := do
  let query := SelectM.build do
    select_ (col "id")
    select_ (col "name")
    select_ (col "email")
    from_ "users"
  renderSelect {} query ≡ "SELECT id, name, email FROM users"

test "select with where clause" := do
  let query := SelectM.build do
    selectAll
    from_ "users"
    where_ (col "active" .== bool true)
  renderSelect {} query ≡ "SELECT * FROM users WHERE (active = TRUE)"

test "select with multiple where conditions" := do
  let query := SelectM.build do
    selectAll
    from_ "users"
    where_ (col "age" .>= val 18)
    where_ (col "active" .== bool true)
  renderSelect {} query ≡ "SELECT * FROM users WHERE ((age >= 18) AND (active = TRUE))"

test "select with inner join" := do
  let query := SelectM.build do
    select_ (col' "u" "name")
    select_ (col' "o" "total")
    from_ "users" (some "u")
    innerJoin "orders" (col' "o" "user_id" .== col' "u" "id") (some "o")
  let sql := renderSelect {} query
  ensure (String.containsSubstr sql "INNER JOIN") "should have INNER JOIN"
  ensure (String.containsSubstr sql "ON") "should have ON clause"

test "select with left join" := do
  let query := SelectM.build do
    selectAll
    from_ "users"
    leftJoin "orders" (col' "orders" "user_id" .== col' "users" "id")
  let sql := renderSelect {} query
  ensure (String.containsSubstr sql "LEFT JOIN") "should have LEFT JOIN"

test "select with group by" := do
  let query := SelectM.build do
    select_ (col "department")
    select_ (count (col "id")) (some "count")
    from_ "employees"
    groupBy_ [col "department"]
  let sql := renderSelect {} query
  ensure (String.containsSubstr sql "GROUP BY department") "should have GROUP BY"

test "select with having" := do
  let query := SelectM.build do
    select_ (col "department")
    select_ (count (col "id")) (some "count")
    from_ "employees"
    groupBy_ [col "department"]
    having_ (count (col "id") .> val 5)
  let sql := renderSelect {} query
  ensure (String.containsSubstr sql "HAVING") "should have HAVING"

test "select with order by" := do
  let query := SelectM.build do
    selectAll
    from_ "users"
    orderBy1 (col "created_at") .desc
  let sql := renderSelect {} query
  ensure (String.containsSubstr sql "ORDER BY created_at DESC") "should have ORDER BY DESC"

test "select with limit" := do
  let query := SelectM.build do
    selectAll
    from_ "users"
    limit_ 10
  renderSelect {} query ≡ "SELECT * FROM users LIMIT 10"

test "select with limit and offset" := do
  let query := SelectM.build do
    selectAll
    from_ "users"
    limit_ 10
    offset_ 20
  renderSelect {} query ≡ "SELECT * FROM users LIMIT 10 OFFSET 20"

test "select distinct" := do
  let query := SelectM.build do
    distinct
    select_ (col "category")
    from_ "products"
  renderSelect {} query ≡ "SELECT DISTINCT category FROM products"

test "complex select query" := do
  let query := SelectM.build do
    select_ (col' "u" "name") (some "user_name")
    select_ (count (col' "o" "id")) (some "order_count")
    select_ (sum (col' "o" "total")) (some "total_spent")
    from_ "users" (some "u")
    leftJoin "orders" (col' "o" "user_id" .== col' "u" "id") (some "o")
    where_ (col' "o" "created_at" .>= str "2024-01-01")
    groupBy_ [col' "u" "id"]
    having_ (count (col' "o" "id") .> val 5)
    orderBy1 (sum (col' "o" "total")) .desc
    limit_ 10
  let sql := renderSelect {} query
  ensure (String.containsSubstr sql "SELECT") "should have SELECT"
  ensure (String.containsSubstr sql "LEFT JOIN") "should have LEFT JOIN"
  ensure (String.containsSubstr sql "WHERE") "should have WHERE"
  ensure (String.containsSubstr sql "GROUP BY") "should have GROUP BY"
  ensure (String.containsSubstr sql "HAVING") "should have HAVING"
  ensure (String.containsSubstr sql "ORDER BY") "should have ORDER BY"
  ensure (String.containsSubstr sql "LIMIT") "should have LIMIT"

end Tests.Select
