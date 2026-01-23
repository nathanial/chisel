/-
  Expression rendering tests
-/
import Chisel
import Crucible
import Staple

namespace Tests.Expr

open Crucible
open Chisel
open Staple (String.containsSubstr)

testSuite "Chisel Expressions"

test "column renders correctly" := do
  renderExpr {} (col "name") ≡ "name"

test "qualified column renders with table" := do
  renderExpr {} (col' "users" "name") ≡ "users.name"

test "star renders correctly" := do
  renderExpr {} star ≡ "*"

test "table star renders correctly" := do
  renderExpr {} (tableStar "users") ≡ "users.*"

test "integer literal renders" := do
  renderExpr {} (val 42) ≡ "42"

test "string literal renders with quotes" := do
  renderExpr {} (str "hello") ≡ "'hello'"

test "equality renders correctly" := do
  renderExpr {} (col "x" .== val 1) ≡ "(x = 1)"

test "inequality renders correctly" := do
  renderExpr {} (col "x" .!= val 1) ≡ "(x <> 1)"

test "less than renders correctly" := do
  renderExpr {} (col "x" .< val 10) ≡ "(x < 10)"

test "AND renders correctly" := do
  renderExpr {} (col "a" .&& col "b") ≡ "(a AND b)"

test "OR renders correctly" := do
  renderExpr {} (or_ (col "a") (col "b")) ≡ "(a OR b)"

test "NOT renders correctly" := do
  renderExpr {} (not_ (col "active")) ≡ "(NOT active)"

test "IS NULL renders correctly" := do
  renderExpr {} (isNull (col "x")) ≡ "(x IS NULL)"

test "IS NOT NULL renders correctly" := do
  renderExpr {} (isNotNull (col "x")) ≡ "(x IS NOT NULL)"

test "BETWEEN renders correctly" := do
  renderExpr {} (between (col "x") (val 1) (val 10)) ≡ "(x BETWEEN 1 AND 10)"

test "IN renders correctly" := do
  renderExpr {} (in_ (col "x") [val 1, val 2, val 3]) ≡ "(x IN (1, 2, 3))"

test "LIKE renders correctly" := do
  renderExpr {} (like (col "name") "%test%") ≡ "(name LIKE '%test%')"

test "function renders correctly" := do
  renderExpr {} (upper (col "name")) ≡ "UPPER(name)"

test "COUNT renders correctly" := do
  renderExpr {} (count (col "id")) ≡ "COUNT(id)"

test "COUNT star renders correctly" := do
  renderExpr {} countAll ≡ "COUNT(*)"

test "COUNT DISTINCT renders correctly" := do
  renderExpr {} (countDistinct (col "type")) ≡ "COUNT(DISTINCT type)"

test "SUM renders correctly" := do
  renderExpr {} (sum (col "amount")) ≡ "SUM(amount)"

test "CAST renders correctly" := do
  renderExpr {} (cast (col "x") "TEXT") ≡ "CAST(x AS TEXT)"

test "CASE WHEN renders correctly" := do
  let expr := case_ [(col "x" .> val 0, str "positive")] (some (str "non-positive"))
  renderExpr {} expr ≡ "(CASE WHEN (x > 0) THEN 'positive' ELSE 'non-positive' END)"

test "arithmetic operators render correctly" := do
  renderExpr {} (col "a" .+ col "b") ≡ "(a + b)"
  renderExpr {} (col "a" .- col "b") ≡ "(a - b)"
  renderExpr {} (col "a" .* col "b") ≡ "(a * b)"
  renderExpr {} (col "a" ./ col "b") ≡ "(a / b)"

test "parameter with question mark style" := do
  renderExpr {} param ≡ "?"

test "parameter with dollar style" := do
  let ctx : RenderContext := { paramStyle := .dollar, paramIndex := 1 }
  renderExpr ctx param ≡ "$1"

test "named parameter with colon style" := do
  let ctx : RenderContext := { paramStyle := .colon }
  renderExpr ctx (namedParam "id") ≡ ":id"

test "raw SQL passes through" := do
  renderExpr {} (raw "CUSTOM_FUNC()") ≡ "CUSTOM_FUNC()"

end Tests.Expr
