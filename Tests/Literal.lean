/-
  Literal rendering tests
-/
import Chisel
import Crucible

namespace Tests.Literal

open Crucible
open Chisel

testSuite "Chisel Literals"

test "null renders as NULL" := do
  Literal.null.render ≡ "NULL"

test "bool true renders as TRUE" := do
  (Literal.bool true).render ≡ "TRUE"

test "bool false renders as FALSE" := do
  (Literal.bool false).render ≡ "FALSE"

test "int renders as number" := do
  (Literal.int 42).render ≡ "42"

test "negative int renders correctly" := do
  (Literal.int (-17)).render ≡ "-17"

test "float renders as number" := do
  let s := (Literal.float 3.14).render
  ensure (s.startsWith "3.14") "should start with 3.14"

test "string renders with quotes" := do
  (Literal.string "hello").render ≡ "'hello'"

test "string escapes single quotes" := do
  (Literal.string "it's").render ≡ "'it''s'"

test "blob renders as hex" := do
  let bytes := ByteArray.mk #[0xDE, 0xAD, 0xBE, 0xEF]
  (Literal.blob bytes).render ≡ "X'DEADBEEF'"

end Tests.Literal
