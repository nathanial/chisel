/-
  Chisel Test Suite
-/
import Crucible
import Tests.Literal
import Tests.Expr
import Tests.Select
import Tests.DML
import Tests.DDL
import Tests.Parser

open Crucible

def main : IO UInt32 := do
  IO.println "Chisel SQL DSL Tests"
  IO.println "===================="
  runAllSuites
