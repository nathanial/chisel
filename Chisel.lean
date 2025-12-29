/-
  Chisel - Type-Safe SQL DSL for Lean 4

  A composable SQL query builder with compile-time syntactic validation.

  ## Quick Start

  ```lean
  import Chisel
  open Chisel

  -- Build a SELECT query
  def query := SelectM.build do
    select_ (col "name")
    select_ (col "email")
    from_ "users"
    where_ (col "active" .== bool true)
    orderBy1 (col "created_at") .desc
    limit_ 10

  #eval renderSelect {} query
  -- "SELECT name, email FROM users WHERE (active = TRUE) ORDER BY created_at DESC LIMIT 10"
  ```

  ## Features

  - Type-safe SQL construction
  - Monadic builder for SELECT queries
  - Fluent builders for INSERT, UPDATE, DELETE
  - Full DDL support (CREATE TABLE, ALTER, DROP, INDEX)
  - Multiple SQL dialect support (SQLite, PostgreSQL, MySQL)
-/

-- Core AST types
import Chisel.Core.Literal
import Chisel.Core.Expr
import Chisel.Core.Select
import Chisel.Core.DML
import Chisel.Core.DDL

-- Builder APIs
import Chisel.Builder.Select
import Chisel.Builder.Insert
import Chisel.Builder.Update
import Chisel.Builder.Delete
import Chisel.Builder.Table
import Chisel.Builder.Index

-- Expression DSL
import Chisel.DSL.Expr

-- Rendering
import Chisel.Render.Expr
import Chisel.Render.DML
import Chisel.Render.DDL

-- Parser
import Chisel.Parser
