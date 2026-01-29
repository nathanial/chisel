# CLAUDE.md

Type-safe SQL DSL for Lean 4 that generates SQL strings with compile-time validation.

## Build Commands

```bash
lake build
lake test
```

## Architecture

```
Chisel/
├── Core/           # AST types (Literal, Expr, Select, DML, DDL)
├── Builder/        # Fluent/monadic builders (Select, Insert, Update, Delete, Table, Index)
├── DSL/            # Expression operators (.==, .&&, etc.)
├── Render/         # SQL string generation (Expr, DML, DDL)
└── Parser/         # SQL parsing (Lexer, Param)
```

## Key Patterns

### SelectM Monadic Builder
```lean
def query := SelectM.build do
  select_ (col "name")
  from_ "users"
  where_ (col "active" .== bool true)
```

### Fluent Builders (INSERT/UPDATE/DELETE)
```lean
def insert := insertInto "users"
  |>.columns ["name", "email"]
  |>.values [str "Alice", str "alice@example.com"]
  |>.build
```

### Expression DSL
Infix operators prefixed with `.` to avoid Lean conflicts:
- Comparison: `.==`, `.!=`, `.<`, `.<=`, `.>`, `.>=`
- Logical: `.&&`, `.||`
- Arithmetic: `.+`, `.-`, `.*`, `./`, `.%`

### Render Context
```lean
let ctx : RenderContext := { dialect := .postgres, paramStyle := .dollar }
renderSelect ctx query
```

## Dependencies

- crucible (testing)
- staple (macros)
- sift (parser combinators)
