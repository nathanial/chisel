# Chisel

A type-safe SQL DSL for Lean 4 that generates SQL strings with compile-time validation.

## Features

- **Full SQL support**: SELECT, INSERT, UPDATE, DELETE, CREATE TABLE, ALTER, DROP, indexes
- **Syntactic type safety**: Valid SQL structure verified at compile time
- **Standalone**: Generates SQL strings (no database dependency)
- **Multiple dialects**: SQLite, PostgreSQL, MySQL parameter styles

## Installation

Add to your `lakefile.lean`:

```lean
require chisel from git "https://github.com/nathanial/chisel" @ "v0.0.1"
```

## Usage

```lean
import Chisel
open Chisel
```

### SELECT Queries

Use the monadic `SelectM` builder for complex queries:

```lean
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

### JOINs

```lean
def joinQuery := SelectM.build do
  select_ (col' "u" "name") (some "user_name")
  select_ (count (col' "o" "id")) (some "order_count")
  from_ "users" (some "u")
  leftJoin "orders" (col' "o" "user_id" .== col' "u" "id") (some "o")
  groupBy_ [col' "u" "id"]
  having_ (count (col' "o" "id") .> val 5)
```

### INSERT

```lean
def insert := insertInto "users"
  |>.columns ["name", "email"]
  |>.values [str "Alice", str "alice@example.com"]
  |>.build

#eval renderInsert {} insert
-- "INSERT INTO users (name, email) VALUES ('Alice', 'alice@example.com')"
```

### UPDATE

```lean
def update := update "users"
  |>.set "name" (str "Bob")
  |>.set "updated_at" now
  |>.where_ (col "id" .== val 1)
  |>.build

#eval renderUpdate {} update
-- "UPDATE users SET name = 'Bob', updated_at = DATETIME('now') WHERE (id = 1)"
```

### DELETE

```lean
def delete := deleteFrom "users"
  |>.where_ (col "id" .== val 1)
  |>.build

#eval renderDelete {} delete
-- "DELETE FROM users WHERE (id = 1)"
```

### CREATE TABLE

```lean
def usersTable := createTable "users"
  |>.ifNotExists
  |>.column "id" .integer [.primaryKey true]
  |>.column "email" .text [.notNull, .unique]
  |>.column "name" (.varchar (some 255)) [.notNull]
  |>.column "created_at" .datetime [.default now]
  |>.build

#eval renderCreateTable {} usersTable
```

### CREATE INDEX

```lean
def emailIndex := createUniqueIndex "idx_users_email" "users"
  |>.column "email"
  |>.build

#eval renderCreateIndex {} emailIndex
-- "CREATE UNIQUE INDEX idx_users_email ON users (email)"
```

## Expression DSL

Chisel provides infix operators prefixed with `.` to avoid conflicts with Lean's built-in operators:

| Operator | SQL |
|----------|-----|
| `.==` | `=` |
| `.!=` | `<>` |
| `.<` | `<` |
| `.<=` | `<=` |
| `.>` | `>` |
| `.>=` | `>=` |
| `.&&` | `AND` |
| `.||` | `OR` |
| `.+` | `+` |
| `.-` | `-` |
| `.*` | `*` |
| `./` | `/` |
| `.%` | `%` |

### Expression Helpers

```lean
-- Column references
col "name"                    -- name
col' "users" "name"           -- users.name

-- Literals
val 42                        -- 42
str "hello"                   -- 'hello'
bool true                     -- TRUE
null                          -- NULL

-- Functions
count (col "id")              -- COUNT(id)
countAll                      -- COUNT(*)
sum (col "amount")            -- SUM(amount)
avg (col "price")             -- AVG(price)
min_ (col "date")             -- MIN(date)
max_ (col "date")             -- MAX(date)
upper (col "name")            -- UPPER(name)
lower (col "name")            -- LOWER(name)
coalesce [col "a", col "b"]   -- COALESCE(a, b)

-- Patterns
like (col "name") "%test%"    -- name LIKE '%test%'
between (col "x") (val 1) (val 10)  -- x BETWEEN 1 AND 10
in_ (col "status") [str "a", str "b"]  -- status IN ('a', 'b')

-- NULL handling
isNull (col "deleted_at")     -- deleted_at IS NULL
isNotNull (col "email")       -- email IS NOT NULL

-- Parameters (for prepared statements)
param                         -- ? (SQLite style)
namedParam "id"               -- :id or @id
indexedParam 1                -- $1 (PostgreSQL style)
```

## Dialects

Configure the render context for different SQL dialects:

```lean
-- SQLite (default)
let ctx : RenderContext := {}

-- PostgreSQL
let ctx : RenderContext := { dialect := .postgres, paramStyle := .dollar }

-- MySQL
let ctx : RenderContext := { dialect := .mysql, paramStyle := .question }
```

## Building

```bash
lake build
lake test
```

## License

MIT License - see [LICENSE](LICENSE) for details.
