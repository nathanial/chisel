# Chisel Roadmap

This document outlines potential improvements, new features, and code cleanup opportunities for the Chisel SQL DSL library.

## Feature Proposals

### [Priority: High] UNION, INTERSECT, and EXCEPT Support
**Description:** Add support for SQL set operations (UNION, UNION ALL, INTERSECT, EXCEPT) to combine SELECT statements.

**Rationale:** These are fundamental SQL operations commonly used in production queries. The current implementation only supports single SELECT statements with no way to combine results.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Core/Expr.lean` - Add SetOp type and compound SELECT AST
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Render/Expr.lean` - Render compound SELECT
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Builder/Select.lean` - Add union/intersect/except builder methods
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Parser.lean` - Parse set operations

**Estimated Effort:** Medium

**Dependencies:** None

---

### [Priority: High] Common Table Expressions (WITH Clause)
**Description:** Add support for Common Table Expressions (CTEs), including recursive CTEs.

**Rationale:** CTEs are essential for complex queries, improving readability and enabling recursive queries for hierarchical data (trees, graphs).

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Core/Expr.lean` - Add CTE structure
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Core/Select.lean` - Extend SelectStmt with WITH clause
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Render/Expr.lean` - Render WITH clause
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Builder/Select.lean` - Add with_/cte builder methods
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Parser.lean` - Parse WITH clause

**Estimated Effort:** Large

**Dependencies:** None

---

### [Priority: High] Window Functions
**Description:** Add support for SQL window functions (OVER clause with PARTITION BY and ORDER BY).

**Rationale:** Window functions are critical for analytics queries (running totals, rankings, moving averages). Many modern applications require these for reporting.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Core/Expr.lean` - Add WindowSpec and window function expressions
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/DSL/Expr.lean` - Add window function helpers (ROW_NUMBER, RANK, LAG, LEAD, etc.)
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Render/Expr.lean` - Render OVER clause
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Parser.lean` - Parse window functions

**Estimated Effort:** Large

**Dependencies:** None

---

### [Priority: High] PostgreSQL ON CONFLICT Clause (Upsert)
**Description:** Add support for PostgreSQL's ON CONFLICT clause for upsert operations.

**Rationale:** The current implementation only supports SQLite's simple conflict actions (OR IGNORE, OR REPLACE). PostgreSQL's ON CONFLICT provides more sophisticated upsert behavior with DO UPDATE SET and conflict targets.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Core/DML.lean` - Add OnConflict structure with conflict targets and actions
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Builder/Insert.lean` - Add onConflictDoNothing/onConflictDoUpdate methods
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Render/DML.lean` - Render dialect-specific conflict handling
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Parser.lean` - Parse ON CONFLICT clause

**Estimated Effort:** Medium

**Dependencies:** None

---

### [Priority: Medium] INSERT ... ON DUPLICATE KEY UPDATE (MySQL)
**Description:** Add MySQL-specific INSERT ... ON DUPLICATE KEY UPDATE support.

**Rationale:** MySQL uses different syntax from PostgreSQL for upsert operations. Supporting multiple dialects properly requires dialect-specific upsert implementations.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Core/DML.lean`
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Render/DML.lean`

**Estimated Effort:** Small

**Dependencies:** PostgreSQL ON CONFLICT implementation (for design consistency)

---

### [Priority: Medium] Subquery Builder Integration
**Description:** Add fluent builder methods for embedding subqueries in SELECT, FROM, and WHERE clauses.

**Rationale:** While subqueries are supported in the AST, there is no ergonomic builder API to construct them. Users must manually construct SelectCore instances.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Builder/Select.lean` - Add fromSubquery, whereIn (subquery variant), exists_ methods

**Estimated Effort:** Small

**Dependencies:** None

---

### [Priority: Medium] JSON Functions
**Description:** Add JSON function support for SQLite (json_extract, json_array, etc.) and PostgreSQL (jsonb operators).

**Rationale:** JSON support is increasingly common in modern databases. SQLite has extensive JSON support, and PostgreSQL's JSONB is widely used.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/DSL/Expr.lean` - Add JSON function helpers
- New file: `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/DSL/Json.lean`

**Estimated Effort:** Medium

**Dependencies:** None

---

### [Priority: Medium] Full-Text Search Functions
**Description:** Add FTS support for SQLite (MATCH, fts5) and PostgreSQL (to_tsvector, to_tsquery).

**Rationale:** Full-text search is a common requirement. Having built-in support would improve developer experience for search-heavy applications.

**Affected Files:**
- New file: `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/DSL/FTS.lean`
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Render/Expr.lean` - Dialect-specific FTS rendering

**Estimated Effort:** Medium

**Dependencies:** None

---

### [Priority: Medium] Type-Safe Schema Integration
**Description:** Create a schema definition system that can validate queries at compile time against known table schemas.

**Rationale:** The current DSL validates SQL structure but not whether columns exist in referenced tables. Compile-time schema validation would catch more errors.

**Affected Files:**
- New file: `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Schema.lean`
- New file: `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Typed.lean`

**Estimated Effort:** Large

**Dependencies:** None

---

### [Priority: Medium] Quarry Integration
**Description:** Provide direct integration with the Quarry SQLite library for executing Chisel queries.

**Rationale:** Currently Chisel only generates SQL strings. Integration with Quarry would provide a complete solution for database operations without requiring manual string handling.

**Affected Files:**
- New file: `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Quarry.lean`

**Estimated Effort:** Medium

**Dependencies:** Quarry library

---

### [Priority: Low] TRUNCATE TABLE Support
**Description:** Add TRUNCATE TABLE statement support.

**Rationale:** TRUNCATE is faster than DELETE for removing all rows and resets auto-increment counters.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Core/DDL.lean`
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Render/DDL.lean`
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Parser.lean`

**Estimated Effort:** Small

**Dependencies:** None

---

### [Priority: Low] CREATE VIEW Support
**Description:** Add CREATE VIEW and DROP VIEW statement support.

**Rationale:** Views are useful for encapsulating complex queries and providing abstraction layers.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Core/DDL.lean`
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Render/DDL.lean`
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Builder/Table.lean`
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Parser.lean`

**Estimated Effort:** Small

**Dependencies:** None

---

### [Priority: Low] CREATE TRIGGER Support
**Description:** Add CREATE TRIGGER and DROP TRIGGER statement support.

**Rationale:** Triggers are used for audit logging, data validation, and maintaining derived data.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Core/DDL.lean`
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Render/DDL.lean`
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Parser.lean`

**Estimated Effort:** Medium

**Dependencies:** None

---

### [Priority: Low] EXPLAIN/EXPLAIN ANALYZE Support
**Description:** Add support for EXPLAIN and EXPLAIN ANALYZE for query planning analysis.

**Rationale:** Useful for debugging and optimizing query performance.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Core/DML.lean`
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Render/DML.lean`

**Estimated Effort:** Small

**Dependencies:** None

---

## Code Improvements

### [Priority: High] Reduce Boilerplate in Parameter Binding Functions
**Current State:** The `bindPositional`, `bindNamed`, and `bindIndexed` functions in `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Parser/Param.lean` have significant code duplication. Each function manually traverses the Expr AST with similar patterns.

**Proposed Change:** Implement a generic traversal function (`mapExprM`) that abstracts the tree traversal, allowing parameter binding to focus only on the transformation logic.

**Benefits:** Reduced code duplication, easier maintenance, less error-prone.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Parser/Param.lean`

**Estimated Effort:** Medium

---

### [Priority: High] Improve Parser Error Messages
**Current State:** Parser error messages show position and expected tokens but lack context about what clause or statement is being parsed.

**Proposed Change:** Add context stack to track parsing location (e.g., "in WHERE clause", "in JOIN condition") and include in error messages.

**Benefits:** Better developer experience when debugging parse failures.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Parser/Core.lean`
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Parser.lean`

**Estimated Effort:** Medium

---

### [Priority: Medium] Add Expr.fold and Expr.map Functions
**Current State:** Expression manipulation requires manual pattern matching on all Expr variants.

**Proposed Change:** Add catamorphism (fold) and functor (map) operations for Expr type to simplify transformations and analyses.

**Benefits:** Cleaner code for expression transformations, parameter substitution, optimization passes.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Core/Expr.lean`

**Estimated Effort:** Medium

---

### [Priority: Medium] Extract Dialect-Specific Rendering
**Current State:** Dialect differences are handled with conditionals scattered throughout render functions.

**Proposed Change:** Create a DialectConfig structure and extract dialect-specific logic into separate functions or a typeclass.

**Benefits:** Easier to add new dialects, cleaner separation of concerns.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Render/Expr.lean`
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Render/DML.lean`
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Render/DDL.lean`

**Estimated Effort:** Medium

---

### [Priority: Medium] Use StringBuilder for SQL Rendering
**Current State:** SQL rendering uses string interpolation and concatenation which creates many intermediate strings.

**Proposed Change:** Use a StringBuilder or similar accumulator pattern for more efficient string building.

**Benefits:** Better performance for large queries, reduced memory allocation.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Render/Expr.lean`
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Render/DML.lean`
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Render/DDL.lean`

**Estimated Effort:** Medium

---

### [Priority: Medium] Convert SelectCore from Inductive to Structure
**Current State:** SelectCore is defined as an inductive type with a single constructor to enable mutual recursion with Expr. This results in verbose accessor functions (lines 155-213 in Expr.lean) and setter functions.

**Proposed Change:** Consider alternative designs:
1. Use a structure with lazy/thunk fields for recursive references
2. Use indexed types or type-level linking
3. Accept the current design but generate accessors via metaprogramming

**Benefits:** Cleaner code, better integration with Lean's structure system.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Core/Expr.lean`

**Estimated Effort:** Large (requires careful design)

---

### [Priority: Low] Add Repr Instances for All AST Types
**Current State:** Some AST types derive Repr, but complex types like SelectCore, Expr do not have full Repr instances.

**Proposed Change:** Implement Repr for all AST types for better debugging.

**Benefits:** Easier debugging, better REPL experience.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Core/Expr.lean`
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Core/DML.lean`
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Core/DDL.lean`

**Estimated Effort:** Small

---

### [Priority: Low] Add BEq Instances for AST Types
**Current State:** Only some types derive BEq. Expr and other mutual types lack equality instances.

**Proposed Change:** Implement BEq for all AST types to enable expression comparison.

**Benefits:** Enables query deduplication, testing, and optimization passes.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Core/Expr.lean`
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Core/DML.lean`
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Core/DDL.lean`

**Estimated Effort:** Medium

---

## Code Cleanup

### [Priority: High] Add Missing Test Coverage for Parser Edge Cases
**Issue:** Parser tests exist but some edge cases are not covered:
- Scientific notation in numbers (e.g., 1e10)
- Unicode in identifiers
- Comments in various positions
- Very long identifiers
- Escaped characters in strings beyond single quotes

**Location:** `/Users/Shared/Projects/lean-workspace/data/chisel/Tests/Parser.lean`

**Action Required:** Add additional test cases for edge cases.

**Estimated Effort:** Small

---

### [Priority: Medium] Add Doc Comments to Public API
**Issue:** While the code is readable, many public functions lack doc comments explaining their purpose and usage.

**Location:**
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/DSL/Expr.lean`
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Builder/Select.lean`
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Builder/Insert.lean`
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Builder/Update.lean`
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Builder/Delete.lean`

**Action Required:** Add `/-! -/` module docs and `/-- -/` function docs.

**Estimated Effort:** Medium

---

### [Priority: Medium] Consolidate Reserved Keyword Lists
**Issue:** Reserved keywords are defined in two places:
1. `sqlKeywords` in `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Parser/Lexer.lean` (lines 12-31)
2. `isReserved` in `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Render/Expr.lean` (lines 33-44)

**Location:** Multiple files as listed above.

**Action Required:** Create a single source of truth for SQL keywords and reference it from both locations.

**Estimated Effort:** Small

---

### [Priority: Medium] Remove Partial Annotations Where Possible
**Issue:** Several parser functions are marked `partial` but may be provably terminating:
- `skipWs`, `many`, `skipMany` in Core.lean
- Expression parsing functions in Parser.lean

**Location:**
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Parser/Core.lean`
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Parser/Lexer.lean`
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Parser.lean`

**Action Required:** Investigate termination proofs using fuel/gas pattern or well-founded recursion.

**Estimated Effort:** Large (may not be worth it for parsing)

---

### [Priority: Low] Consistent Naming for Alias Parameters
**Issue:** Alias parameters are inconsistently named: `alias_` (with underscore to avoid keyword conflict) vs `a` in various places.

**Location:**
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Builder/Update.lean` - uses both `alias_` and `a`
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Builder/Delete.lean` - uses both
- `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Core/DML.lean` - uses `alias_`

**Action Required:** Standardize on `alias_` throughout and rename local variables for clarity.

**Estimated Effort:** Small

---

### [Priority: Low] Add Property-Based Tests
**Issue:** All tests are example-based. Property-based tests would improve confidence in parser/renderer round-tripping.

**Location:** `/Users/Shared/Projects/lean-workspace/data/chisel/Tests/`

**Action Required:** Add property tests verifying:
- `parse(render(expr)) == expr` for well-formed expressions
- All rendered SQL is parseable
- Parameter binding preserves structure

**Estimated Effort:** Medium

**Dependencies:** Property-based testing library (plausible)

---

### [Priority: Low] Consider Splitting Parser.lean
**Issue:** `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Parser.lean` is 960 lines, handling parsing for expressions, SELECT, DML, DDL, and the public API.

**Location:** `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Parser.lean`

**Action Required:** Consider splitting into:
- `Parser/Expr.lean` - Expression parsing
- `Parser/Select.lean` - SELECT parsing
- `Parser/DML.lean` - INSERT/UPDATE/DELETE parsing
- `Parser/DDL.lean` - CREATE/ALTER/DROP parsing
- `Parser.lean` - Public API and re-exports

**Estimated Effort:** Small

---

### [Priority: Low] Fix Minor Float Rendering Issue
**Issue:** Float literal rendering uses `toString` which may produce unexpected precision for some values.

**Location:** `/Users/Shared/Projects/lean-workspace/data/chisel/Chisel/Core/Literal.lean` (line 51)

**Action Required:** Consider using a custom float-to-string function with controlled precision, or document the behavior.

**Estimated Effort:** Small

---

## Summary

### High Priority Items
1. UNION/INTERSECT/EXCEPT support
2. Common Table Expressions (CTEs)
3. Window functions
4. PostgreSQL ON CONFLICT (Upsert)
5. Reduce parameter binding boilerplate
6. Improve parser error messages
7. Add missing parser edge case tests

### Medium Priority Items
1. MySQL ON DUPLICATE KEY UPDATE
2. Subquery builder integration
3. JSON functions
4. Full-text search functions
5. Type-safe schema integration
6. Quarry integration
7. Add Expr.fold/map functions
8. Extract dialect-specific rendering
9. Use StringBuilder for rendering
10. Add doc comments
11. Consolidate keyword lists

### Low Priority Items
1. TRUNCATE TABLE
2. CREATE VIEW
3. CREATE TRIGGER
4. EXPLAIN/EXPLAIN ANALYZE
5. Repr/BEq instances
6. Remove partial annotations
7. Consistent alias naming
8. Property-based tests
9. Split Parser.lean
10. Float rendering precision
