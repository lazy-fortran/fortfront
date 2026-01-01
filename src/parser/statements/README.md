# Parser Statements

## Purpose

This directory handles parsing of executable statements: assignments, I/O operations, control transfers, memory management, and data initialization. Statement parsing is complex due to context-sensitive keywords and ambiguous syntax that requires lookahead and disambiguation.

## File Index

| File | Description |
|------|-------------|
| parser_statement_core.f90 | Core statement parsing orchestration |
| parser_statement_detection.f90 | Statement type detection and routing |
| parser_statement_utilities.f90 | Statement parsing utilities |
| parser_statement_callbacks.f90 | Callback interface for statement processing |
| parser_basic_statement_module.f90 | Basic statement parsing: continue, stop, return |
| parser_execution_statements.f90 | Include wrapper for executable statement dispatcher |
| parser_execution_statements_module.inc | Executable statement parsing dispatcher implementation |
| parser_io_statements.f90 | Include wrapper for I/O statement parsing |
| parser_io_statements_common.inc | I/O parsing helpers and implied do parsing |
| parser_io_statements_parsers.inc | I/O statement parsing: read, write, print, open, close |
| parser_intrinsic_statements.f90 | Intrinsic statement parsing |
| parser_external_statements.f90 | External statement parsing (EXTERNAL attribute) |
| parser_legacy_statements.f90 | Legacy Fortran statement parsing (goto, computed goto) |
| parser_memory_statements.f90 | Memory management: allocate, deallocate, nullify |
| parser_statement_data_module.f90 | Data statement parsing |
| parser_keyword_disambiguation.f90 | Disambiguate keywords from identifiers in statement context |

## Key Concepts

**Statement Type Detection**
- First token determines statement type:
  - `print` → I/O statement
  - `if` → Control flow (handled in control_flow/)
  - `do` → Loop (handled in control_flow/)
  - Identifier → Assignment or call statement
- Lookahead for disambiguation:
  - `data x /5/` → data statement
  - `data = 5` → assignment statement

**I/O Statements**
- **Read**: `read(*, *) x, y, z`, `read(10, fmt='(I5)') value`
- **Write**: `write(*, *) x, y, z`, `write(10, fmt='(F10.5)') value`
- **Print**: `print *, x, y, z`, `print '(A)', message`
- **Open**: `open(unit=10, file='data.txt', status='old')`
- **Close**: `close(unit=10, status='keep')`
- **Inquire**: `inquire(file='data.txt', exist=file_exists)`
- **Format**: `format(I5, F10.5, A)` (format specification)

**Control Transfer Statements**
- **Return**: `return`, `return expr` (alternate return legacy)
- **Stop**: `stop`, `stop 'error message'`, `stop error_code`
- **Continue**: `continue` (no-op, legacy loop marker)
- **Exit**: `exit`, `exit loop_name`
- **Cycle**: `cycle`, `cycle loop_name`
- **Goto**: `goto label` (legacy)

**Memory Management**
- **Allocate**: `allocate(array(10, 20), stat=ierr)`
- **Deallocate**: `deallocate(array, stat=ierr)`
- **Nullify**: `nullify(pointer)` (set pointer to null)

**Data Statements**
- Initialize variables: `data x, y, z / 1, 2, 3 /`
- Array initialization: `data array / 100*0.0 /`
- Character data: `data name / 'John' /`
- Multiple groups: `data x, y / 1, 2 / z / 3 /`

**Keyword Disambiguation**
- Many Fortran keywords can also be identifiers
- Context determines interpretation:
  - Statement start → likely keyword
  - After `=` → identifier
  - After `.` → identifier (structure component)
- Lookahead resolves ambiguity:
  - `data x /5/` → data statement (keyword)
  - `data = 5` → assignment (identifier)

**Assignment vs Call**
- Both start with identifier: `var`, `sub`
- Next token disambiguates:
  - `var = expr` → assignment
  - `sub(args)` → call statement (if subroutine)
  - `var(indices)` → array assignment
- Type information helps (from previous passes)

## Dependencies

**Parser Core**
- `parser/core/parser_state` - State management
- `parser/core/parser_dispatcher` - Statement routing

**Parser Expressions**
- `parser/expressions/parser_assignment` - Assignment parsing
- `parser/expressions/parser_expressions` - Expression parsing

**AST Factory**
- `ast/factory/ast_factory_statements` - Statement node creation
- `ast/factory/ast_factory_io` - I/O node creation

**Common Utilities**
- `common/identifier_table` - Identifier management
