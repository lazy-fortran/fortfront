# AST Module Migration Guide

## Status

As of 2025-10, `ast_core` no longer exists. This guide helps downstream forks migrate.

## Migration: Use Explicit Imports

**Instead of:**
```fortran
use ast_core  ! God module - hidden dependencies
```

**Use specific imports:**
```fortran
use ast_nodes_core, only: assignment_node, identifier_node, program_node
use ast_arena_modern, only: ast_arena_t, create_ast_arena
use ast_factory, only: create_assignment, create_identifier
```

## Available Modules

| Module | Key Types |
|--------|-----------|
| `ast_nodes_core` | `program_node`, `assignment_node`, `identifier_node` |
| `ast_nodes_control` | `if_node`, `do_loop_node`, `select_case_node` |
| `ast_nodes_procedure` | `function_def_node`, `subroutine_def_node` |
| `ast_nodes_data` | `declaration_node`, `module_node` |
| `ast_nodes_io` | `print_statement_node`, `read_statement_node` |
| `ast_arena_modern` | `ast_arena_t`, `create_ast_arena` |
| `ast_factory` | `create_assignment`, `create_identifier` |
| `ast_traversal` | `traverse_ast`, `visit_nodes` |

## Common Patterns

**AST Construction:**
```fortran
use ast_nodes_core, only: assignment_node
use ast_factory, only: create_assignment
node = create_assignment(...)
```

**AST Traversal:**
```fortran
use ast_arena_modern, only: ast_arena_t
use ast_traversal, only: traverse_program
call traverse_program(arena, prog)
```

## Finding Remaining References

```bash
rg "^\s*use\s+ast_core\b" -n src app test
```
