# AST Module Migration Guide

## Overview

The `ast_core` module implements a "god module" anti-pattern that creates hidden dependencies and tight coupling. This guide provides step-by-step instructions for migrating to explicit module imports.

## Problem with ast_core

```fortran
! PROBLEMATIC: Hidden dependencies, tight coupling
use ast_core
```

**Issues:**
- Hidden dependencies: You don't see what modules you actually depend on
- Coupling explosion: Single import brings in entire AST ecosystem  
- Compilation cascades: Changes to any AST module force full recompilation
- Interface pollution: Access to internal implementation details

## Migration Strategy

### Step 1: Identify Required Types and Functions

Before migrating, identify what AST components your module actually uses:

```bash
# Find AST types used in your module
grep -E "(assignment_node|identifier_node|program_node)" your_module.f90
```

### Step 2: Replace ast_core with Explicit Imports

**Instead of:**
```fortran
use ast_core
```

**Use specific imports:**
```fortran
! Core AST types
use ast_nodes_core, only: assignment_node, identifier_node, literal_node, &
                          binary_op_node, call_or_subscript_node, program_node

! AST arena management
use ast_arena, only: ast_arena_t, init_ast_arena

! Control flow nodes (if needed)
use ast_nodes_control, only: if_node, do_loop_node, do_while_node

! Factory functions (if needed)  
use ast_factory, only: create_assignment, create_identifier
```

## Module-Specific Import Guide

### For Parser Modules
```fortran
use ast_nodes_core, only: assignment_node, identifier_node, literal_node, &
                          binary_op_node, program_node
use ast_nodes_control, only: if_node, do_loop_node  
use ast_arena, only: ast_arena_t
```

### For Semantic Analysis Modules
```fortran
use ast_nodes_core, only: assignment_node, identifier_node, program_node
use ast_nodes_data, only: declaration_node, parameter_declaration_node
use ast_arena, only: ast_arena_t
use ast_operations, only: get_node_type
```

### For Code Generation Modules
```fortran
use ast_nodes_core, only: program_node, assignment_node, identifier_node
use ast_nodes_procedure, only: function_def_node, subroutine_def_node
use ast_arena, only: ast_arena_t
use ast_traversal, only: traverse_ast
```

## Available AST Modules

| Module | Purpose | Key Types |
|--------|---------|-----------|
| `ast_nodes_core` | Core AST nodes | `program_node`, `assignment_node`, `identifier_node` |
| `ast_nodes_control` | Control flow | `if_node`, `do_loop_node`, `select_case_node` |
| `ast_nodes_procedure` | Procedures | `function_def_node`, `subroutine_def_node` |
| `ast_nodes_data` | Data declarations | `declaration_node`, `module_node` |
| `ast_nodes_io` | I/O statements | `print_statement_node`, `read_statement_node` |
| `ast_arena` | Arena management | `ast_arena_t`, `init_ast_arena` |
| `ast_factory` | Factory functions | `create_assignment`, `create_identifier` |
| `ast_operations` | AST utilities | `get_node_type`, `find_nodes` |
| `ast_traversal` | Tree traversal | `traverse_ast`, `visit_nodes` |

## Migration Benefits

After migration, you'll have:

- **Clear Dependencies**: Explicit imports show actual relationships
- **Faster Compilation**: Only compile needed modules  
- **Better Encapsulation**: Use minimal required interfaces
- **Refactoring Safety**: Move modules without breaking hidden dependencies
- **Reduced Coupling**: Module boundaries become meaningful again

## Gradual Migration Process

1. **Start Small**: Migrate test files first
2. **Core Modules**: Migrate parser, semantic analyzer, code generator
3. **Support Modules**: Migrate remaining modules
4. **Remove ast_core**: Delete the god module after all migrations

## Common Patterns

### Pattern 1: AST Construction
```fortran
! Old
use ast_core
node = create_assignment(...)

! New  
use ast_nodes_core, only: assignment_node
use ast_factory, only: create_assignment
node = create_assignment(...)
```

### Pattern 2: AST Traversal
```fortran
! Old
use ast_core
call traverse_program(arena, prog)

! New
use ast_arena, only: ast_arena_t
use ast_nodes_core, only: program_node
use ast_traversal, only: traverse_program
call traverse_program(arena, prog)
```

### Pattern 3: Type Checking
```fortran
! Old
use ast_core
select type (node => arena%entries(i)%node)

! New
use ast_nodes_core, only: assignment_node, identifier_node
use ast_arena, only: ast_arena_t
select type (node => arena%entries(i)%node)
```

## Testing Migration

After migration:

1. **Compilation**: Ensure module compiles
2. **Functionality**: Run module-specific tests
3. **Integration**: Run full test suite
4. **Performance**: Check compilation time improvements

## Support

For migration questions or issues:
- Review this guide
- Check existing migrated modules for examples
- Create GitHub issue with specific migration questions