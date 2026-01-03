# AST Nodes

## Purpose

This directory contains all AST (Abstract Syntax Tree) node type definitions for representing Fortran programs. Each file defines a family of related node types with their data structures and metadata. These nodes are allocated in the AST arena and accessed via the visitor pattern.

Node types cover the complete Fortran language: programs, modules, procedures, expressions, statements, control flow, loops, arrays, derived types, and I/O operations.

## File Index

| File | Description |
|------|-------------|
| ast_nodes_core.f90 | Core program structure: programs, modules, functions, subroutines, variables, literals |
| ast_nodes_procedure.f90 | Procedure-related nodes: parameters, result declarations, internal procedures, entry points |
| ast_nodes_conditional.f90 | Conditional constructs: if/elseif/else blocks, select case/type, where/elsewhere |
| ast_nodes_control.f90 | Control flow: goto, continue, stop, exit, cycle, return |
| ast_nodes_loops.f90 | Loop constructs: do loops, do-while, implied-do arrays |
| ast_nodes_array.f90 | Array operations: indexing, slicing, array constructors, reshape |
| ast_nodes_bounds.f90 | Array bounds and specifications: dimension attributes, array shape information |
| ast_nodes_data.f90 | Data structures: derived types, type declarations, data statements, common blocks |
| ast_nodes_io.f90 | I/O operations: read, write, print, open, close, inquire, format statements |
| ast_nodes_transfer.f90 | Control transfer: call statements, entry statements, return statements |
| ast_nodes_associate.f90 | Associate constructs: associate blocks, local name binding |
| ast_nodes_misc.f90 | Miscellaneous nodes facade (includes two parts below) |
| ast_nodes_misc_part1.inc | Miscellaneous nodes part 1: interface blocks, use statements, imports |
| ast_nodes_misc_part2.inc | Miscellaneous nodes part 2: save attributes, equivalence, compiler directives |
| ast_nodes_generics.f90 | LFortran generics extensions: template blocks and instantiate statements |

## Key Concepts

**Node Type Families**
- Organized by syntactic category (not semantic meaning)
- Each family contains related node types
- Shared fields grouped within families
- Type-specific metadata included

**Node Structure**
- `node_kind` - Discriminates node type within family
- Type-specific fields (unions in Fortran derived types)
- Source location information (file, line, column)
- Optional type annotation (from semantic analysis)

**Memory Layout**
- For arena allocation details, see [AST Arena README](../arena/README.md)
- Fixed-size node types for predictable allocation
- No dynamic memory within nodes (use indices)

**Modularization Strategy**
- Files split when approaching 500-line target
- Includes (.inc files) used for oversized modules
- Facade modules provide unified interface
- See `ast_nodes_misc.f90` for split pattern

**Node Naming Convention**
- Type suffix: `_node_t` (e.g., `if_node_t`)
- Kind constants: `NODE_*` (e.g., `NODE_IF`)
- Descriptive names reflecting syntax

## Dependencies

**AST Infrastructure**
- `ast/ast_base` - Base types and constants
- `ast/ast_types` - Core AST type definitions

**Type System** (for typed nodes)
- `semantic/types/` - Type annotations after semantic analysis

**Common Utilities**
- `common/identifier_table` - Identifier string interning
- `common/uid_generator` - Unique node IDs
