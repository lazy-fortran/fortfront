# AST Factory

## Purpose

This directory provides factory methods for creating AST nodes in the arena. Factory functions abstract the allocation details and provide a type-safe, convenient interface for constructing nodes during parsing. Each factory module corresponds to a family of related AST nodes.

Factory methods handle arena allocation, node initialization, field assignment, and registration in the node index.

## File Index

| File | Description |
|------|-------------|
| ast_factory.f90 | Public facade for all factory methods |
| ast_factory_core.f90 | Core node factories: programs, modules, functions, variables, literals |
| ast_factory_procedures.f90 | Procedure node factories: parameters, results, internal procedures, entries |
| ast_factory_declarations.f90 | Declaration node factories: type declarations, variable declarations, attributes |
| ast_factory_expressions.f90 | Expression node factories: binary ops, unary ops, function calls, indexing |
| ast_factory_statements.f90 | Statement node factories: assignments, print, return, continue |
| ast_factory_control.f90 | Control flow factory facade (includes two parts below) |
| ast_factory_control_part1.inc | Control flow factories part 1: if/elseif/else, select case/type |
| ast_factory_control_part2.inc | Control flow factories part 2: where/elsewhere, goto, exit, cycle |
| ast_factory_arrays.f90 | Array node factories: constructors, slicing, indexing, reshape |
| ast_factory_io.f90 | I/O node factories: read, write, print, open, close, format |
| ast_factory_errors.f90 | Error node factories: parse error recovery nodes |
| ast_factory_generics.f90 | LFortran generics factories: template blocks and instantiate statements |

## Key Concepts

**Factory Method Pattern**
- For arena allocation details, see [AST Arena README](../arena/README.md)
- Encapsulates node allocation and initialization
- Provides type-safe construction interface
- Hides arena details from callers

**Typical Factory Signature**
```fortran
function create_if_node(arena, condition_index, then_body, else_body) &
    result(node_index)
    type(ast_arena_t), intent(inout) :: arena
    integer, intent(in) :: condition_index
    integer, intent(in) :: then_body(:)
    integer, intent(in), optional :: else_body(:)
    integer :: node_index
end function
```

**Factory Responsibilities**
1. Allocate node in arena
2. Initialize all required fields
3. Set default values for optional fields
4. Register node in global index
5. Return node index for reference

**Node Index Management**
- Factory returns integer index, not pointer
- Index used for all subsequent node access
- Arena + index = safe node reference
- Prevents dangling pointer issues

**Error Node Creation**
- Special factories for parse error recovery
- Error nodes preserve partial parse information
- Enable continued parsing after errors
- Support error reporting with context

**Modularization**
- Factories grouped by node family
- Split modules when approaching size limits
- Facade modules provide unified interface
- Includes (.inc) used for oversized factories

## Dependencies

**AST Infrastructure**
- `ast/arena/` - Arena allocation for nodes
- `ast/nodes/` - Node type definitions
- `ast/ast_base` - Base types and constants

**Common Utilities**
- `common/identifier_table` - Identifier interning
- `common/uid_generator` - Unique node IDs
