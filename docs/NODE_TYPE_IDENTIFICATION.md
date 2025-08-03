# AST Node Type Identification Strategy

This document provides a comprehensive guide for identifying and working with AST node types in the fortfront public API.

## Overview

The fortfront compiler uses a polymorphic AST (Abstract Syntax Tree) design where all nodes inherit from a common `ast_node` base class. This document explains the recommended patterns for identifying and working with specific node types.

## Table of Contents

1. [Node Hierarchy](#node-hierarchy)
2. [Primary Identification Patterns](#primary-identification-patterns)
3. [Arena-Based Type Search](#arena-based-type-search)
4. [Visitor Pattern](#visitor-pattern)
5. [Best Practices](#best-practices)
6. [Common Pitfalls](#common-pitfalls)
7. [Performance Considerations](#performance-considerations)
8. [Examples](#examples)

## Node Hierarchy

The fortfront AST uses a well-structured inheritance hierarchy:

```
ast_node (abstract base)
├── Core Nodes (ast_nodes_core.f90)
│   ├── program_node
│   ├── assignment_node
│   ├── identifier_node
│   ├── literal_node
│   ├── binary_op_node
│   └── call_or_subscript_node
├── Control Flow Nodes (ast_nodes_control.f90)
│   ├── if_node
│   ├── do_loop_node
│   ├── select_case_node
│   └── associate_node
├── Procedure Nodes (ast_nodes_procedure.f90)
│   ├── function_def_node
│   └── subroutine_def_node
├── Data Nodes (ast_nodes_data.f90)
│   ├── declaration_node
│   └── module_node
└── I/O Nodes (ast_nodes_io.f90)
    ├── print_statement_node
    └── write_statement_node
```

## Primary Identification Patterns

### 1. Type Constants and Helper Functions (Recommended for Issue #34)

The fortfront public API provides integer constants and helper functions for efficient node type identification:

```fortran
use fortfront

! Using node type constants
if (get_node_type(arena, index) == NODE_ASSIGNMENT) then
    ! Process assignment
end if

! Using direct node type identification  
class(ast_node), allocatable :: node
node = arena%entries(index)%node
if (get_node_type_id(node) == NODE_ASSIGNMENT) then
    ! Process assignment
end if
```

**Available Constants:**
- `NODE_PROGRAM`, `NODE_ASSIGNMENT`, `NODE_BINARY_OP`
- `NODE_IDENTIFIER`, `NODE_LITERAL`, `NODE_ARRAY_LITERAL`
- `NODE_CALL_OR_SUBSCRIPT`, `NODE_FUNCTION_DEF`, `NODE_SUBROUTINE_DEF`
- `NODE_DECLARATION`, `NODE_PARAMETER_DECLARATION`
- `NODE_IF`, `NODE_DO_LOOP`, `NODE_DO_WHILE`, `NODE_SELECT_CASE`
- `NODE_MODULE`, `NODE_USE_STATEMENT`, `NODE_PRINT_STATEMENT`
- `NODE_COMMENT`, `NODE_WHERE`, `NODE_FORALL` and more...

**Helper Functions:**
- `get_node_type_id(node)` - Get type constant from node object
- `get_node_type(arena, index)` - Get type constant from arena index

**Advantages:**
- O(1) integer comparison - fastest method
- Consistent API across all tools
- No risk of typos in type names
- Future-proof for new node types

### 2. `select type` with `type is` (Alternative Pattern)

This is an alternative pattern for type identification:

```fortran
use ast_core
use ast_nodes_core  ! Import specific node types

select type (node => arena%entries(index)%node)
type is (program_node)
    ! Handle program node
    write(*, '(A)') 'Program name: ' // node%name
    
type is (assignment_node)
    ! Handle assignment node
    target_idx = node%target_index
    value_idx = node%value_index
    
type is (identifier_node)
    ! Handle identifier node
    write(*, '(A)') 'Identifier: ' // node%name
    
type is (literal_node)
    ! Handle literal node  
    write(*, '(A)') 'Literal value: ' // node%value
    
type is (binary_op_node)
    ! Handle binary operation
    left_idx = node%left_index
    right_idx = node%right_index
    operator = node%operator
    
class default
    ! Handle unknown or unhandled types
    write(*, '(A)') 'Unhandled node type'
end select
```

**Advantages:**
- Compile-time type safety
- Excellent performance
- Exhaustive checking with `class default`
- IDE support for auto-completion

### 2. Nested Type Checking

For complex scenarios requiring multiple type checks:

```fortran
select type (node => arena%entries(index)%node)
type is (call_or_subscript_node)
    ! Check if it's an array access or function call
    if (node%is_array_access) then
        write(*, '(A)') 'Array access detected'
    else
        write(*, '(A)') 'Function call detected'
    end if
    
type is (range_subscript_node)
    ! Check if it's a character substring
    if (node%is_character_substring) then
        write(*, '(A)') 'Character substring detected'
    else
        write(*, '(A)') 'Array range detected'
    end if
end select
```

## Arena-Based Type Search

The arena provides string-based type searching for bulk operations:

### Finding Nodes by Type Name

```fortran
use ast_core

! Find all assignment nodes
integer, allocatable :: assign_indices(:)
assign_indices = arena%find_by_type("assignment")

! Process all assignments
do i = 1, size(assign_indices)
    select type (node => arena%entries(assign_indices(i))%node)
    type is (assignment_node)
        ! Process assignment
        call process_assignment(node)
    end select
end do
```

### Available Type Names

Common type names for string-based search:
- `"program"` - Program nodes
- `"assignment"` - Assignment statements
- `"identifier"` - Variable/function identifiers
- `"literal"` - Literal values
- `"binary_op"` - Binary operations
- `"call_or_subscript"` - Function calls/array access
- `"if"` - If statements
- `"do_loop"` - Do loops
- `"associate"` - Associate constructs

## Visitor Pattern

For systematic AST traversal, use the visitor pattern:

```fortran
use ast_visitor

type, extends(ast_visitor_t) :: my_visitor_t
contains
    procedure :: visit_program => my_visit_program
    procedure :: visit_assignment => my_visit_assignment
    procedure :: visit_identifier => my_visit_identifier
    ! ... implement other visit methods
end type

subroutine my_visit_program(this, node)
    class(my_visitor_t), intent(inout) :: this
    class(program_node), intent(in) :: node
    
    write(*, '(A)') 'Visiting program: ' // node%name
end subroutine

! Usage:
type(my_visitor_t) :: visitor
call node%accept(visitor)
```

## Best Practices

### 1. Always Use `class default`

```fortran
select type (node => arena%entries(index)%node)
type is (assignment_node)
    ! Handle assignment
class default
    ! Always include this for future-proofing
    error stop 'Unexpected node type encountered'
end select
```

### 2. Import Specific Node Types

```fortran
! Good: Import only what you need
use ast_nodes_core, only: assignment_node, identifier_node

! Avoid: Importing everything
use ast_nodes_core  ! Imports all node types
```

### 3. Check Allocation Before Access

```fortran
select type (node => arena%entries(index)%node)
type is (program_node)
    if (allocated(node%body_indices)) then
        ! Safe to access body_indices
        call process_body(node%body_indices)
    end if
end select
```

### 4. Use Meaningful Variable Names

```fortran
! Good: Clear variable names
select type (assign_node => arena%entries(index)%node)
type is (assignment_node)
    target_var_idx = assign_node%target_index

! Avoid: Generic names that confuse purpose  
select type (n => arena%entries(index)%node)
type is (assignment_node)
    idx = n%target_index
```

## Common Pitfalls

### 1. Missing `class default`

```fortran
! WRONG: No default case
select type (node => arena%entries(index)%node)
type is (assignment_node)
    ! Handle assignment
! Missing class default - future node types will be silently ignored
end select

! CORRECT: Always include default
select type (node => arena%entries(index)%node)
type is (assignment_node)
    ! Handle assignment
class default
    error stop 'Unhandled node type'
end select
```

### 2. Accessing Unallocated Arrays

```fortran
! WRONG: Direct access without checking
select type (node => arena%entries(index)%node)
type is (program_node)
    first_stmt = node%body_indices(1)  ! May crash if empty
end select

! CORRECT: Check allocation first
select type (node => arena%entries(index)%node)
type is (program_node)
    if (allocated(node%body_indices) .and. size(node%body_indices) > 0) then
        first_stmt = node%body_indices(1)
    end if
end select
```

### 3. Incorrect Type Names in String Search

```fortran
! WRONG: Incorrect type name
nodes = arena%find_by_type("assignments")  ! Should be "assignment"

! CORRECT: Use exact type names
nodes = arena%find_by_type("assignment")
```

## Performance Considerations

### 1. Prefer `select type` Over String Search

```fortran
! FASTER: Direct type checking
select type (node => arena%entries(index)%node)
type is (assignment_node)
    ! Process directly
end select

! SLOWER: String-based search when you already have the node
if (arena%entries(index)%node_type == "assignment") then
    ! String comparison overhead
end if
```

### 2. Cache Type Search Results

```fortran
! Good: Cache results for repeated access
integer, allocatable :: assignment_indices(:)
assignment_indices = arena%find_by_type("assignment")

do i = 1, size(assignment_indices)
    ! Process each assignment
end do

! Avoid: Repeated searches
do i = 1, arena%size
    if (arena%entries(i)%node_type == "assignment") then
        ! Inefficient repeated string comparisons
    end if
end do
```

## Examples

### Example 1: Simple AST Processing

```fortran
program process_ast
    use ast_core
    use ast_nodes_core
    implicit none
    
    type(ast_arena_t) :: arena
    integer :: i
    
    ! ... populate arena ...
    
    ! Process all nodes in the arena
    do i = 1, arena%size
        select type (node => arena%entries(i)%node)
        type is (program_node)
            write(*, '(A)') 'Found program: ' // node%name
            
        type is (assignment_node)
            write(*, '(A,I0,A,I0)') 'Assignment: target=', &
                node%target_index, ' value=', node%value_index
                
        type is (identifier_node)
            write(*, '(A)') 'Identifier: ' // node%name
            
        class default
            write(*, '(A)') 'Other node type: ' // arena%entries(i)%node_type
        end select
    end do
end program
```

### Example 2: Finding Function Calls

```fortran
subroutine find_function_calls(arena)
    use ast_core
    use ast_nodes_core
    type(ast_arena_t), intent(in) :: arena
    
    integer, allocatable :: call_indices(:)
    integer :: i
    
    ! Find all potential function calls
    call_indices = arena%find_by_type("call_or_subscript")
    
    write(*, '(A,I0,A)') 'Found ', size(call_indices), ' call/subscript nodes'
    
    do i = 1, size(call_indices)
        select type (node => arena%entries(call_indices(i))%node)
        type is (call_or_subscript_node)
            if (.not. node%is_array_access) then
                write(*, '(A)') 'Function call: ' // node%name
            end if
        end select
    end do
end subroutine
```

### Example 3: AST Visitor Implementation

```fortran
module my_ast_analysis
    use ast_visitor
    use ast_nodes_core
    implicit none
    
    type, extends(ast_visitor_t) :: statement_counter_t
        integer :: assignment_count = 0
        integer :: identifier_count = 0
    contains
        procedure :: visit_assignment => count_assignment
        procedure :: visit_identifier => count_identifier
    end type
    
contains
    
    subroutine count_assignment(this, node)
        class(statement_counter_t), intent(inout) :: this
        class(assignment_node), intent(in) :: node
        
        this%assignment_count = this%assignment_count + 1
    end subroutine
    
    subroutine count_identifier(this, node)
        class(statement_counter_t), intent(inout) :: this
        class(identifier_node), intent(in) :: node
        
        this%identifier_count = this%identifier_count + 1
    end subroutine
    
end module

! Usage:
program analyze_ast
    use my_ast_analysis
    use ast_core
    
    type(statement_counter_t) :: counter
    type(ast_arena_t) :: arena
    integer :: i
    
    ! ... populate arena ...
    
    ! Visit all nodes
    do i = 1, arena%size
        call arena%entries(i)%node%accept(counter)
    end do
    
    write(*, '(A,I0)') 'Assignments found: ', counter%assignment_count
    write(*, '(A,I0)') 'Identifiers found: ', counter%identifier_count
end program
```

## Conclusion

The fortfront AST provides flexible and efficient node type identification through:

1. **`select type`** - Primary pattern for direct type checking
2. **Arena search** - Bulk operations and type filtering  
3. **Visitor pattern** - Systematic traversal and analysis

Choose the appropriate pattern based on your use case:
- Use `select type` for processing individual nodes
- Use arena search for finding all nodes of specific types
- Use visitor pattern for comprehensive AST analysis

Always follow the best practices to ensure robust, maintainable code that handles future AST extensions gracefully.