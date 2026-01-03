# AST Node Type Identification

## Node Hierarchy

```
ast_node (abstract base)
+-- Core Nodes: program_node, assignment_node, identifier_node, literal_node, binary_op_node
+-- Control Flow: if_node, do_loop_node, select_case_node, associate_node
+-- Procedures: function_def_node, subroutine_def_node
+-- Data: declaration_node, module_node
+-- I/O: print_statement_node, write_statement_node
```

## Primary Method: Type Constants

Use integer constants for O(1) type identification:

```fortran
use fortfront

if (get_node_type(arena, index) == NODE_ASSIGNMENT) then
    ! Process assignment
end if
```

**Available Constants**: `NODE_PROGRAM`, `NODE_ASSIGNMENT`, `NODE_BINARY_OP`, `NODE_IDENTIFIER`, `NODE_LITERAL`, `NODE_ARRAY_LITERAL`, `NODE_CALL_OR_SUBSCRIPT`, `NODE_FUNCTION_DEF`, `NODE_SUBROUTINE_DEF`, `NODE_DECLARATION`, `NODE_IF`, `NODE_DO_LOOP`, `NODE_MODULE`, `NODE_PRINT_STATEMENT`, etc.

## Alternative: select type

```fortran
use ast_nodes_core, only: assignment_node, identifier_node, literal_node

select type (node => arena%entries(index)%node)
type is (assignment_node)
    target_idx = node%target_index
    value_idx = node%value_index

type is (identifier_node)
    print *, 'Identifier: ', node%name

type is (literal_node)
    print *, 'Literal: ', node%value

class default
    print *, 'Unknown node type'
end select
```

## Arena-Based Type Search

Find all nodes of a specific type:

```fortran
integer, allocatable :: assign_indices(:)
assign_indices = arena%find_by_type("assignment")

do i = 1, size(assign_indices)
    select type (node => arena%entries(assign_indices(i))%node)
    type is (assignment_node)
        call process_assignment(node)
    end select
end do
```

**Type names**: `"program"`, `"assignment"`, `"identifier"`, `"literal"`, `"binary_op"`, `"call_or_subscript"`, `"if"`, `"do_loop"`, `"associate"`

## Visitor Pattern

```fortran
type, extends(ast_visitor_t) :: my_visitor_t
    integer :: assignment_count = 0
contains
    procedure :: visit_assignment => count_assignment
end type

subroutine count_assignment(this, node)
    class(my_visitor_t), intent(inout) :: this
    class(assignment_node), intent(in) :: node
    this%assignment_count = this%assignment_count + 1
end subroutine

! Usage
type(my_visitor_t) :: visitor
do i = 1, arena%size
    call arena%entries(i)%node%accept(visitor)
end do
```

## Best Practices

1. **Always include `class default`** in select type for future-proofing
2. **Import specific node types** instead of entire modules
3. **Check allocation before access**: `if (allocated(node%body_indices)) then`
4. **Cache search results** for repeated access instead of repeated searches
5. **Prefer type constants** over string search for performance
