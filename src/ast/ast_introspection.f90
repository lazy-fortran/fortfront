module ast_introspection
    ! AST Node Introspection APIs for Static Analysis
    ! Provides comprehensive API for examining AST nodes and their properties
    
    use ast_core
    use type_system_hm, only: mono_type_t
    implicit none
    private

    ! Import source location type from existing definitions
    ! Note: source_location_t is already defined in fortfront.f90

    ! Node type constants (re-export from fortfront for consistency)
    integer, parameter, public :: NODE_PROGRAM = 1
    integer, parameter, public :: NODE_FUNCTION_DEF = 2
    integer, parameter, public :: NODE_ASSIGNMENT = 3
    integer, parameter, public :: NODE_BINARY_OP = 4
    integer, parameter, public :: NODE_IDENTIFIER = 5
    integer, parameter, public :: NODE_LITERAL = 6
    integer, parameter, public :: NODE_ARRAY_LITERAL = 7
    integer, parameter, public :: NODE_CALL_OR_SUBSCRIPT = 8
    integer, parameter, public :: NODE_SUBROUTINE_DEF = 9
    integer, parameter, public :: NODE_SUBROUTINE_CALL = 10
    integer, parameter, public :: NODE_DECLARATION = 11
    integer, parameter, public :: NODE_PARAMETER_DECLARATION = 12
    integer, parameter, public :: NODE_IF = 13
    integer, parameter, public :: NODE_DO_LOOP = 14
    integer, parameter, public :: NODE_DO_WHILE = 15
    integer, parameter, public :: NODE_SELECT_CASE = 16
    integer, parameter, public :: NODE_CASE_BLOCK = 17
    integer, parameter, public :: NODE_MODULE = 18
    integer, parameter, public :: NODE_USE_STATEMENT = 19
    integer, parameter, public :: NODE_PRINT_STATEMENT = 20
    integer, parameter, public :: NODE_WRITE_STATEMENT = 21
    integer, parameter, public :: NODE_READ_STATEMENT = 22
    integer, parameter, public :: NODE_ALLOCATE_STATEMENT = 23
    integer, parameter, public :: NODE_DEALLOCATE_STATEMENT = 24
    integer, parameter, public :: NODE_STOP = 25
    integer, parameter, public :: NODE_RETURN = 26
    integer, parameter, public :: NODE_CYCLE = 27
    integer, parameter, public :: NODE_EXIT = 28
    integer, parameter, public :: NODE_WHERE = 29
    integer, parameter, public :: NODE_INTERFACE_BLOCK = 30
    integer, parameter, public :: NODE_DERIVED_TYPE = 31
    integer, parameter, public :: NODE_POINTER_ASSIGNMENT = 32
    integer, parameter, public :: NODE_FORALL = 33
    integer, parameter, public :: NODE_CASE_RANGE = 34
    integer, parameter, public :: NODE_CASE_DEFAULT = 35
    integer, parameter, public :: NODE_COMPLEX_LITERAL = 36
    integer, parameter, public :: NODE_INCLUDE_STATEMENT = 37
    integer, parameter, public :: NODE_CONTAINS = 38
    integer, parameter, public :: NODE_FORMAT_DESCRIPTOR = 39
    integer, parameter, public :: NODE_UNKNOWN = 99

    ! Public API functions from issue #12
    public :: get_node
    public :: get_node_type_id
    public :: get_node_source_location
    public :: has_semantic_info
    public :: get_node_type_info_from_arena

contains

    ! Get node by index from arena (issue #12 requirement)
    function get_node(arena, index) result(node)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        class(ast_node), allocatable :: node

        ! Bounds checking
        if (index <= 0 .or. index > arena%size) return
        if (.not. allocated(arena%entries)) return
        if (size(arena%entries) < index) return
        if (.not. allocated(arena%entries(index)%node)) return
        
        ! Return copy of the node
        allocate(node, source=arena%entries(index)%node)
    end function get_node

    ! Get node type identifier (issue #12 requirement)
    function get_node_type_id(node) result(type_id)
        class(ast_node), intent(in) :: node
        integer :: type_id

        ! Use select type to determine node type
        select type (node)
        type is (program_node)
            type_id = NODE_PROGRAM
        type is (function_def_node)
            type_id = NODE_FUNCTION_DEF
        type is (assignment_node)
            type_id = NODE_ASSIGNMENT
        type is (binary_op_node)
            type_id = NODE_BINARY_OP
        type is (identifier_node)
            type_id = NODE_IDENTIFIER
        type is (literal_node)
            type_id = NODE_LITERAL
        type is (array_literal_node)
            type_id = NODE_ARRAY_LITERAL
        type is (call_or_subscript_node)
            type_id = NODE_CALL_OR_SUBSCRIPT
        type is (subroutine_def_node)
            type_id = NODE_SUBROUTINE_DEF
        type is (subroutine_call_node)
            type_id = NODE_SUBROUTINE_CALL
        type is (declaration_node)
            type_id = NODE_DECLARATION
        type is (parameter_declaration_node)
            type_id = NODE_PARAMETER_DECLARATION
        type is (if_node)
            type_id = NODE_IF
        type is (do_loop_node)
            type_id = NODE_DO_LOOP
        type is (do_while_node)
            type_id = NODE_DO_WHILE
        type is (select_case_node)
            type_id = NODE_SELECT_CASE
        type is (case_block_node)
            type_id = NODE_CASE_BLOCK
        type is (module_node)
            type_id = NODE_MODULE
        type is (use_statement_node)
            type_id = NODE_USE_STATEMENT
        type is (print_statement_node)
            type_id = NODE_PRINT_STATEMENT
        type is (write_statement_node)
            type_id = NODE_WRITE_STATEMENT
        type is (read_statement_node)
            type_id = NODE_READ_STATEMENT
        type is (allocate_statement_node)
            type_id = NODE_ALLOCATE_STATEMENT
        type is (deallocate_statement_node)
            type_id = NODE_DEALLOCATE_STATEMENT
        type is (stop_node)
            type_id = NODE_STOP
        type is (return_node)
            type_id = NODE_RETURN
        type is (cycle_node)
            type_id = NODE_CYCLE
        type is (exit_node)
            type_id = NODE_EXIT
        type is (where_node)
            type_id = NODE_WHERE
        type is (interface_block_node)
            type_id = NODE_INTERFACE_BLOCK
        type is (derived_type_node)
            type_id = NODE_DERIVED_TYPE
        type is (pointer_assignment_node)
            type_id = NODE_POINTER_ASSIGNMENT
        type is (forall_node)
            type_id = NODE_FORALL
        type is (case_range_node)
            type_id = NODE_CASE_RANGE
        type is (case_default_node)
            type_id = NODE_CASE_DEFAULT
        type is (complex_literal_node)
            type_id = NODE_COMPLEX_LITERAL
        type is (include_statement_node)
            type_id = NODE_INCLUDE_STATEMENT
        type is (contains_node)
            type_id = NODE_CONTAINS
        type is (format_descriptor_node)
            type_id = NODE_FORMAT_DESCRIPTOR
        class default
            type_id = NODE_UNKNOWN
        end select
    end function get_node_type_id

    ! Get source location for any node (issue #12 requirement)
    ! Note: Returns a simple type that matches the existing source_location_t in fortfront.f90
    subroutine get_node_source_location(node, line, column)
        class(ast_node), intent(in) :: node
        integer, intent(out) :: line, column

        ! Extract line and column from node's line and column fields
        ! All AST nodes have line and column fields
        line = node%line
        column = node%column
    end subroutine get_node_source_location

    ! Check if a node has semantic information attached (issue #12 requirement)
    function has_semantic_info(node) result(has_info)
        class(ast_node), intent(in) :: node
        logical :: has_info

        ! Check if the node has inferred type information
        has_info = allocated(node%inferred_type)
    end function has_semantic_info

    ! Get inferred type information if available (issue #12 requirement)
    function get_node_type_info_from_arena(arena, index) result(type_info)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        type(mono_type_t), allocatable :: type_info

        ! Bounds checking
        if (index <= 0 .or. index > arena%size) return
        if (.not. allocated(arena%entries)) return
        if (size(arena%entries) < index) return
        if (.not. allocated(arena%entries(index)%node)) return
        if (.not. allocated(arena%entries(index)%node%inferred_type)) return
        
        ! Return copy of the inferred type if available
        allocate(type_info)
        type_info = arena%entries(index)%node%inferred_type
    end function get_node_type_info_from_arena

end module ast_introspection