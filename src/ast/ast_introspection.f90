module ast_introspection
    ! AST Node Introspection APIs for Static Analysis
    ! Provides comprehensive API for examining AST nodes and their properties
    
    use ast_core
    use type_system_hm, only: mono_type_t
    implicit none
    private

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

        ! Use select type to determine node type and return standard constants
        select type (node)
        type is (program_node)
            type_id = 1
        type is (function_def_node)
            type_id = 2
        type is (assignment_node)
            type_id = 3
        type is (binary_op_node)
            type_id = 4
        type is (identifier_node)
            type_id = 5
        type is (literal_node)
            type_id = 6
        type is (array_literal_node)
            type_id = 7
        type is (call_or_subscript_node)
            type_id = 8
        type is (subroutine_def_node)
            type_id = 9
        type is (subroutine_call_node)
            type_id = 10
        type is (declaration_node)
            type_id = 11
        type is (parameter_declaration_node)
            type_id = 12
        type is (if_node)
            type_id = 13
        type is (do_loop_node)
            type_id = 14
        type is (do_while_node)
            type_id = 15
        type is (select_case_node)
            type_id = 16
        type is (case_block_node)
            type_id = 17
        type is (module_node)
            type_id = 18
        type is (use_statement_node)
            type_id = 19
        type is (print_statement_node)
            type_id = 20
        type is (write_statement_node)
            type_id = 21
        type is (read_statement_node)
            type_id = 22
        type is (allocate_statement_node)
            type_id = 23
        type is (deallocate_statement_node)
            type_id = 24
        type is (stop_node)
            type_id = 25
        type is (return_node)
            type_id = 26
        type is (cycle_node)
            type_id = 27
        type is (exit_node)
            type_id = 28
        type is (where_node)
            type_id = 29
        type is (interface_block_node)
            type_id = 30
        type is (derived_type_node)
            type_id = 31
        type is (pointer_assignment_node)
            type_id = 32
        type is (forall_node)
            type_id = 33
        type is (case_range_node)
            type_id = 34
        type is (case_default_node)
            type_id = 35
        type is (complex_literal_node)
            type_id = 36
        type is (include_statement_node)
            type_id = 37
        type is (contains_node)
            type_id = 38
        type is (format_descriptor_node)
            type_id = 39
        class default
            type_id = 99
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