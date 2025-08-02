module ast_introspection
    ! AST Node Introspection APIs for Static Analysis
    ! Provides comprehensive API for examining AST nodes and their properties
    
    use ast_core
    use ast_nodes_core
    use ast_nodes_procedure
    use ast_nodes_control
    use ast_nodes_data
    use ast_nodes_io
    use ast_nodes_misc
    use type_system_hm, only: mono_type_t
    implicit none
    private

    ! Public API functions from issue #12
    public :: visit_node_at  ! Safe node access using visitor pattern
    public :: get_node_type_id
    public :: get_node_source_location
    public :: has_semantic_info
    public :: get_node_type_info_from_arena  ! Legacy (returns unallocated)
    
    ! New safe read-only type access functions
    public :: get_node_type_kind
    public :: get_node_type_details
    
    ! Additional safe introspection functions that work with indices
    public :: get_node_type_id_from_arena
    public :: get_node_source_location_from_arena

contains

    ! Safe node access using visitor pattern (issue #12 requirement)
    ! 
    ! CRITICAL: Node Copying is FORBIDDEN
    ! ===================================
    ! AST nodes contain complex allocatable components (particularly recursive
    ! mono_type_t structures) that CANNOT be safely copied. Attempting to copy
    ! nodes will result in:
    !   - Segmentation faults
    !   - Double-free errors
    !   - Memory corruption
    !
    ! This function provides the ONLY safe way to access nodes by using the
    ! visitor pattern, which avoids all copying.
    !
    ! Usage:
    !   type(my_visitor_t) :: visitor
    !   call visit_node_at(arena, index, visitor)
    !
    ! For read-only access to specific properties, use:
    !   - get_node_type_id_from_arena() for node type identification
    !   - get_node_source_location_from_arena() for source location
    !   - get_node_type_kind() and get_node_type_details() for type information
    !
    subroutine visit_node_at(arena, index, visitor)
        use ast_visitor
        use ast_traversal, only: visit_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        class(ast_visitor_t), intent(inout) :: visitor
        
        ! Bounds checking
        if (index <= 0 .or. index > arena%size) return
        if (.not. allocated(arena%entries)) return
        if (size(arena%entries) < index) return
        if (.not. allocated(arena%entries(index)%node)) return
        
        ! Use the visitor pattern to safely access the node
        select type (node => arena%entries(index)%node)
        class is (ast_node)
            call visit_node(node, visitor)
        end select
    end subroutine visit_node_at

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

    ! Get basic type kind from node (safe read-only access)
    function get_node_type_kind(arena, index) result(type_kind)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        integer :: type_kind

        type_kind = 0  ! Unknown/no type
        
        ! Bounds checking
        if (index <= 0 .or. index > arena%size) return
        if (.not. allocated(arena%entries)) return
        if (size(arena%entries) < index) return
        if (.not. allocated(arena%entries(index)%node)) return
        if (.not. allocated(arena%entries(index)%node%inferred_type)) return
        
        ! Safe read-only access to basic type kind
        type_kind = arena%entries(index)%node%inferred_type%kind
    end function get_node_type_kind

    ! Get type details without dangerous deep copy (safe read-only access)
    subroutine get_node_type_details(arena, index, kind, type_size, is_allocatable, &
                                   is_pointer, found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        integer, intent(out) :: kind, type_size
        logical, intent(out) :: is_allocatable, is_pointer, found

        found = .false.
        kind = 0
        type_size = 0
        is_allocatable = .false.
        is_pointer = .false.
        
        ! Bounds checking
        if (index <= 0 .or. index > arena%size) return
        if (.not. allocated(arena%entries)) return
        if (size(arena%entries) < index) return
        if (.not. allocated(arena%entries(index)%node)) return
        if (.not. allocated(arena%entries(index)%node%inferred_type)) return
        
        ! Safe read-only access to type details
        associate (node_type => arena%entries(index)%node%inferred_type)
            kind = node_type%kind
            type_size = node_type%size
            is_allocatable = node_type%alloc_info%is_allocatable
            is_pointer = node_type%alloc_info%is_pointer
            found = .true.
        end associate
    end subroutine get_node_type_details

    ! Legacy function - now returns unallocated for safety
    function get_node_type_info_from_arena(arena, index) result(type_info)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        type(mono_type_t), allocatable :: type_info
        
        ! Always return unallocated to prevent segfaults
        ! Use get_node_type_kind() or get_node_type_details() instead
    end function get_node_type_info_from_arena

    ! Get node type ID directly from arena (safe, no copying)
    function get_node_type_id_from_arena(arena, index) result(type_id)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        integer :: type_id
        
        type_id = 99  ! Unknown by default
        
        ! Bounds checking
        if (index <= 0 .or. index > arena%size) return
        if (.not. allocated(arena%entries)) return
        if (size(arena%entries) < index) return
        if (.not. allocated(arena%entries(index)%node)) return
        
        ! Use existing function with the node reference
        type_id = get_node_type_id(arena%entries(index)%node)
    end function get_node_type_id_from_arena

    ! Get source location directly from arena (safe, no copying)
    subroutine get_node_source_location_from_arena(arena, index, line, column)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        integer, intent(out) :: line, column
        
        line = 0
        column = 0
        
        ! Bounds checking
        if (index <= 0 .or. index > arena%size) return
        if (.not. allocated(arena%entries)) return
        if (size(arena%entries) < index) return
        if (.not. allocated(arena%entries(index)%node)) return
        
        ! Direct access to node fields
        line = arena%entries(index)%node%line
        column = arena%entries(index)%node%column
    end subroutine get_node_source_location_from_arena

end module ast_introspection