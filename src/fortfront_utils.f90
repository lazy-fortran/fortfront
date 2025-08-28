module fortfront_utils
    ! fortfront Utility Functions - Basic AST navigation and manipulation
    ! This module contains utility functions for AST operations:
    ! - Node existence and basic access
    ! - Parent/child navigation
    ! - Type identification
    ! - Basic traversal operations
    
    use ast_core, only: ast_arena_t, ast_node, program_node, assignment_node, &
                       function_def_node, ast_arena_stats_t
    use semantic_analyzer, only: semantic_context_t
    use type_system_unified, only: mono_type_t
    use fortfront_types, only: source_range_t, diagnostic_t, &
                              NODE_PROGRAM, NODE_FUNCTION_DEF, NODE_ASSIGNMENT, NODE_BINARY_OP, &
                              NODE_IDENTIFIER, NODE_LITERAL, NODE_ARRAY_LITERAL, NODE_CALL_OR_SUBSCRIPT, &
                              NODE_SUBROUTINE_DEF, NODE_SUBROUTINE_CALL, NODE_DECLARATION, &
                              NODE_PARAMETER_DECLARATION, NODE_IF, NODE_DO_LOOP, NODE_DO_WHILE, &
                              NODE_SELECT_CASE, NODE_CASE_BLOCK, NODE_MODULE, NODE_USE_STATEMENT, &
                              NODE_PRINT_STATEMENT, NODE_WRITE_STATEMENT, NODE_READ_STATEMENT, &
                              NODE_ALLOCATE_STATEMENT, NODE_DEALLOCATE_STATEMENT, NODE_STOP, &
                              NODE_RETURN, NODE_GOTO, NODE_ERROR_STOP, NODE_CYCLE, NODE_EXIT, &
                              NODE_WHERE, NODE_INTERFACE_BLOCK, NODE_DERIVED_TYPE, &
                              NODE_POINTER_ASSIGNMENT, NODE_FORALL, NODE_CASE_RANGE, &
                              NODE_CASE_DEFAULT, NODE_COMPLEX_LITERAL, NODE_INCLUDE_STATEMENT, &
                              NODE_CONTAINS, NODE_FORMAT_DESCRIPTOR, NODE_COMMENT, &
                              NODE_IMPLICIT_STATEMENT, NODE_UNKNOWN
    
    implicit none
    private
    
    ! Public utility functions
    public :: node_exists, get_node_type_at, get_node_location, &
              get_parent, get_next_sibling, get_previous_sibling, &
              get_block_statements, is_last_in_block, is_block_node, &
              get_node_type, get_children, traverse_ast, traverse_node, &
              get_node_range, get_arena_stats, analyze_program, &
              get_type_for_node, get_diagnostics, ast_to_json, &
              semantic_info_to_json, find_nodes_by_type, get_max_depth, &
              get_node_as_program, get_node_as_assignment, get_node_as_function_def

contains

    ! Check if a node exists at the given index
    function node_exists(arena, node_index) result(exists)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        logical :: exists
        
        exists = node_index > 0 .and. node_index <= arena%size
        if (exists) then
            exists = allocated(arena%entries(node_index)%node)
        end if
    end function node_exists
    
    ! Get node type at index (returns empty string if invalid)
    function get_node_type_at(arena, node_index) result(node_type)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: node_type
        
        if (node_exists(arena, node_index)) then
            node_type = arena%entries(node_index)%node_type
        else
            node_type = ""
        end if
    end function get_node_type_at
    
    ! Get node line/column info
    subroutine get_node_location(arena, node_index, line, column)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, intent(out) :: line, column
        
        line = 0
        column = 0
        if (node_exists(arena, node_index)) then
            line = arena%entries(node_index)%node%line
            column = arena%entries(node_index)%node%column
        end if
    end subroutine get_node_location
    
    
    ! Get parent node for a given node index
    function get_parent(arena, node_index) result(parent_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer :: parent_index
        
        parent_index = 0
        if (node_index > 0 .and. node_index <= arena%size) then
            parent_index = arena%entries(node_index)%parent_index
        end if
    end function get_parent
    
    ! Get children indices for a node
    function get_children(arena, node_index) result(child_indices)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, allocatable :: child_indices(:)
        
        allocate(child_indices(0))
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%child_indices)) then
                child_indices = arena%entries(node_index)%child_indices( &
                    1:arena%entries(node_index)%child_count)
            end if
        end if
    end function get_children

    ! Get next sibling node in the same parent
    function get_next_sibling(arena, node_index) result(next_sibling)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer :: next_sibling
        
        next_sibling = arena%get_next_sibling(node_index)
    end function get_next_sibling

    ! Get previous sibling node in the same parent
    function get_previous_sibling(arena, node_index) result(prev_sibling)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer :: prev_sibling
        
        prev_sibling = arena%get_previous_sibling(node_index)
    end function get_previous_sibling

    ! Get all statements in a block (for block nodes like if, do, etc.)
    function get_block_statements(arena, block_index) result(stmt_indices)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: block_index
        integer, allocatable :: stmt_indices(:)
        
        stmt_indices = arena%get_block_statements(block_index)
    end function get_block_statements

    ! Check if a statement is the last executable statement in its block
    function is_last_in_block(arena, node_index) result(is_last)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        logical :: is_last
        
        is_last = arena%is_last_in_block(node_index)
    end function is_last_in_block

    ! Check if a node represents a block (contains statements)
    function is_block_node(arena, node_index) result(is_block)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        logical :: is_block
        
        is_block = arena%is_block_node(node_index)
    end function is_block_node
    
    ! Traverse AST with callback procedure
    subroutine traverse_ast(arena, root_index, callback, pre_order)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        interface
            subroutine callback(arena, node_index)
                import :: ast_arena_t
                type(ast_arena_t), intent(in) :: arena
                integer, intent(in) :: node_index
            end subroutine callback
        end interface
        logical, intent(in), optional :: pre_order
        
        logical :: do_pre_order
        
        do_pre_order = .true.
        if (present(pre_order)) do_pre_order = pre_order
        
        call traverse_node(arena, root_index, callback, do_pre_order)
    end subroutine traverse_ast
    
    ! Internal recursive traversal
    recursive subroutine traverse_node(arena, node_index, callback, pre_order)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        interface
            subroutine callback(arena, node_index)
                import :: ast_arena_t
                type(ast_arena_t), intent(in) :: arena
                integer, intent(in) :: node_index
            end subroutine callback
        end interface
        logical, intent(in) :: pre_order
        
        integer, allocatable :: child_indices(:)
        integer :: i
        
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        ! Pre-order visit
        if (pre_order) then
            call callback(arena, node_index)
        end if
        
        ! Visit children
        child_indices = get_children(arena, node_index)
        do i = 1, size(child_indices)
            call traverse_node(arena, child_indices(i), callback, pre_order)
        end do
        
        ! Post-order visit
        if (.not. pre_order) then
            call callback(arena, node_index)
        end if
    end subroutine traverse_node
    
    ! Get source range for a node
    function get_node_range(arena, node_index) result(range)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(source_range_t) :: range
        
        integer :: line, column
        
        ! Initialize with default values
        range%start%line = 1
        range%start%column = 1
        range%start%byte_offset = 0
        range%end = range%start
        
        if (node_exists(arena, node_index)) then
            call get_node_location(arena, node_index, line, column)
            range%start%line = line
            range%start%column = column
            ! End position would need to be calculated based on node content
            ! For now, use same as start
            range%end = range%start
        end if
    end function get_node_range
    
    ! Arena statistics wrapper
    function get_arena_stats(arena) result(stats)
        type(ast_arena_t), intent(in) :: arena
        type(ast_arena_stats_t) :: stats
        
        ! Use stats from the arena
        stats = arena%get_stats()
        stats%total_nodes = arena%compat_size
        stats%max_depth = arena%max_depth
    end function get_arena_stats
    
    ! Analyze program with explicit context (for advanced usage)
    subroutine analyze_program(ctx, arena, prog_index)
        use semantic_analyzer, only: analyze_program_impl => analyze_program
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: prog_index
        
        call analyze_program_impl(ctx, arena, prog_index)
    end subroutine analyze_program
    
    ! Get type information for a node
    subroutine get_type_for_node(arena, node_index, node_type, found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(mono_type_t), allocatable, intent(out) :: node_type
        logical, intent(out) :: found
        
        integer :: i
        
        found = .false.
        if (node_exists(arena, node_index)) then
            if (allocated(arena%entries(node_index)%node)) then
                if (arena%entries(node_index)%node%inferred_type%kind > 0) then
                    allocate(node_type)
                    ! Manual deep copy to avoid issues with assignment operator
                    associate (src_type => arena%entries(node_index)%node%inferred_type)
                        node_type%kind = src_type%kind
                        node_type%size = src_type%size
                        node_type%var%id = src_type%var%id
                        node_type%var%name = src_type%var%name  ! Fixed-size character copy
                        ! Unified arena API handles args internally - no manual copying needed
                    end associate
                    found = .true.
                end if
            end if
        end if
    end subroutine get_type_for_node
    
    ! Collect diagnostics (placeholder for future implementation)
    function get_diagnostics(ctx) result(diagnostics)
        type(semantic_context_t), intent(in) :: ctx
        type(diagnostic_t), allocatable :: diagnostics(:)
        
        ! Placeholder - return empty array
        allocate(diagnostics(0))
    end function get_diagnostics
    
    ! AST to JSON serialization
    subroutine ast_to_json(arena, root_index, json_string)
        use ast_json, only: ast_to_json_string
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        character(len=:), allocatable, intent(out) :: json_string
        
        json_string = ast_to_json_string(arena, root_index)
    end subroutine ast_to_json
    
    ! Semantic info to JSON (placeholder)
    subroutine semantic_info_to_json(ctx, json_string)
        type(semantic_context_t), intent(in) :: ctx
        character(len=:), allocatable, intent(out) :: json_string
        
        ! Placeholder implementation
        json_string = '{"semantic_info": "not implemented"}'
    end subroutine semantic_info_to_json
    
    ! Find nodes by type name
    function find_nodes_by_type(arena, type_name) result(indices)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: type_name
        integer, allocatable :: indices(:)
        
        indices = arena%find_by_type(type_name)
    end function find_nodes_by_type
    
    ! Get maximum depth of AST from a given node
    function get_max_depth(arena, node_index) result(max_depth)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer :: max_depth
        
        max_depth = compute_depth(arena, node_index, 1)  ! Start at depth 1 for root
    end function get_max_depth
    
    ! Recursively compute depth
    recursive function compute_depth(arena, node_index, current_depth) result(max_depth)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, intent(in) :: current_depth
        integer :: max_depth
        integer, allocatable :: children(:)
        integer :: i, child_depth
        
        max_depth = current_depth
        
        if (node_index <= 0 .or. node_index > arena%size) return
        
        children = get_children(arena, node_index)
        if (allocated(children)) then
            do i = 1, size(children)
                child_depth = compute_depth(arena, children(i), current_depth + 1)
                if (child_depth > max_depth) max_depth = child_depth
            end do
        end if
    end function compute_depth
    

    ! Get node type as integer constant
    function get_node_type(arena, node_index) result(node_type)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer :: node_type
        class(ast_node), allocatable :: node
        
        node_type = NODE_UNKNOWN
        
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        ! Map node type string to constant (handle both with and without _node suffix)
        select case (trim(arena%entries(node_index)%node_type))
        case ("program_node", "program")
            node_type = NODE_PROGRAM
        case ("function_def_node", "function_def")
            node_type = NODE_FUNCTION_DEF
        case ("assignment_node", "assignment")
            node_type = NODE_ASSIGNMENT
        case ("binary_op_node", "binary_op")
            node_type = NODE_BINARY_OP
        case ("identifier_node", "identifier")
            node_type = NODE_IDENTIFIER
        case ("literal_node", "literal")
            node_type = NODE_LITERAL
        case ("array_literal_node", "array_literal")
            node_type = NODE_ARRAY_LITERAL
        case ("call_or_subscript_node", "call_or_subscript")
            node_type = NODE_CALL_OR_SUBSCRIPT
        case ("subroutine_def_node", "subroutine_def")
            node_type = NODE_SUBROUTINE_DEF
        case ("subroutine_call_node", "subroutine_call")
            node_type = NODE_SUBROUTINE_CALL
        case ("declaration_node", "declaration")
            node_type = NODE_DECLARATION
        case ("parameter_declaration_node", "parameter_declaration")
            node_type = NODE_PARAMETER_DECLARATION
        case ("if_node", "if")
            node_type = NODE_IF
        case ("do_loop_node", "do_loop")
            node_type = NODE_DO_LOOP
        case ("do_while_node", "do_while")
            node_type = NODE_DO_WHILE
        case ("select_case_node", "select_case")
            node_type = NODE_SELECT_CASE
        case ("case_block_node", "case_block")
            node_type = NODE_CASE_BLOCK
        case ("module_node", "module")
            node_type = NODE_MODULE
        case ("use_statement_node", "use_statement")
            node_type = NODE_USE_STATEMENT
        case ("print_statement_node", "print_statement")
            node_type = NODE_PRINT_STATEMENT
        case ("write_statement_node", "write_statement")
            node_type = NODE_WRITE_STATEMENT
        case ("read_statement_node", "read_statement")
            node_type = NODE_READ_STATEMENT
        case ("allocate_statement_node", "allocate_statement")
            node_type = NODE_ALLOCATE_STATEMENT
        case ("deallocate_statement_node", "deallocate_statement")
            node_type = NODE_DEALLOCATE_STATEMENT
        case ("stop_node", "stop")
            node_type = NODE_STOP
        case ("return_node", "return")
            node_type = NODE_RETURN
        case ("goto_node", "goto")
            node_type = NODE_GOTO
        case ("error_stop_node", "error_stop")
            node_type = NODE_ERROR_STOP
        case ("cycle_node", "cycle")
            node_type = NODE_CYCLE
        case ("exit_node", "exit")
            node_type = NODE_EXIT
        case ("where_node", "where")
            node_type = NODE_WHERE
        case ("interface_block_node", "interface_block")
            node_type = NODE_INTERFACE_BLOCK
        case ("derived_type_node", "derived_type")
            node_type = NODE_DERIVED_TYPE
        case ("pointer_assignment_node", "pointer_assignment")
            node_type = NODE_POINTER_ASSIGNMENT
        case ("forall_node", "forall")
            node_type = NODE_FORALL
        case ("case_range_node", "case_range")
            node_type = NODE_CASE_RANGE
        case ("case_default_node", "case_default")
            node_type = NODE_CASE_DEFAULT
        case ("complex_literal_node", "complex_literal")
            node_type = NODE_COMPLEX_LITERAL
        case ("include_statement_node", "include_statement")
            node_type = NODE_INCLUDE_STATEMENT
        case ("contains_node", "contains")
            node_type = NODE_CONTAINS
        case ("format_descriptor_node", "format_descriptor")
            node_type = NODE_FORMAT_DESCRIPTOR
        case ("comment_node", "comment")
            node_type = NODE_COMMENT
        case ("implicit_statement_node", "implicit_statement")
            node_type = NODE_IMPLICIT_STATEMENT
        end select
    end function get_node_type
    
    ! Get typed node access functions
    function get_node_as_program(arena, index) result(node)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        type(program_node), pointer :: node
        
        nullify(node)
        if (index > 0 .and. index <= arena%size) then
            if (allocated(arena%entries(index)%node)) then
                select type (p => arena%entries(index)%node)
                type is (program_node)
                    node => p
                end select
            end if
        end if
    end function get_node_as_program
    
    function get_node_as_assignment(arena, index) result(node)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        type(assignment_node), pointer :: node
        
        nullify(node)
        if (index > 0 .and. index <= arena%size) then
            if (allocated(arena%entries(index)%node)) then
                select type (p => arena%entries(index)%node)
                type is (assignment_node)
                    node => p
                end select
            end if
        end if
    end function get_node_as_assignment
    
    function get_node_as_function_def(arena, index) result(node)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        type(function_def_node), pointer :: node
        
        nullify(node)
        if (index > 0 .and. index <= arena%size) then
            if (allocated(arena%entries(index)%node)) then
                select type (p => arena%entries(index)%node)
                type is (function_def_node)
                    node => p
                end select
            end if
        end if
    end function get_node_as_function_def

end module fortfront_utils