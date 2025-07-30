module fortfront
    ! fortfront Public API - Facade module exposing all functionality for fluff
    ! This module provides a unified interface to all fortfront phases:
    ! - Lexical Analysis
    ! - AST Construction and Arena Management
    ! - Semantic Analysis with Type Inference
    ! - Code Generation
    
    ! Re-export core pipeline functionality
    use frontend, only: lex_source, parse_tokens, analyze_semantics, emit_fortran, &
                       transform_lazy_fortran_string, compilation_options_t
    
    ! Re-export AST arena and core types
    use ast_core, only: ast_arena_t, ast_node, program_node, assignment_node, &
                        binary_op_node, function_def_node, identifier_node, &
                        literal_node, array_literal_node, call_or_subscript_node, &
                        subroutine_def_node, subroutine_call_node, declaration_node, &
                        parameter_declaration_node, if_node, do_loop_node, &
                        do_while_node, select_case_node, case_block_node, &
                        module_node, use_statement_node, include_statement_node, &
                        print_statement_node, write_statement_node, read_statement_node, &
                        allocate_statement_node, deallocate_statement_node, &
                        stop_node, return_node, cycle_node, exit_node, &
                        where_node, interface_block_node, derived_type_node, &
                        pointer_assignment_node, forall_node, case_range_node, &
                        case_default_node, complex_literal_node, &
                        LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING, &
                        LITERAL_LOGICAL, LITERAL_ARRAY, LITERAL_COMPLEX, &
                        create_ast_stack, ast_arena_stats_t
    
    ! Re-export semantic analyzer functionality
    use semantic_analyzer, only: semantic_context_t, create_semantic_context
    
    ! Re-export lexer token type
    use lexer_core, only: token_t, tokenize_core
    
    ! Re-export type system
    use type_system_hm, only: mono_type_t, TINT, TREAL, TCHAR, TLOGICAL, &
                             TFUN, TARRAY, TVAR
    
    implicit none
    public
    
    ! Additional facade-specific types and procedures
    
    ! Source location tracking
    type :: source_location_t
        integer :: line = 1
        integer :: column = 1
        integer :: byte_offset = 0  ! For efficient text manipulation
    end type source_location_t
    
    type :: source_range_t
        type(source_location_t) :: start
        type(source_location_t) :: end
    end type source_range_t
    
    ! Diagnostic information
    integer, parameter :: DIAGNOSTIC_ERROR = 1
    integer, parameter :: DIAGNOSTIC_WARNING = 2
    integer, parameter :: DIAGNOSTIC_INFO = 3
    integer, parameter :: DIAGNOSTIC_HINT = 4
    
    type :: diagnostic_t
        integer :: severity = DIAGNOSTIC_INFO
        character(len=:), allocatable :: message
        type(source_range_t) :: location
        character(len=:), allocatable :: code      ! Error code (e.g., "F001")
        character(len=:), allocatable :: category  ! Error category
    end type diagnostic_t
    
contains
    
    ! Create AST arena (wrapper for consistency)
    function create_ast_arena() result(arena)
        type(ast_arena_t) :: arena
        arena = create_ast_stack()
    end function create_ast_arena
    
    ! Get node from arena by index
    function get_node(arena, node_index) result(node)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        class(ast_node), allocatable :: node
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                allocate(node, source=arena%entries(node_index)%node)
            end if
        end if
    end function get_node
    
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
                child_indices = arena%entries(node_index)%child_indices(1:arena%entries(node_index)%child_count)
            end if
        end if
    end function get_children
    
    ! Traverse AST with visitor pattern
    subroutine traverse_ast(arena, root_index, visitor, pre_order)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        class(*), intent(inout) :: visitor
        logical, intent(in), optional :: pre_order
        
        logical :: do_pre_order
        
        do_pre_order = .true.
        if (present(pre_order)) do_pre_order = pre_order
        
        call traverse_node(arena, root_index, visitor, do_pre_order)
    end subroutine traverse_ast
    
    ! Internal recursive traversal
    recursive subroutine traverse_node(arena, node_index, visitor, pre_order)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        class(*), intent(inout) :: visitor
        logical, intent(in) :: pre_order
        
        integer, allocatable :: child_indices(:)
        integer :: i
        
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        ! Pre-order visit
        if (pre_order) then
            call arena%entries(node_index)%node%accept(visitor)
        end if
        
        ! Visit children
        child_indices = get_children(arena, node_index)
        do i = 1, size(child_indices)
            call traverse_node(arena, child_indices(i), visitor, pre_order)
        end do
        
        ! Post-order visit
        if (.not. pre_order) then
            call arena%entries(node_index)%node%accept(visitor)
        end if
    end subroutine traverse_node
    
    ! Get source range for a node
    function get_node_range(arena, node_index) result(range)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(source_range_t) :: range
        
        class(ast_node), allocatable :: node
        
        ! Initialize with default values
        range%start%line = 1
        range%start%column = 1
        range%start%byte_offset = 0
        range%end = range%start
        
        node = get_node(arena, node_index)
        if (allocated(node)) then
            range%start%line = node%line
            range%start%column = node%column
            ! End position would need to be calculated based on node content
            ! For now, use same as start
            range%end = range%start
        end if
    end function get_node_range
    
    ! Arena statistics wrapper
    function get_arena_stats(arena) result(stats)
        type(ast_arena_t), intent(in) :: arena
        type(ast_arena_stats_t) :: stats
        
        stats = arena%get_stats()
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
    function get_type_for_node(arena, node_index) result(node_type)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(mono_type_t), allocatable :: node_type
        
        class(ast_node), allocatable :: node
        
        node = get_node(arena, node_index)
        if (allocated(node)) then
            if (allocated(node%inferred_type)) then
                allocate(node_type, source=node%inferred_type)
            end if
        end if
    end function get_type_for_node
    
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
    
end module fortfront