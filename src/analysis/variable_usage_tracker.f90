module variable_usage_tracker_module
    use iso_fortran_env, only: error_unit
    use ast_core
    use ast_arena
    use ast_nodes_core, only: binary_op_node, call_or_subscript_node, &
                              identifier_node, component_access_node
    use ast_nodes_bounds, only: array_slice_node
    implicit none
    private

    ! Public types
    public :: variable_usage_info_t, expression_visitor_t
    
    ! Public procedures  
    public :: create_variable_usage_info, get_variables_in_expression
    public :: get_identifiers_in_subtree, visit_expression_nodes
    public :: is_variable_used_in_expression, count_variable_usage

    ! Variable usage information
    type :: variable_usage_info_t
        character(len=:), allocatable :: variable_names(:)
        integer, allocatable :: usage_counts(:)
        integer, allocatable :: node_indices(:)
        integer :: total_count = 0
    contains
        procedure :: assign_usage_info
        generic :: assignment(=) => assign_usage_info
    end type variable_usage_info_t

    ! Expression visitor interface
    abstract interface
        subroutine expression_visitor_interface(arena, node_index, node_type, &
                                               user_data)
            import :: ast_arena_t
            type(ast_arena_t), intent(in) :: arena
            integer, intent(in) :: node_index
            character(len=*), intent(in) :: node_type
            class(*), intent(inout), optional :: user_data
        end subroutine expression_visitor_interface
    end interface

    type :: expression_visitor_t
        procedure(expression_visitor_interface), pointer, nopass :: visit => null()
    end type expression_visitor_t

contains

    ! Create empty variable usage info
    function create_variable_usage_info() result(info)
        type(variable_usage_info_t) :: info
        
        ! Leave arrays unallocated initially
        ! They will be allocated when first variable is added
        info%total_count = 0
    end function create_variable_usage_info

    ! Get all variables used in an expression
    function get_variables_in_expression(arena, expr_index) result(info)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index
        type(variable_usage_info_t) :: info
        
        info = create_variable_usage_info()
        
        if (expr_index <= 0 .or. expr_index > arena%size) return
        if (.not. allocated(arena%entries(expr_index)%node)) return
        
        call collect_identifiers_recursive(arena, expr_index, info)
    end function get_variables_in_expression

    ! Recursively collect all identifier nodes from expression subtree
    recursive subroutine collect_identifiers_recursive(arena, node_index, info)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        character(len=:), allocatable :: node_type
        
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        node_type = arena%entries(node_index)%node_type
        
        ! If this is an identifier, add it to the list
        if (node_type == "identifier") then
            call add_identifier_to_info(arena, node_index, info)
        end if
        
        ! Traverse all child nodes based on node type
        select case (node_type)
        case ("binary_op")
            call process_binary_op_children(arena, node_index, info)
        case ("call_or_subscript")
            call process_call_or_subscript_children(arena, node_index, info)
        case ("array_slice")
            call process_array_slice_children(arena, node_index, info)
        case ("component_access")
            call process_component_access_children(arena, node_index, info)
        case ("if", "if_statement")
            call process_if_node_children(arena, node_index, info)
        case ("do_while")
            call process_do_while_node_children(arena, node_index, info)
        case ("select_case")
            call process_select_case_node_children(arena, node_index, info)
        case ("where")
            call process_where_node_children(arena, node_index, info)
        case ("where_stmt")
            call process_where_stmt_node_children(arena, node_index, info)
        case ("program")
            call process_program_node_children(arena, node_index, info)
        case ("literal")
            call process_literal_node_children(arena, node_index, info)
        case ("multi_declaration")
            call process_multi_declaration_node_children(arena, node_index, info)
        case ("print_statement")
            call process_print_statement_node_children(arena, node_index, info)
        case ("case_block")
            call process_case_block_node_children(arena, node_index, info)
        case ("do_loop")
            call process_do_loop_node_children(arena, node_index, info)
        case ("assignment")
            call process_assignment_node_children(arena, node_index, info)
        case ("subroutine_call", "call_statement")
            call process_subroutine_call_children(arena, node_index, info)
        case ("write_statement")
            call process_write_statement_children(arena, node_index, info)
        case ("read_statement")
            call process_read_statement_children(arena, node_index, info)
        case ("allocate_statement")
            call process_allocate_statement_children(arena, node_index, info)
        case ("deallocate_statement")
            call process_deallocate_statement_children(arena, node_index, info)
        case ("associate")
            call process_associate_construct_children(arena, node_index, info)
        case ("subroutine_def")
            call process_subroutine_def_children(arena, node_index, info)
        case ("function_def")
            call process_function_def_children(arena, node_index, info)
        end select
    end subroutine collect_identifiers_recursive

    ! Add an identifier to the usage info
    subroutine add_identifier_to_info(arena, node_index, info)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        character(len=:), allocatable :: var_name
        
        ! Get variable name from identifier node
        select type (node => arena%entries(node_index)%node)
        type is (identifier_node)
            var_name = node%name
        class default
            return
        end select
        
        ! Use common helper
        call add_variable_to_info_common(var_name, node_index, info)
    end subroutine add_identifier_to_info

    ! Common helper to add a variable name to usage info
    subroutine add_variable_to_info_common(var_name, node_index, info)
        character(len=*), intent(in) :: var_name
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        integer :: i, existing_index
        
        ! Check if variable already exists in the list
        existing_index = 0
        if (allocated(info%variable_names)) then
            do i = 1, size(info%variable_names)
                if (info%variable_names(i) == var_name) then
                    existing_index = i
                    exit
                end if
            end do
        end if
        
        if (existing_index > 0) then
            ! Increment existing count
            info%usage_counts(existing_index) = info%usage_counts(existing_index) + 1
        else
            ! Add new variable
            if (.not. allocated(info%variable_names)) then
                ! First variable - allocate new arrays
                allocate(character(len=len(var_name)) :: info%variable_names(1))
                info%variable_names(1) = var_name
                allocate(info%usage_counts(1))
                info%usage_counts(1) = 1
                allocate(info%node_indices(1))
                info%node_indices(1) = node_index
            else
                ! Extend existing arrays
                block
                    character(len=:), allocatable :: temp_names(:)
                    integer, allocatable :: temp_counts(:), temp_indices(:)
                    integer :: n
                    
                    n = size(info%variable_names)
                    allocate(character(len=max(len(info%variable_names), len(var_name))) :: temp_names(n+1))
                    allocate(temp_counts(n+1))
                    allocate(temp_indices(n+1))
                    
                    temp_names(1:n) = info%variable_names
                    temp_names(n+1) = var_name
                    temp_counts(1:n) = info%usage_counts
                    temp_counts(n+1) = 1
                    temp_indices(1:n) = info%node_indices
                    temp_indices(n+1) = node_index
                    
                    call move_alloc(temp_names, info%variable_names)
                    call move_alloc(temp_counts, info%usage_counts)
                    call move_alloc(temp_indices, info%node_indices)
                end block
            end if
        end if
        
        info%total_count = info%total_count + 1
    end subroutine add_variable_to_info_common

    ! Add a string-based variable name to the usage info
    subroutine add_string_to_info(var_name, node_index, info)
        character(len=*), intent(in) :: var_name
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        ! Use common helper
        call add_variable_to_info_common(var_name, node_index, info)
    end subroutine add_string_to_info

    ! Process binary operation children
    subroutine process_binary_op_children(arena, node_index, info)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        select type (node => arena%entries(node_index)%node)
        type is (binary_op_node)
            if (node%left_index > 0) then
                call collect_identifiers_recursive(arena, node%left_index, info)
            end if
            if (node%right_index > 0) then
                call collect_identifiers_recursive(arena, node%right_index, info)
            end if
        end select
    end subroutine process_binary_op_children


    ! Process call or subscript children
    subroutine process_call_or_subscript_children(arena, node_index, info)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        integer :: i
        
        select type (node => arena%entries(node_index)%node)
        type is (call_or_subscript_node)
            ! The function/array name is stored as a string, not an index
            ! We need to add it manually to the info if it's a variable reference
            if (allocated(node%name)) then
                call add_string_to_info(node%name, node_index, info)
            end if
            
            ! Process all arguments/subscripts
            if (allocated(node%arg_indices)) then
                do i = 1, size(node%arg_indices)
                    if (node%arg_indices(i) > 0) then
                        call collect_identifiers_recursive(arena, node%arg_indices(i), info)
                    end if
                end do
            end if
        end select
    end subroutine process_call_or_subscript_children

    ! Process array slice children
    subroutine process_array_slice_children(arena, node_index, info)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        select type (node => arena%entries(node_index)%node)
        type is (array_slice_node)
            ! Process array name
            if (node%array_index > 0) then
                call collect_identifiers_recursive(arena, node%array_index, info)
            end if
            
            ! Process slice bounds
            block
                integer :: i
                do i = 1, node%num_dimensions
                    if (node%bounds_indices(i) > 0) then
                        call collect_identifiers_recursive(arena, node%bounds_indices(i), info)
                    end if
                end do
            end block
        end select
    end subroutine process_array_slice_children

    ! Process component access children
    subroutine process_component_access_children(arena, node_index, info)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        select type (node => arena%entries(node_index)%node)
        type is (component_access_node)
            ! Process the base expression (structure/derived type)
            if (node%base_expr_index > 0) then
                call collect_identifiers_recursive(arena, node%base_expr_index, info)
            end if
            
            ! Component name is stored as a string
            if (allocated(node%component_name)) then
                call add_string_to_info(node%component_name, node_index, info)
            end if
        end select
    end subroutine process_component_access_children

    ! Process if node children
    subroutine process_if_node_children(arena, node_index, info)
        use ast_nodes_control, only: if_node, elseif_wrapper
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        integer :: i, j
        
        ! Validate node index bounds
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        select type (node => arena%entries(node_index)%node)
        type is (if_node)
            ! Process condition expression
            if (node%condition_index > 0) then
                call collect_identifiers_recursive(arena, node%condition_index, info)
            end if
            
            ! Process then body statements
            if (allocated(node%then_body_indices)) then
                do i = 1, size(node%then_body_indices)
                    if (node%then_body_indices(i) > 0) then
                        call collect_identifiers_recursive(arena, node%then_body_indices(i), info)
                    end if
                end do
            end if
            
            ! Process elseif blocks
            if (allocated(node%elseif_blocks)) then
                do i = 1, size(node%elseif_blocks)
                    ! Process elseif condition
                    if (node%elseif_blocks(i)%condition_index > 0) then
                        call collect_identifiers_recursive(arena, node%elseif_blocks(i)%condition_index, info)
                    end if
                    
                    ! Process elseif body
                    if (allocated(node%elseif_blocks(i)%body_indices)) then
                        do j = 1, size(node%elseif_blocks(i)%body_indices)
                            if (node%elseif_blocks(i)%body_indices(j) > 0) then
                                call collect_identifiers_recursive(arena, node%elseif_blocks(i)%body_indices(j), info)
                            end if
                        end do
                    end if
                end do
            end if
            
            ! Process else body statements
            if (allocated(node%else_body_indices)) then
                do i = 1, size(node%else_body_indices)
                    if (node%else_body_indices(i) > 0) then
                        call collect_identifiers_recursive(arena, node%else_body_indices(i), info)
                    end if
                end do
            end if
        end select
    end subroutine process_if_node_children

    ! Process do while node children
    subroutine process_do_while_node_children(arena, node_index, info)
        use ast_nodes_control, only: do_while_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        integer :: i
        
        ! Validate node index bounds
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        select type (node => arena%entries(node_index)%node)
        type is (do_while_node)
            ! Process condition expression
            if (node%condition_index > 0) then
                call collect_identifiers_recursive(arena, node%condition_index, info)
            end if
            
            ! Process body statements
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    if (node%body_indices(i) > 0) then
                        call collect_identifiers_recursive(arena, node%body_indices(i), info)
                    end if
                end do
            end if
        end select
    end subroutine process_do_while_node_children

    ! Process select case node children
    subroutine process_select_case_node_children(arena, node_index, info)
        use ast_nodes_control, only: select_case_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        integer :: i
        
        select type (node => arena%entries(node_index)%node)
        type is (select_case_node)
            ! Process selector expression
            if (node%selector_index > 0) then
                call collect_identifiers_recursive(arena, node%selector_index, info)
            end if
            
            ! Process case blocks
            if (allocated(node%case_indices)) then
                do i = 1, size(node%case_indices)
                    if (node%case_indices(i) > 0) then
                        call collect_identifiers_recursive(arena, node%case_indices(i), info)
                    end if
                end do
            end if
            
            ! Process default case
            if (node%default_index > 0) then
                call collect_identifiers_recursive(arena, node%default_index, info)
            end if
        end select
    end subroutine process_select_case_node_children

    ! Process where node children
    subroutine process_where_node_children(arena, node_index, info)
        use ast_nodes_control, only: where_node, elsewhere_clause_t
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        integer :: i, j
        
        select type (node => arena%entries(node_index)%node)
        type is (where_node)
            ! Process mask expression
            if (node%mask_expr_index > 0) then
                call collect_identifiers_recursive(arena, node%mask_expr_index, info)
            end if
            
            ! Process where body statements
            if (allocated(node%where_body_indices)) then
                do i = 1, size(node%where_body_indices)
                    if (node%where_body_indices(i) > 0) then
                        call collect_identifiers_recursive(arena, node%where_body_indices(i), info)
                    end if
                end do
            end if
            
            ! Process elsewhere clauses
            if (allocated(node%elsewhere_clauses)) then
                do i = 1, size(node%elsewhere_clauses)
                    ! Process elsewhere mask if present
                    if (node%elsewhere_clauses(i)%mask_index > 0) then
                        call collect_identifiers_recursive(arena, node%elsewhere_clauses(i)%mask_index, info)
                    end if
                    
                    ! Process elsewhere body
                    if (allocated(node%elsewhere_clauses(i)%body_indices)) then
                        do j = 1, size(node%elsewhere_clauses(i)%body_indices)
                            if (node%elsewhere_clauses(i)%body_indices(j) > 0) then
                                call collect_identifiers_recursive(arena, node%elsewhere_clauses(i)%body_indices(j), info)
                            end if
                        end do
                    end if
                end do
            end if
        end select
    end subroutine process_where_node_children

    ! Process where statement node children  
    subroutine process_where_stmt_node_children(arena, node_index, info)
        use ast_nodes_control, only: where_stmt_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        select type (node => arena%entries(node_index)%node)
        type is (where_stmt_node)
            ! Process mask expression
            if (node%mask_expr_index > 0) then
                call collect_identifiers_recursive(arena, node%mask_expr_index, info)
            end if
            
            ! Process assignment
            if (node%assignment_index > 0) then
                call collect_identifiers_recursive(arena, node%assignment_index, info)
            end if
        end select
    end subroutine process_where_stmt_node_children

    ! Process program node children
    subroutine process_program_node_children(arena, node_index, info)
        use ast_nodes_core, only: program_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        integer :: i
        
        ! Validate node index bounds
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        select type (node => arena%entries(node_index)%node)
        type is (program_node)
            ! Process all body statements
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    if (node%body_indices(i) > 0) then
                        call collect_identifiers_recursive(arena, node%body_indices(i), info)
                    end if
                end do
            end if
        end select
    end subroutine process_program_node_children

    ! Process literal node children (might contain parsed expressions)
    subroutine process_literal_node_children(arena, node_index, info)
        use ast_nodes_core, only: literal_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        ! Literals typically don't have child nodes with expressions
        ! But we should check if this literal represents a parsed statement
        ! that might contain identifiers (like an if statement)
        
        ! For now, we don't traverse literal nodes as they usually contain
        ! constant values, not variable references
    end subroutine process_literal_node_children

    ! Process multi declaration node children  
    subroutine process_multi_declaration_node_children(arena, node_index, info)
        use ast_nodes_data, only: declaration_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        integer :: i
        
        ! Validate node index bounds
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        select type (node => arena%entries(node_index)%node)
        type is (declaration_node)
            ! Only process if this is actually a multi-declaration
            if (node%is_multi_declaration) then
                ! Process initialization expression if present
                if (node%has_initializer .and. node%initializer_index > 0) then
                    call collect_identifiers_recursive(arena, node%initializer_index, info)
                end if
            end if
        end select
    end subroutine process_multi_declaration_node_children

    ! Process print statement node children
    subroutine process_print_statement_node_children(arena, node_index, info)
        use ast_nodes_io, only: print_statement_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        integer :: i
        
        ! Validate node index bounds
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        select type (node => arena%entries(node_index)%node)
        type is (print_statement_node)
            ! Process all expression arguments
            if (allocated(node%expression_indices)) then
                do i = 1, size(node%expression_indices)
                    if (node%expression_indices(i) > 0) then
                        call collect_identifiers_recursive(arena, node%expression_indices(i), info)
                    end if
                end do
            end if
        end select
    end subroutine process_print_statement_node_children

    ! Process case block node children
    subroutine process_case_block_node_children(arena, node_index, info)
        use ast_nodes_control, only: case_block_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        integer :: i
        
        ! Validate node index bounds
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        select type (node => arena%entries(node_index)%node)
        type is (case_block_node)
            ! Process case value expressions
            if (allocated(node%value_indices)) then
                do i = 1, size(node%value_indices)
                    if (node%value_indices(i) > 0) then
                        call collect_identifiers_recursive(arena, node%value_indices(i), info)
                    end if
                end do
            end if
            
            ! Process case body statements
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    if (node%body_indices(i) > 0) then
                        call collect_identifiers_recursive(arena, node%body_indices(i), info)
                    end if
                end do
            end if
        end select
    end subroutine process_case_block_node_children

    ! Process do loop node children
    subroutine process_do_loop_node_children(arena, node_index, info)
        use ast_nodes_control, only: do_loop_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        integer :: i
        
        ! Validate node index bounds
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        select type (node => arena%entries(node_index)%node)
        type is (do_loop_node)
            ! Process loop variable (stored as string, not index)
            if (allocated(node%var_name)) then
                call add_string_to_info(node%var_name, node_index, info)
            end if
            
            ! Process start expression
            if (node%start_expr_index > 0) then
                call collect_identifiers_recursive(arena, node%start_expr_index, info)
            end if
            
            ! Process end expression
            if (node%end_expr_index > 0) then
                call collect_identifiers_recursive(arena, node%end_expr_index, info)
            end if
            
            ! Process step expression
            if (node%step_expr_index > 0) then
                call collect_identifiers_recursive(arena, node%step_expr_index, info)
            end if
            
            ! Process loop body statements
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    if (node%body_indices(i) > 0) then
                        call collect_identifiers_recursive(arena, node%body_indices(i), info)
                    end if
                end do
            end if
        end select
    end subroutine process_do_loop_node_children

    ! Process assignment node children
    subroutine process_assignment_node_children(arena, node_index, info)
        use ast_nodes_core, only: assignment_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        ! Validate node index bounds
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        ! Verify node type matches expectation
        if (arena%entries(node_index)%node_type /= "assignment") then
            ! Node type mismatch - this shouldn't happen if called correctly
            return
        end if
        
        select type (node => arena%entries(node_index)%node)
        type is (assignment_node)
            ! Process target (LHS) - might have array subscripts
            if (node%target_index > 0) then
                call collect_identifiers_recursive(arena, node%target_index, info)
            end if
            
            ! Process value (RHS)
            if (node%value_index > 0) then
                call collect_identifiers_recursive(arena, node%value_index, info)
            end if
        end select
    end subroutine process_assignment_node_children

    ! Process subroutine call children
    subroutine process_subroutine_call_children(arena, node_index, info)
        use ast_nodes_procedure, only: subroutine_call_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        integer :: i
        
        ! Validate node index bounds
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        ! Verify node type matches expectation
        if (arena%entries(node_index)%node_type /= "subroutine_call" .and. &
            arena%entries(node_index)%node_type /= "call_statement") then
            ! Node type mismatch - this shouldn't happen if called correctly
            return
        end if
        
        select type (node => arena%entries(node_index)%node)
        type is (subroutine_call_node)
            ! Subroutine name is stored as string, not index
            if (allocated(node%name)) then
                call add_string_to_info(node%name, node_index, info)
            end if
            
            ! Process all arguments
            if (allocated(node%arg_indices)) then
                do i = 1, size(node%arg_indices)
                    if (node%arg_indices(i) > 0) then
                        call collect_identifiers_recursive(arena, node%arg_indices(i), info)
                    end if
                end do
            end if
        end select
    end subroutine process_subroutine_call_children

    ! Process write statement children
    subroutine process_write_statement_children(arena, node_index, info)
        use ast_nodes_io, only: write_statement_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        integer :: i
        
        ! Validate node index bounds
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        ! Verify node type matches expectation
        if (arena%entries(node_index)%node_type /= "write_statement") then
            ! Node type mismatch - this shouldn't happen if called correctly
            return
        end if
        
        select type (node => arena%entries(node_index)%node)
        type is (write_statement_node)
            ! Process runtime format expression if present
            if (node%format_expr_index > 0) then
                call collect_identifiers_recursive(arena, node%format_expr_index, info)
            end if
            
            ! Process iostat variable if present
            if (node%iostat_var_index > 0) then
                call collect_identifiers_recursive(arena, node%iostat_var_index, info)
            end if
            
            ! Process all output arguments
            if (allocated(node%arg_indices)) then
                do i = 1, size(node%arg_indices)
                    if (node%arg_indices(i) > 0) then
                        call collect_identifiers_recursive(arena, node%arg_indices(i), info)
                    end if
                end do
            end if
        end select
    end subroutine process_write_statement_children

    ! Process read statement children
    subroutine process_read_statement_children(arena, node_index, info)
        use ast_nodes_io, only: read_statement_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        integer :: i
        
        ! Validate node index bounds
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        ! Verify node type matches expectation
        if (arena%entries(node_index)%node_type /= "read_statement") then
            ! Node type mismatch - this shouldn't happen if called correctly
            return
        end if
        
        select type (node => arena%entries(node_index)%node)
        type is (read_statement_node)
            ! Process runtime format expression if present
            if (node%format_expr_index > 0) then
                call collect_identifiers_recursive(arena, node%format_expr_index, info)
            end if
            
            ! Process iostat variable if present
            if (node%iostat_var_index > 0) then
                call collect_identifiers_recursive(arena, node%iostat_var_index, info)
            end if
            
            ! Process all variables to read into
            if (allocated(node%var_indices)) then
                do i = 1, size(node%var_indices)
                    if (node%var_indices(i) > 0) then
                        call collect_identifiers_recursive(arena, node%var_indices(i), info)
                    end if
                end do
            end if
        end select
    end subroutine process_read_statement_children

    ! Process allocate statement children
    subroutine process_allocate_statement_children(arena, node_index, info)
        use ast_nodes_misc, only: allocate_statement_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        integer :: i
        
        ! Validate node index bounds
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries)) return
        if (node_index > size(arena%entries)) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        ! Verify node type matches expectation
        if (arena%entries(node_index)%node_type /= "allocate_statement") then
            ! Node type mismatch - this shouldn't happen if called correctly
            return
        end if
        
        select type (node => arena%entries(node_index)%node)
        type is (allocate_statement_node)
            ! Process variables being allocated
            if (allocated(node%var_indices)) then
                do i = 1, size(node%var_indices)
                    if (node%var_indices(i) > 0) then
                        call collect_identifiers_recursive(arena, node%var_indices(i), info)
                    end if
                end do
            end if
            
            ! Process shape expressions for each variable
            if (allocated(node%shape_indices)) then
                do i = 1, size(node%shape_indices)
                    if (node%shape_indices(i) > 0) then
                        call collect_identifiers_recursive(arena, node%shape_indices(i), info)
                    end if
                end do
            end if
            
            ! Process stat variable if present
            if (node%stat_var_index > 0) then
                call collect_identifiers_recursive(arena, node%stat_var_index, info)
            end if
            
            ! Process errmsg variable if present
            if (node%errmsg_var_index > 0) then
                call collect_identifiers_recursive(arena, node%errmsg_var_index, info)
            end if
            
            ! Process source expression if present
            if (node%source_expr_index > 0) then
                call collect_identifiers_recursive(arena, node%source_expr_index, info)
            end if
            
            ! Process mold expression if present
            if (node%mold_expr_index > 0) then
                call collect_identifiers_recursive(arena, node%mold_expr_index, info)
            end if
        end select
    end subroutine process_allocate_statement_children

    ! Process deallocate statement children
    subroutine process_deallocate_statement_children(arena, node_index, info)
        use ast_nodes_misc, only: deallocate_statement_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        integer :: i
        
        ! Validate node index bounds
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries)) return
        if (node_index > size(arena%entries)) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        ! Verify node type matches expectation
        if (arena%entries(node_index)%node_type /= "deallocate_statement") then
            ! Node type mismatch - this shouldn't happen if called correctly
            return
        end if
        
        select type (node => arena%entries(node_index)%node)
        type is (deallocate_statement_node)
            ! Process variables being deallocated
            if (allocated(node%var_indices)) then
                do i = 1, size(node%var_indices)
                    if (node%var_indices(i) > 0) then
                        call collect_identifiers_recursive(arena, node%var_indices(i), info)
                    end if
                end do
            end if
            
            ! Process stat variable if present
            if (node%stat_var_index > 0) then
                call collect_identifiers_recursive(arena, node%stat_var_index, info)
            end if
            
            ! Process errmsg variable if present
            if (node%errmsg_var_index > 0) then
                call collect_identifiers_recursive(arena, node%errmsg_var_index, info)
            end if
        end select
    end subroutine process_deallocate_statement_children

    ! Process associate construct children
    subroutine process_associate_construct_children(arena, node_index, info)
        use ast_nodes_control, only: associate_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        integer :: i
        
        ! Validate node index bounds
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries)) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        select type (node => arena%entries(node_index)%node)
        type is (associate_node)
            ! Process all associations - track variables used in target expressions
            if (allocated(node%associations)) then
                do i = 1, size(node%associations)
                    ! Process the target expression - this tracks the original variable
                    ! The alias name itself is NOT a variable usage, it's a new binding
                    if (node%associations(i)%expr_index > 0) then
                        call collect_identifiers_recursive(arena, node%associations(i)%expr_index, info)
                    end if
                end do
            end if
            
            ! Process the body statements
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    if (node%body_indices(i) > 0) then
                        call collect_identifiers_recursive(arena, node%body_indices(i), info)
                    end if
                end do
            end if
        end select
    end subroutine process_associate_construct_children

    ! Process procedure definition children (shared by subroutine and function)
    subroutine process_procedure_def_body(arena, node_index, info, body_indices)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        integer, intent(in), optional :: body_indices(:)
        
        integer :: i
        
        ! Process all body statements - this will capture all identifiers used
        ! within the procedure, including dummy arguments when they're used
        if (present(body_indices)) then
            do i = 1, size(body_indices)
                if (body_indices(i) > 0) then
                    call collect_identifiers_recursive(arena, body_indices(i), info)
                end if
            end do
        end if
    end subroutine process_procedure_def_body

    ! Process subroutine definition children
    subroutine process_subroutine_def_children(arena, node_index, info)
        use ast_nodes_procedure, only: subroutine_def_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        ! Validate node index bounds
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries)) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        select type (node => arena%entries(node_index)%node)
        type is (subroutine_def_node)
            if (allocated(node%body_indices)) then
                call process_procedure_def_body(arena, node_index, info, node%body_indices)
            end if
        end select
    end subroutine process_subroutine_def_children

    ! Process function definition children
    subroutine process_function_def_children(arena, node_index, info)
        use ast_nodes_procedure, only: function_def_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        ! Validate node index bounds
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries)) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        select type (node => arena%entries(node_index)%node)
        type is (function_def_node)
            if (allocated(node%body_indices)) then
                call process_procedure_def_body(arena, node_index, info, node%body_indices)
            end if
        end select
    end subroutine process_function_def_children

    ! Get list of all identifiers in a subtree (convenience function)
    function get_identifiers_in_subtree(arena, root_index) result(identifiers)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        character(len=:), allocatable :: identifiers(:)
        
        type(variable_usage_info_t) :: info
        
        info = get_variables_in_expression(arena, root_index)
        
        if (allocated(info%variable_names) .and. size(info%variable_names) > 0) then
            identifiers = info%variable_names
        else
            allocate(character(len=0) :: identifiers(0))
        end if
    end function get_identifiers_in_subtree

    ! Visit all expression nodes with a visitor function
    subroutine visit_expression_nodes(arena, root_index, visitor, user_data)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        type(expression_visitor_t), intent(in) :: visitor
        class(*), intent(inout), optional :: user_data
        
        if (.not. associated(visitor%visit)) return
        
        call visit_nodes_recursive(arena, root_index, visitor, user_data)
    end subroutine visit_expression_nodes

    ! Recursively visit nodes
    recursive subroutine visit_nodes_recursive(arena, node_index, visitor, user_data)
        use ast_nodes_control, only: associate_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(expression_visitor_t), intent(in) :: visitor
        class(*), intent(inout), optional :: user_data
        
        character(len=:), allocatable :: node_type
        integer :: i
        
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        node_type = arena%entries(node_index)%node_type
        
        ! Visit this node
        call visitor%visit(arena, node_index, node_type, user_data)
        
        ! Recursively visit children (same logic as collect_identifiers_recursive)
        select case (node_type)
        case ("binary_op")
            select type (node => arena%entries(node_index)%node)
            type is (binary_op_node)
                if (node%left_index > 0) then
                    call visit_nodes_recursive(arena, node%left_index, visitor, user_data)
                end if
                if (node%right_index > 0) then
                    call visit_nodes_recursive(arena, node%right_index, visitor, user_data)
                end if
            end select
        case ("call_or_subscript")
            select type (node => arena%entries(node_index)%node)
            type is (call_or_subscript_node)
                ! The function/array name is stored as a string, not an index
                ! so we only visit the arguments/subscripts
                if (allocated(node%arg_indices)) then
                    do i = 1, size(node%arg_indices)
                        if (node%arg_indices(i) > 0) then
                            call visit_nodes_recursive(arena, node%arg_indices(i), visitor, user_data)
                        end if
                    end do
                end if
            end select
        case ("array_slice")
            select type (node => arena%entries(node_index)%node)
            type is (array_slice_node)
                ! Process array name
                if (node%array_index > 0) then
                    call visit_nodes_recursive(arena, node%array_index, visitor, user_data)
                end if
                
                ! Process slice bounds
                do i = 1, node%num_dimensions
                    if (node%bounds_indices(i) > 0) then
                        call visit_nodes_recursive(arena, node%bounds_indices(i), visitor, user_data)
                    end if
                end do
            end select
        case ("component_access")
            select type (node => arena%entries(node_index)%node)
            type is (component_access_node)
                ! Process the base expression (structure/derived type)
                if (node%base_expr_index > 0) then
                    call visit_nodes_recursive(arena, node%base_expr_index, visitor, user_data)
                end if
            end select
        case ("associate")
            select type (node => arena%entries(node_index)%node)
            type is (associate_node)
                ! Visit association expressions
                if (allocated(node%associations)) then
                    do i = 1, size(node%associations)
                        if (node%associations(i)%expr_index > 0) then
                            call visit_nodes_recursive(arena, node%associations(i)%expr_index, visitor, user_data)
                        end if
                    end do
                end if
                
                ! Visit body statements
                if (allocated(node%body_indices)) then
                    do i = 1, size(node%body_indices)
                        if (node%body_indices(i) > 0) then
                            call visit_nodes_recursive(arena, node%body_indices(i), visitor, user_data)
                        end if
                    end do
                end if
            end select
        end select
    end subroutine visit_nodes_recursive

    ! Check if a specific variable is used in an expression
    function is_variable_used_in_expression(arena, expr_index, var_name) result(used)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index
        character(len=*), intent(in) :: var_name
        logical :: used
        
        type(variable_usage_info_t) :: info
        integer :: i
        
        used = .false.
        info = get_variables_in_expression(arena, expr_index)
        
        if (allocated(info%variable_names)) then
            do i = 1, size(info%variable_names)
                if (info%variable_names(i) == var_name) then
                    used = .true.
                    exit
                end if
            end do
        end if
    end function is_variable_used_in_expression

    ! Count how many times a variable is used in an expression
    function count_variable_usage(arena, expr_index, var_name) result(count)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index
        character(len=*), intent(in) :: var_name
        integer :: count
        
        type(variable_usage_info_t) :: info
        integer :: i
        
        count = 0
        info = get_variables_in_expression(arena, expr_index)
        
        if (allocated(info%variable_names)) then
            do i = 1, size(info%variable_names)
                if (info%variable_names(i) == var_name) then
                    count = info%usage_counts(i)
                    exit
                end if
            end do
        end if
    end function count_variable_usage

    ! Assignment operator for variable_usage_info_t to handle deep copying
    subroutine assign_usage_info(lhs, rhs)
        class(variable_usage_info_t), intent(inout) :: lhs
        type(variable_usage_info_t), intent(in) :: rhs
        
        ! Clear any existing allocations
        if (allocated(lhs%variable_names)) deallocate(lhs%variable_names)
        if (allocated(lhs%usage_counts)) deallocate(lhs%usage_counts)
        if (allocated(lhs%node_indices)) deallocate(lhs%node_indices)
        
        ! Deep copy allocatable arrays
        if (allocated(rhs%variable_names)) then
            allocate(character(len(rhs%variable_names)) :: lhs%variable_names(size(rhs%variable_names)))
            lhs%variable_names = rhs%variable_names
        end if
        
        if (allocated(rhs%usage_counts)) then
            allocate(lhs%usage_counts(size(rhs%usage_counts)))
            lhs%usage_counts = rhs%usage_counts
        end if
        
        if (allocated(rhs%node_indices)) then
            allocate(lhs%node_indices(size(rhs%node_indices)))
            lhs%node_indices = rhs%node_indices
        end if
        
        ! Copy scalar value
        lhs%total_count = rhs%total_count
    end subroutine assign_usage_info

end module variable_usage_tracker_module