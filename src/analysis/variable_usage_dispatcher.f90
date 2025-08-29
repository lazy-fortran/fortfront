module variable_usage_dispatcher_module
    use iso_fortran_env, only: error_unit
    use ast_arena_modern
    use variable_usage_core_module
    use variable_usage_node_processors_module
    implicit none
    private

    ! Public procedures  
    public :: dispatch_node_processing
    public :: process_if_node_children, process_do_while_node_children
    public :: process_select_case_node_children, process_where_node_children
    public :: process_where_stmt_node_children, process_multi_declaration_node_children
    public :: process_print_statement_node_children, process_case_block_node_children
    public :: process_do_loop_node_children, process_subroutine_call_children
    public :: process_write_statement_children, process_read_statement_children
    public :: process_allocate_statement_children, process_deallocate_statement_children
    public :: process_associate_construct_children, process_subroutine_def_children
    public :: process_function_def_children

    ! Interface declaration for recursive function from collector
    interface
        recursive subroutine collect_identifiers_recursive(arena, node_index, info)
            import :: ast_arena_t, variable_usage_info_t
            type(ast_arena_t), intent(in) :: arena
            integer, intent(in) :: node_index
            type(variable_usage_info_t), intent(inout) :: info
        end subroutine collect_identifiers_recursive
    end interface

contains

    ! Dispatch node processing based on node type
    subroutine dispatch_node_processing(arena, node_index, info, node_type)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        character(len=*), intent(in) :: node_type
        
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
    end subroutine dispatch_node_processing

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

end module variable_usage_dispatcher_module