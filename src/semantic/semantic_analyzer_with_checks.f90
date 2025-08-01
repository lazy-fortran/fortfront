module semantic_analyzer_with_checks
    use semantic_analyzer
    use semantic_checks
    use ast_core
    implicit none
    private
    
    public :: analyze_program_with_checks
    
contains

    ! Analyze AST with semantic checks including INTENT violations
    subroutine analyze_program_with_checks(arena, root_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: root_index
        
        type(semantic_context_t) :: ctx
        
        ! Create semantic context
        ctx = create_semantic_context()
        
        ! Do regular semantic analysis
        call analyze_program(ctx, arena, root_index)
        
        ! Now check for INTENT violations in all assignments
        call check_all_assignments(ctx, arena, root_index)
        
    end subroutine analyze_program_with_checks
    
    ! Recursively check all assignments in the AST
    recursive subroutine check_all_assignments(ctx, arena, node_index)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index
        
        character(len=:), allocatable :: error_msg
        integer :: i, j
        
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        select type (node => arena%entries(node_index)%node)
        type is (assignment_node)
            ! Check this assignment
            call check_intent_violations(ctx, arena, node_index, error_msg)
            if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
                error stop error_msg
            end if
            
        type is (program_node)
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    call check_all_assignments(ctx, arena, node%body_indices(i))
                end do
            end if
            
        type is (function_def_node)
            ! Enter function scope and set up parameters
            call ctx%scopes%enter_function(node%name)
            call ctx%param_tracker%clear()
            
            ! Track parameters
            if (allocated(node%param_indices)) then
                do i = 1, size(node%param_indices)
                    if (allocated(arena%entries(node%param_indices(i))%node)) then
                        select type (param => arena%entries(node%param_indices(i))%node)
                        type is (parameter_declaration_node)
                            call ctx%param_tracker%add_parameter(param%name, param%intent)
                        end select
                    end if
                end do
            end if
            
            ! Check body
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    call check_all_assignments(ctx, arena, node%body_indices(i))
                end do
            end if
            
            ! Exit scope
            call ctx%scopes%leave_scope()
            
        type is (subroutine_def_node)
            ! Enter subroutine scope and set up parameters
            call ctx%scopes%enter_subroutine(node%name)
            call ctx%param_tracker%clear()
            
            ! Track parameters
            if (allocated(node%param_indices)) then
                do i = 1, size(node%param_indices)
                    if (allocated(arena%entries(node%param_indices(i))%node)) then
                        select type (param => arena%entries(node%param_indices(i))%node)
                        type is (parameter_declaration_node)
                            call ctx%param_tracker%add_parameter(param%name, param%intent)
                        end select
                    end if
                end do
            end if
            
            ! Check body
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    call check_all_assignments(ctx, arena, node%body_indices(i))
                end do
            end if
            
            ! Exit scope
            call ctx%scopes%leave_scope()
            
        type is (if_node)
            ! Check then branch
            if (allocated(node%then_body_indices)) then
                do i = 1, size(node%then_body_indices)
                    call check_all_assignments(ctx, arena, node%then_body_indices(i))
                end do
            end if
            
            ! Check else branch
            if (allocated(node%else_body_indices)) then
                do i = 1, size(node%else_body_indices)
                    call check_all_assignments(ctx, arena, node%else_body_indices(i))
                end do
            end if
            
        type is (do_loop_node)
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    call check_all_assignments(ctx, arena, node%body_indices(i))
                end do
            end if
            
        type is (do_while_node)
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    call check_all_assignments(ctx, arena, node%body_indices(i))
                end do
            end if
            
        type is (where_node)
            if (allocated(node%where_body_indices)) then
                do i = 1, size(node%where_body_indices)
                    call check_all_assignments(ctx, arena, node%where_body_indices(i))
                end do
            end if
            
            if (allocated(node%elsewhere_clauses)) then
                do i = 1, size(node%elsewhere_clauses)
                    if (allocated(node%elsewhere_clauses(i)%body_indices)) then
                        do j = 1, size(node%elsewhere_clauses(i)%body_indices)
                            call check_all_assignments(ctx, arena, node%elsewhere_clauses(i)%body_indices(j))
                        end do
                    end if
                end do
            end if
            
        end select
        
    end subroutine check_all_assignments

end module semantic_analyzer_with_checks