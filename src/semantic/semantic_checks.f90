module semantic_checks
    use ast_core
    use semantic_analyzer, only: semantic_context_t
    implicit none
    private

    public :: check_intent_violations

contains

    ! Check for INTENT violations in assignments
    subroutine check_intent_violations(ctx, arena, assign_index, error_msg)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: assign_index
        character(len=:), allocatable, intent(out) :: error_msg
        
        integer :: target_index
        character(len=:), allocatable :: var_name
        logical :: is_parameter
        character(len=:), allocatable :: intent_attr
        
        error_msg = ""
        
        ! Get assignment node
        if (assign_index <= 0 .or. assign_index > arena%size) return
        
        select type (assign_node => arena%entries(assign_index)%node)
        type is (assignment_node)
            target_index = assign_node%target_index
            
            ! Get target variable name
            if (target_index > 0 .and. target_index <= arena%size) then
                select type (target => arena%entries(target_index)%node)
                type is (identifier_node)
                    var_name = target%name
                    
                    ! Check if variable is a parameter with intent
                    call check_parameter_intent(ctx, var_name, is_parameter, intent_attr)
                    
                    if (is_parameter .and. allocated(intent_attr)) then
                        if (intent_attr == "in") then
                            error_msg = "Cannot modify INTENT(IN) parameter: " // var_name
                        end if
                    end if
                end select
            end if
        end select
    end subroutine check_intent_violations
    
    ! Check if a variable is a parameter and get its intent
    subroutine check_parameter_intent(ctx, var_name, is_parameter, intent_attr)
        type(semantic_context_t), intent(in) :: ctx
        character(len=*), intent(in) :: var_name
        logical, intent(out) :: is_parameter
        character(len=:), allocatable, intent(out) :: intent_attr
        
        ! Check if variable is a parameter in the current scope
        is_parameter = ctx%param_tracker%is_parameter(var_name)
        
        if (is_parameter) then
            intent_attr = ctx%param_tracker%get_parameter_intent(var_name)
        else
            intent_attr = ""
        end if
    end subroutine check_parameter_intent

end module semantic_checks