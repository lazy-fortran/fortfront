module constant_folding_module
    use iso_fortran_env, only: error_unit
    use ast_core
    use ast_arena
    implicit none
    private

    public :: evaluate_constant_condition

    ! Result type for constant evaluation
    integer, parameter :: CONSTANT_UNKNOWN = 0
    integer, parameter :: CONSTANT_TRUE = 1
    integer, parameter :: CONSTANT_FALSE = 2

contains

    ! Evaluate a condition expression to determine if it's a constant
    function evaluate_constant_condition(arena, condition_index) result(constant_value)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: condition_index
        integer :: constant_value
        
        ! Default to unknown (dynamic condition)
        constant_value = CONSTANT_UNKNOWN
        
        if (condition_index <= 0 .or. condition_index > arena%size) then
            return
        end if
        
        ! Check the condition expression
        select type (node => arena%entries(condition_index)%node)
        type is (literal_node)
            ! Handle boolean literals
            if (node%literal_type == "logical" .or. &
                node%value == ".true." .or. node%value == ".false.") then
                if (node%value == ".true.") then
                    constant_value = CONSTANT_TRUE
                else if (node%value == ".false.") then
                    constant_value = CONSTANT_FALSE
                end if
            end if
            
        type is (binary_op_node)
            ! Handle simple constant expressions
            constant_value = evaluate_binary_expression(arena, node)
            
        class default
            ! Unknown or complex expression - cannot fold
            constant_value = CONSTANT_UNKNOWN
        end select
        
    end function evaluate_constant_condition

    ! Evaluate binary expressions for constants
    function evaluate_binary_expression(arena, op_node) result(constant_value)
        type(ast_arena_t), intent(in) :: arena
        type(binary_op_node), intent(in) :: op_node
        integer :: constant_value
        
        integer :: left_int, right_int
        logical :: left_valid, right_valid
        
        constant_value = CONSTANT_UNKNOWN
        
        ! Get left and right operand values
        call evaluate_operand(arena, op_node%left_index, left_int, left_valid)
        call evaluate_operand(arena, op_node%right_index, right_int, right_valid)
        
        ! If either operand is not a constant integer, can't fold
        if (.not. left_valid .or. .not. right_valid) then
            return
        end if
        
        
        ! Evaluate the operation
        select case (trim(op_node%operator))
        case (".eq.", "==")
            if (left_int == right_int) then
                constant_value = CONSTANT_TRUE
            else
                constant_value = CONSTANT_FALSE
            end if
            
        case (".ne.", "/=")
            if (left_int /= right_int) then
                constant_value = CONSTANT_TRUE
            else
                constant_value = CONSTANT_FALSE
            end if
            
        case (".gt.", ">")
            if (left_int > right_int) then
                constant_value = CONSTANT_TRUE
            else
                constant_value = CONSTANT_FALSE
            end if
            
        case (".lt.", "<")
            if (left_int < right_int) then
                constant_value = CONSTANT_TRUE
            else
                constant_value = CONSTANT_FALSE
            end if
            
        case (".ge.", ">=")
            if (left_int >= right_int) then
                constant_value = CONSTANT_TRUE
            else
                constant_value = CONSTANT_FALSE
            end if
            
        case (".le.", "<=")
            if (left_int <= right_int) then
                constant_value = CONSTANT_TRUE
            else
                constant_value = CONSTANT_FALSE
            end if
            
        case default
            ! Unsupported operation
            constant_value = CONSTANT_UNKNOWN
        end select
        
    end function evaluate_binary_expression

    ! Extract integer value from operand
    subroutine evaluate_operand(arena, operand_index, int_value, is_valid)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: operand_index
        integer, intent(out) :: int_value
        logical, intent(out) :: is_valid
        
        ! Default values
        int_value = 0
        is_valid = .false.
        
        if (operand_index <= 0 .or. operand_index > arena%size) then
            return
        end if
        
        select type (node => arena%entries(operand_index)%node)
        type is (literal_node)
            if (node%literal_type == "integer") then
                read(node%value, *) int_value
                is_valid = .true.
            end if
        class default
            ! Not a literal integer
            is_valid = .false.
        end select
        
    end subroutine evaluate_operand

end module constant_folding_module