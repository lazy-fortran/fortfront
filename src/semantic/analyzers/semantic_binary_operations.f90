module semantic_binary_operations
    ! Binary operation inference logic extracted from semantic_analyzer
    ! for architectural compliance (Issue #1117)
    use type_system_unified, only: mono_type_t, &
                                   create_mono_type, &
                                   TCHAR, TLOGICAL
    use semantic_type_operations, only: get_common_type
    implicit none
    private

    public :: infer_string_concatenation
    public :: infer_comparison_operation
    public :: infer_logical_operation

contains

    ! Calculate string concatenation result type
    recursive function infer_string_concatenation(left_typ, right_typ) result(typ)
        type(mono_type_t), intent(in) :: left_typ, right_typ
        type(mono_type_t) :: typ
        integer :: left_size, right_size, total_size
        logical :: can_calculate_size
        
        ! Try to get sizes of operands
        left_size = 0
        right_size = 0
        can_calculate_size = .false.
        
        ! Get left operand size
        if (left_typ%kind == TCHAR .and. left_typ%size >= 0) then
            left_size = left_typ%size
        end if
        
        ! Get right operand size
        if (right_typ%kind == TCHAR .and. right_typ%size >= 0) then
            right_size = right_typ%size
        end if
        
        ! If we can determine both sizes, calculate total
        if (left_typ%kind == TCHAR .and. right_typ%kind == TCHAR .and. &
            left_typ%size >= 0 .and. right_typ%size >= 0) then
            total_size = left_size + right_size
            can_calculate_size = .true.
        end if
        
        ! Create appropriate character type
        if (can_calculate_size) then
            typ = create_mono_type(TCHAR, char_size=total_size)
            typ%alloc_info%needs_allocatable_string = .false.
        else
            typ = create_mono_type(TCHAR)
            typ%alloc_info%needs_allocatable_string = .true.
        end if
    end function infer_string_concatenation

    ! Infer comparison operation result type
    recursive function infer_comparison_operation(left_typ, right_typ) result(typ)
        type(mono_type_t), intent(in) :: left_typ, right_typ
        type(mono_type_t) :: typ
        
        ! Comparison operators always return logical
        typ = create_mono_type(TLOGICAL)
    end function infer_comparison_operation

    ! Infer logical operation result type
    recursive function infer_logical_operation() result(typ)
        type(mono_type_t) :: typ
        
        ! Logical operators always return logical
        typ = create_mono_type(TLOGICAL)
    end function infer_logical_operation

end module semantic_binary_operations