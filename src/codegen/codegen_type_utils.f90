module codegen_type_utils
    implicit none
    private

    ! Type standardization configuration
    logical, save :: standardize_types_enabled = .true.

    public :: set_type_standardization, get_type_standardization

contains

    ! Get current type standardization setting
    subroutine get_type_standardization(enabled)
        logical, intent(out) :: enabled
        enabled = standardize_types_enabled
    end subroutine get_type_standardization

    ! Set type standardization setting
    subroutine set_type_standardization(enabled)
        logical, intent(in) :: enabled
        standardize_types_enabled = enabled
    end subroutine set_type_standardization

end module codegen_type_utils