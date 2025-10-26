module standardizer_declarations_state
    implicit none
    private

    logical, save :: type_standardization_enabled = .false.

    public :: get_standardizer_type_standardization
    public :: set_standardizer_type_standardization

contains

    subroutine get_standardizer_type_standardization(enabled)
        logical, intent(out) :: enabled
        enabled = type_standardization_enabled
    end subroutine get_standardizer_type_standardization

    subroutine set_standardizer_type_standardization(enabled)
        logical, intent(in) :: enabled
        type_standardization_enabled = enabled
    end subroutine set_standardizer_type_standardization

end module standardizer_declarations_state
