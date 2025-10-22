module transformation_api
    ! Public transformation API for library consumers
    ! Provides high-level transformation from Lazy Fortran to Standard Fortran
    use frontend_transformation, only: &
        transform_lazy_fortran_string, &
        transform_lazy_fortran_string_with_format, &
        transform_with_context, &
        format_options_t, &
        transform_context_t, &
        INPUT_MODE_LAZY, &
        INPUT_MODE_STANDARD, &
        detect_input_mode_from_content

    implicit none
    private

    ! Main transformation functions
    public :: transform_lazy_fortran_string
    public :: transform_lazy_fortran_string_with_format
    public :: transform_with_context

    ! Context and options types
    public :: format_options_t
    public :: transform_context_t

    ! Input mode constants
    public :: INPUT_MODE_LAZY
    public :: INPUT_MODE_STANDARD

    ! Utility functions
    public :: detect_input_mode_from_content

end module transformation_api
