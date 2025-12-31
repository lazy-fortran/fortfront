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
        OPERATING_MODE_INFER, &
        OPERATING_MODE_STRICT
    use frontend_core, only: compile_source, compilation_options_t

    implicit none
    private

    ! Main transformation functions
    public :: transform_lazy_fortran_string
    public :: transform_lazy_fortran_string_with_format
    public :: transform_with_context
    public :: compile_source

    ! Context and options types
    public :: format_options_t
    public :: transform_context_t
    public :: compilation_options_t

    ! Input mode constants
    public :: INPUT_MODE_LAZY
    public :: INPUT_MODE_STANDARD
    public :: OPERATING_MODE_INFER
    public :: OPERATING_MODE_STRICT

end module transformation_api
