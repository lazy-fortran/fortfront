module frontend_transformation
    use frontend_transformation_common, only: format_options_t, transform_context_t
    use frontend_transformation_pipeline, only: transform_lazy_fortran_string, &
        transform_lazy_fortran_string_with_format, transform_with_context, &
        detect_input_mode_from_content
    use frontend_transformation_structure
    use frontend_transformation_analysis
    use semantic_input_mode, only: INPUT_MODE_LAZY, INPUT_MODE_STANDARD
    implicit none
    private

    public :: transform_lazy_fortran_string
    public :: transform_lazy_fortran_string_with_format
    public :: transform_with_context
    public :: detect_input_mode_from_content
    public :: format_options_t
    public :: transform_context_t
    public :: INPUT_MODE_LAZY
    public :: INPUT_MODE_STANDARD

end module frontend_transformation
