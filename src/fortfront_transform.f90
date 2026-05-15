module fortfront_transform
    use transformation_api, only: transform_lazy_fortran_string, &
                                  transform_lazy_fortran_string_with_format, &
                                  transform_with_context, compile_source, &
                                  format_options_t, transform_context_t, &
                                  compilation_options_t, INPUT_MODE_LAZY, &
                                  INPUT_MODE_STANDARD, OPERATING_MODE_INFER, &
                                  OPERATING_MODE_STRICT
    implicit none
    public
end module fortfront_transform
