module fortfront_codegen
    use codegen_api, only: generate_code_from_arena, generate_code_polymorphic, &
                           initialize_codegen, emit_fortran, &
                           set_type_standardization, get_type_standardization, &
                           set_indent_config, get_indent_config, &
                           set_line_length_config, get_line_length_config, &
                           add_line_continuations, set_arena_generator
    implicit none
    public
end module fortfront_codegen
