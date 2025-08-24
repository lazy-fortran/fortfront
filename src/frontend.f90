module frontend
    ! fortfront - Core analysis frontend
    ! Simple, clean interface: Lexer → Parser → Semantic → Standard Fortran codegen
    ! This module provides backward compatibility by re-exporting functionality
    ! from the refactored modular structure

    ! Re-export core functionality
    use frontend_core, only: lex_source, analyze_semantics, emit_fortran, &
                            compile_source, compilation_options_t, &
                            compile_from_tokens_json, compile_from_ast_json, &
                            compile_from_semantic_json, lex_file
    use frontend_parsing, only: parse_tokens, find_program_unit_boundary, &
                               is_function_start, is_end_function, parse_program_unit, &
                               is_do_loop_start, is_do_while_start, is_select_case_start, &
                               is_end_do, is_end_select, is_if_then_start, is_end_if
    use frontend_transformation, only: transform_lazy_fortran_string, &
                                     transform_lazy_fortran_string_with_format, &
                                     format_options_t

    implicit none
    private

    ! Re-export all public interfaces for backward compatibility
    public :: lex_source, parse_tokens, analyze_semantics, emit_fortran
    public :: compile_source, compilation_options_t
    public :: compile_from_tokens_json, compile_from_ast_json, &
              compile_from_semantic_json
    public :: transform_lazy_fortran_string, &
              transform_lazy_fortran_string_with_format, format_options_t
    ! Debug functions for unit testing
    public :: find_program_unit_boundary, is_function_start, is_end_function, &
              parse_program_unit
    public :: is_do_loop_start, is_do_while_start, is_select_case_start, &
              is_end_do, is_end_select
    public :: is_if_then_start, is_end_if
    public :: lex_file

end module frontend