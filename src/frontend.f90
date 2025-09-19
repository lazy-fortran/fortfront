module frontend
    ! fortfront - Core analysis frontend
    ! Simple, clean interface: Lexer -> Parser -> Semantic -> Standard Fortran codegen
    !
    ! Decision: keep frontend as the stable public entry point
    ! -------------------------------------------------------------------------
    ! This module intentionally remains as a thin umbrella that re-exports the
    ! documented public API from the refactored modules. It exists to provide a
    ! single import for downstream users and the CLI.
    !
    ! Scope policy
    ! - Only re-export explicitly documented entry points (lexer/parsing,
    !   semantics, transformation, and codegen helpers).
    ! - Internal implementation must not depend on `frontend`; internal code
    !   should import narrow modules directly (e.g., `frontend_parsing`,
    !   `frontend_core`, `frontend_transformation`).
    ! - Tests may import `frontend` to validate public behavior.
    !
    ! This comment documents the umbrella decision per Phase 3.

    ! Re-export core functionality
    use frontend_core, only: lex_source, analyze_semantics, emit_fortran, &
                            compile_source, compilation_options_t, &
                            lex_file
    use frontend_parsing, only: parse_tokens, parse_tokens_safe, &
                               parse_result_with_index_t, &
                               find_program_unit_boundary, &
                               is_function_start, is_end_function, &
                               parse_program_unit, is_do_loop_start, &
                               is_do_while_start, is_select_case_start, &
                               is_end_do, is_end_select, is_if_then_start, is_end_if
    use frontend_transformation, only: transform_lazy_fortran_string, &
                                     transform_lazy_fortran_string_with_format, &
                                     format_options_t

    implicit none
    private

    ! Re-export all public interfaces for backward compatibility
    public :: lex_source, parse_tokens, parse_tokens_safe, parse_result_with_index_t, &
              analyze_semantics, emit_fortran
    public :: compile_source, compilation_options_t
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
