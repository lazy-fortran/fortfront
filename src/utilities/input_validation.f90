module input_validation
    ! Dedicated input validation module (Issue #262)
    !
    ! Provides comprehensive validation functions for input source code analysis
    ! Cleanly separated from frontend concerns with no circular dependencies

    use lexer_core, only: token_t, TK_EOF, TK_KEYWORD, TK_COMMENT, TK_NEWLINE, &
        TK_OPERATOR, TK_IDENTIFIER, TK_NUMBER, TK_UNKNOWN, &
        TK_STRING, &
        TK_WHITESPACE, to_lower
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_keyword_disambiguation_module, only: keyword_should_parse_as_identifier

    implicit none
    private

    ! Public API for input validation
    public :: validate_basic_syntax
    public :: check_missing_then_statements
    public :: check_incomplete_statements
    public :: check_incomplete_lazy_function_defs
    public :: check_for_fortran_content
    public :: check_missing_end_constructs
    public :: contains_invalid_patterns
    public :: has_only_meaningless_tokens
    public :: format_enhanced_error
    public :: format_syntax_error
    public :: split_into_lines

    ! Internal helper functions (private to module)
    private :: is_likely_valid_fortran
    private :: has_any_fortran_patterns
    private :: is_likely_fortran_expression
    private :: detect_lazy_func_header

contains

    include 'input_validation_part1.inc'
    include 'input_validation_part2.inc'

end module input_validation
