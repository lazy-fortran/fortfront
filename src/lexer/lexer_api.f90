module lexer_api
    ! Public lexer API for library consumers
    ! Provides tokenization functionality for Fortran source code
    use lexer_core, only: &
        token_t, &
        tokenize_core, &
        tokenize_core_with_trivia, &
        tokenize_safe, &
        tokenize_safe_with_trivia, &
        tokenize_core_safe, &
        token_type_name, &
        tokenize_result_t, &
        lexer_options_t, &
        to_lower, &
        TK_EOF, &
        TK_IDENTIFIER, &
        TK_NUMBER, &
        TK_STRING, &
        TK_OPERATOR, &
        TK_KEYWORD, &
        TK_NEWLINE, &
        TK_COMMENT, &
        TK_WHITESPACE, &
        TK_UNKNOWN
    use frontend_core, only: lex_source, lex_file

    implicit none
    private

    ! Core types
    public :: token_t
    public :: tokenize_result_t
    public :: lexer_options_t

    ! Token kind constants
    public :: TK_EOF
    public :: TK_IDENTIFIER
    public :: TK_NUMBER
    public :: TK_STRING
    public :: TK_OPERATOR
    public :: TK_KEYWORD
    public :: TK_NEWLINE
    public :: TK_COMMENT
    public :: TK_WHITESPACE
    public :: TK_UNKNOWN

    ! Main tokenization functions
    public :: tokenize_core
    public :: tokenize_core_with_trivia
    public :: tokenize_safe
    public :: tokenize_safe_with_trivia
    public :: tokenize_core_safe
    public :: lex_source
    public :: lex_file

    ! Utility functions
    public :: token_type_name
    public :: to_lower

end module lexer_api
