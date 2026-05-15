module fortfront_lexer
    use lexer_api, only: token_t, tokenize_result_t, lexer_options_t, &
                         tokenize_core, tokenize_core_with_trivia, &
                         tokenize_safe, tokenize_safe_with_trivia, &
                         tokenize_core_safe, lex_source, lex_file, &
                         token_type_name, to_lower, TK_EOF, TK_IDENTIFIER, &
                         TK_NUMBER, TK_STRING, TK_OPERATOR, TK_KEYWORD, &
                         TK_NEWLINE, TK_COMMENT, TK_WHITESPACE, TK_UNKNOWN
    implicit none
    public
end module fortfront_lexer
