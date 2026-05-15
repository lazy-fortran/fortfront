module fortfront_compiler
    use frontend_compiler_api, only: compiler_frontend_options_t, &
                                     compiler_frontend_result_t, &
                                     compile_frontend_from_string, &
                                     compile_frontend_from_file
    use frontend_compiler_queries, only: is_subroutine_call_statement, &
                                         get_subroutine_call_name, &
                                         get_subroutine_call_arg_indices
    use fortfront_semantic, only: INPUT_MODE_LAZY, INPUT_MODE_STANDARD, &
                                  OPERATING_MODE_INFER, OPERATING_MODE_STRICT
    use fortfront_ast, only: ast_arena_t
    use fortfront_lexer, only: token_t
    implicit none
    public
end module fortfront_compiler
