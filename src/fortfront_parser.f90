module fortfront_parser
    use parser_api, only: parse_result_with_index_t, ast_arena_t, &
                          compiler_arena_t, parse_tokens, parse_tokens_safe, &
                          create_compiler_arena, destroy_compiler_arena, &
                          find_program_unit_boundary, is_function_start, &
                          is_end_function, parse_program_unit, &
                          is_do_loop_start, is_do_while_start, &
                          is_select_case_start, is_end_do, is_end_select, &
                          is_if_then_start, is_end_if
    implicit none
    public
end module fortfront_parser
