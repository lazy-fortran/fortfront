program test_issue_1822_asynchronous_attribute
    use frontend_core, only: lex_source
    use frontend_parsing, only: parse_tokens
    use standardizer, only: standardize_ast
    use codegen_core, only: codegen_core_generate_arena, initialize_codegen
    use ast_arena_modern, only: ast_arena_t
    use lexer_core, only: token_t
    implicit none

    logical :: ok

    ok = check_asynchronous_preserved()
    if (ok) then
        print *, "PASS: Issue #1822 - asynchronous attribute preserved correctly"
    else
        error stop "FAIL: Issue #1822 - asynchronous attribute removed or mangled"
    end if

contains

    logical function check_asynchronous_preserved() result(ok)
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: error_msg
        type(ast_arena_t) :: arena
        integer :: root_index
        character(len=:), allocatable :: output

        ok = .false.

        call initialize_codegen()

        source = 'program test_asynchronous_attribute' // new_line('a') // &
            '    implicit none' // new_line('a') // &
            '    integer, asynchronous :: async_var' // new_line('a') // &
            '    async_var = 42' // new_line('a') // &
            '    print *, async_var' // new_line('a') // &
            'end program test_asynchronous_attribute'

        call lex_source(source, tokens, error_msg)
        if (len_trim(error_msg) > 0) return

        call parse_tokens(tokens, arena, root_index, error_msg)
        if (len_trim(error_msg) > 0) return

        call standardize_ast(arena, root_index)
        output = codegen_core_generate_arena(arena, root_index)

        if (index(output, 'integer :: async_var, asynchronous') > 0) return
        if (index(output, 'integer, asynchronous :: async_var') == 0) return

        ok = .true.
    end function check_asynchronous_preserved

end program test_issue_1822_asynchronous_attribute
