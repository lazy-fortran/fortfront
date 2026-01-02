program test_issue_1740_keyword_arguments
    use frontend_core, only: lex_source
    use frontend_parsing, only: parse_tokens
    use standardizer, only: standardize_ast
    use codegen_core, only: codegen_core_generate_arena, initialize_codegen
    use ast_arena_modern, only: ast_arena_t
    use lexer_core, only: token_t
    implicit none

    logical :: ok

    ok = check_keyword_argument_preserved()
    if (ok) then
        print *, "PASS: Issue #1740 - keyword arg preserved"
    else
        error stop "FAIL: Issue #1740 - keyword arg lost"
    end if

contains

    include 'common/cli_io_reader.inc'
    include 'common/read_example.inc'

    function check_keyword_argument_preserved() result(passed)
        logical :: passed
        character(len=:), allocatable :: source
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: output_code
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index

        passed = .true.

        call initialize_codegen()

        call read_example('examples/f90/issue_1740_optional_keyword_arguments.f90', source)
        call lex_source(source, tokens, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, "FAIL: lexing error:", trim(error_msg)
            passed = .false.
            return
        end if

        call parse_tokens(tokens, arena, root_index, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, "FAIL: parsing error:", trim(error_msg)
            passed = .false.
            return
        end if

        call standardize_ast(arena, root_index)

        output_code = codegen_core_generate_arena(arena, root_index)

        if (index(output_code, "greeting = 'Hi'") <= 0) then
            print *, "FAIL: keyword arg missing"
            passed = .false.
        else
            print *, "PASS: keyword arg preserved"
        end if
    end function check_keyword_argument_preserved


end program test_issue_1740_keyword_arguments
