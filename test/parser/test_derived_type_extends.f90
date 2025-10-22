program test_derived_type_extends
    use frontend, only: lex_source, parse_tokens
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use lexer_core, only: token_t
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    character(len=*), parameter :: source = &
        "type, extends(base_type) :: derived_type" // new_line('A') // &
        "end type derived_type"

    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: mod_index
    character(len=:), allocatable :: error_msg

    call lex_source(source, tokens, error_msg)
    if (len_trim(error_msg) > 0) then
        print *, "FAIL: lexer error:", trim(error_msg)
        stop 1
    end if

    arena = create_ast_arena()
    call parse_tokens(tokens, arena, mod_index, error_msg)
    if (len_trim(error_msg) > 0) then
        print *, "FAIL: parse error:", trim(error_msg)
        stop 1
    end if

    if (mod_index <= 0) then
        print *, "FAIL: parse failed, no index returned"
        stop 1
    end if

    print *, "PASS: derived type with EXTENDS clause parsed without error"

end program test_derived_type_extends
