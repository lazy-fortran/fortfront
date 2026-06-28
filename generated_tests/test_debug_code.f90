program test_debug_code
    use fortfront
    implicit none
    character(len=:), allocatable :: source, error_msg, fortran_code
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: prog_index

    source = "x = 42"
    call lex_source(source, tokens, error_msg)
    arena = create_ast_arena()
    call parse_tokens(tokens, arena, prog_index, error_msg)
    call analyze_semantics(arena, prog_index)
    call emit_fortran(arena, prog_index, fortran_code)

    print *, "Generated code length:", len_trim(fortran_code)
    print *, "Generated code:"
    print *, trim(fortran_code)
    print *, "---END---"

    if (index(fortran_code, "implicit none") > 0) then
        print *, "FOUND: implicit none"
    else
        print *, "NOT FOUND: implicit none"
    end if
end program test_debug_code
