program test_extends_with_attributes
    use frontend, only: lex_source, parse_tokens, emit_fortran
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use lexer_core, only: token_t
    implicit none

    character(len=*), parameter :: source = &
        "type, public, extends(base_type) :: derived_type" // new_line('A') // &
        "    integer :: x" // new_line('A') // &
        "end type derived_type"
    character(len=:), allocatable :: code
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

    call emit_fortran(arena, mod_index, code)
    if (index(code, "extends(base_type)") == 0) then
        print *, "FAIL: extends clause not in generated code"
        print *, "Generated code:", code
        stop 1
    end if

    if (index(code, "public") == 0) then
        print *, "FAIL: public attribute not in generated code"
        print *, "Generated code:", code
        stop 1
    end if

    print *, "PASS: extends with other attributes roundtrip"

end program test_extends_with_attributes
