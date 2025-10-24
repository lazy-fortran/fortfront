program test_issue_1809_implicit_none_type
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    call test_implicit_none_type_spec()
    print *, ""
    call test_implicit_none_external_spec()
    print *, ""
    call test_implicit_none_plain()
    print *, ""
    print *, "Issue 1809 implicit none specification tests completed."

contains

    subroutine test_implicit_none_type_spec()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        character(len=1), parameter :: nl = new_line('A')

        input_code = &
            "program test_implicit_none_typing" // nl // &
            "    implicit none (type)" // nl // &
            "    integer :: x" // nl // &
            "    " // nl // &
            "    x = 5" // nl // &
            "    print *, x" // nl // &
            "end program test_implicit_none_typing"

        arena = create_ast_arena()
        call lex_source(input_code, tokens, error_msg)
        call parse_tokens(tokens, arena, prog_index, error_msg)
        call emit_fortran(arena, prog_index, output_code)

        if (index(output_code, 'implicit none (type)') == 0) then
            print *, "FAIL: implicit none (type) specification not preserved"
            print *, "Output:", output_code
            error stop 1
        end if

        print *, "PASS: implicit none (type) specification preserved"
    end subroutine test_implicit_none_type_spec

    subroutine test_implicit_none_external_spec()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        character(len=1), parameter :: nl = new_line('A')

        input_code = &
            "program test_implicit_none_external" // nl // &
            "    implicit none (external)" // nl // &
            "    integer :: x" // nl // &
            "    " // nl // &
            "    x = 5" // nl // &
            "    print *, x" // nl // &
            "end program test_implicit_none_external"

        arena = create_ast_arena()
        call lex_source(input_code, tokens, error_msg)
        call parse_tokens(tokens, arena, prog_index, error_msg)
        call emit_fortran(arena, prog_index, output_code)

        if (index(output_code, 'implicit none (external)') == 0) then
            print *, "FAIL: implicit none (external) specification not preserved"
            print *, "Output:", output_code
            error stop 1
        end if

        print *, "PASS: implicit none (external) specification preserved"
    end subroutine test_implicit_none_external_spec

    subroutine test_implicit_none_plain()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        character(len=1), parameter :: nl = new_line('A')

        input_code = &
            "program test_implicit_none_plain" // nl // &
            "    implicit none" // nl // &
            "    integer :: x" // nl // &
            "    " // nl // &
            "    x = 5" // nl // &
            "    print *, x" // nl // &
            "end program test_implicit_none_plain"

        arena = create_ast_arena()
        call lex_source(input_code, tokens, error_msg)
        call parse_tokens(tokens, arena, prog_index, error_msg)
        call emit_fortran(arena, prog_index, output_code)

        if (index(output_code, 'implicit none') == 0) then
            print *, "FAIL: implicit none not preserved"
            print *, "Output:", output_code
            error stop 1
        end if

        if (index(output_code, 'implicit none (') /= 0) then
            print *, "FAIL: plain implicit none should not have specification"
            print *, "Output:", output_code
            error stop 1
        end if

        print *, "PASS: plain implicit none preserved without specification"
    end subroutine test_implicit_none_plain

end program test_issue_1809_implicit_none_type
