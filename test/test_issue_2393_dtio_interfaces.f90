program test_issue_2393_dtio_interfaces
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        iostat_eor
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    call test_dtio_basic_write_formatted()
    call test_dtio_read_write_both()
    call test_dtio_unformatted()

    print *, ""
    print *, "All DTIO interface tests passed"

contains

    include 'common/read_example.inc'

    subroutine test_dtio_basic_write_formatted()
        character(:), allocatable :: input_code, output_code, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index

        print *, ""
        print *, "=== Test 1: DTIO interface write(formatted) ==="

        call read_example('examples/f90/dtio_interface_basic.f90', input_code)

        call lex_source(input_code, tokens, error_msg)
        if (error_msg /= "") then
            print *, "✗ FAIL: Lexer error:", trim(error_msg)
            error stop 1
        end if

        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (error_msg /= "") then
            print *, "✗ FAIL: Parser error:", trim(error_msg)
            error stop 1
        end if

        call emit_fortran(arena, root_index, output_code)

        if (index(output_code, "interface write(formatted)") == 0) then
            print *, "✗ FAIL: interface write(formatted) not found"
            print *, "Output:", output_code
            error stop 1
        end if

        if (index(output_code, "end interface") == 0) then
            print *, "✗ FAIL: end interface not found"
            error stop 1
        end if

        print *, "✓ PASS: DTIO write(formatted) interface parsed correctly"
    end subroutine test_dtio_basic_write_formatted

    subroutine test_dtio_read_write_both()
        character(:), allocatable :: input_code, output_code, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index

        print *, ""
        print *, "=== Test 2: DTIO interfaces with both read and write ==="

        call read_example('examples/f90/dtio_interface_read_write.f90', input_code)

        call lex_source(input_code, tokens, error_msg)
        if (error_msg /= "") then
            print *, "✗ FAIL: Lexer error:", trim(error_msg)
            error stop 1
        end if

        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (error_msg /= "") then
            print *, "✗ FAIL: Parser error:", trim(error_msg)
            error stop 1
        end if

        call emit_fortran(arena, root_index, output_code)

        if (index(output_code, "interface write(formatted)") == 0) then
            print *, "✗ FAIL: interface write(formatted) not found"
            error stop 1
        end if

        if (index(output_code, "interface read(formatted)") == 0) then
            print *, "✗ FAIL: interface read(formatted) not found"
            error stop 1
        end if

        print *, "✓ PASS: Both read and write interfaces parsed correctly"
    end subroutine test_dtio_read_write_both

    subroutine test_dtio_unformatted()
        character(:), allocatable :: input_code, output_code, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index

        print *, ""
        print *, "=== Test 3: DTIO unformatted interfaces ==="

        call read_example('examples/f90/dtio_interface_unformatted.f90', input_code)

        call lex_source(input_code, tokens, error_msg)
        if (error_msg /= "") then
            print *, "✗ FAIL: Lexer error:", trim(error_msg)
            error stop 1
        end if

        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (error_msg /= "") then
            print *, "✗ FAIL: Parser error:", trim(error_msg)
            error stop 1
        end if

        call emit_fortran(arena, root_index, output_code)

        if (index(output_code, "interface write(unformatted)") == 0) then
            print *, "✗ FAIL: interface write(unformatted) not found"
            error stop 1
        end if

        if (index(output_code, "interface read(unformatted)") == 0) then
            print *, "✗ FAIL: interface read(unformatted) not found"
            error stop 1
        end if

        print *, "✓ PASS: Unformatted DTIO interfaces parsed correctly"
    end subroutine test_dtio_unformatted


end program test_issue_2393_dtio_interfaces
