program test_issue_1823_contiguous_attribute
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    call test_contiguous_pointer_basic()
    print *, ""
    call test_contiguous_pointer_2d_array()
    print *, ""
    print *, "Issue 1823 CONTIGUOUS attribute tests completed."

contains

    subroutine test_contiguous_pointer_basic()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        character(len=1), parameter :: nl = new_line('A')

        input_code = &
            "program test_contiguous_attribute" // nl // &
            "    implicit none" // nl // &
            "    " // nl // &
            "    call test_sub()" // nl // &
            "    " // nl // &
            "contains" // nl // &
            "    subroutine test_sub()" // nl // &
            "        real, dimension(:), contiguous, pointer :: ptr" // nl // &
            "        real, dimension(10), target :: arr" // nl // &
            "        " // nl // &
            "        arr = [(real(i), i=1,10)]" // nl // &
            "        ptr => arr" // nl // &
            "        print *, ptr" // nl // &
            "    end subroutine test_sub" // nl // &
            "end program test_contiguous_attribute"

        arena = create_ast_arena()
        call lex_source(input_code, tokens, error_msg)
        call parse_tokens(tokens, arena, prog_index, error_msg)
        call emit_fortran(arena, prog_index, output_code)

        if (index(output_code, ', contiguous') == 0) then
            print *, "FAIL: CONTIGUOUS attribute not preserved"
            print *, "Output:", output_code
            error stop 1
        else
            print *, "PASS: CONTIGUOUS attribute preserved"
        end if

        if (index(output_code, ', pointer') == 0) then
            print *, "FAIL: POINTER attribute not preserved"
            print *, "Output:", output_code
            error stop 1
        else
            print *, "PASS: POINTER attribute preserved"
        end if

        if (index(output_code, 'real :: contiguous') /= 0) then
            print *, "FAIL: CONTIGUOUS treated as variable name"
            print *, "Output:", output_code
            error stop 1
        else
            print *, "PASS: CONTIGUOUS not treated as variable name"
        end if

        if (index(output_code, ':: ptr') == 0) then
            print *, "FAIL: ptr variable name not preserved"
            print *, "Output:", output_code
            error stop 1
        else
            print *, "PASS: ptr variable name preserved"
        end if
    end subroutine test_contiguous_pointer_basic

    subroutine test_contiguous_pointer_2d_array()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        character(len=1), parameter :: nl = new_line('A')

        input_code = &
            "program test" // nl // &
            "    implicit none" // nl // &
            "    real, dimension(:,:), contiguous, pointer :: mat_ptr" // nl // &
            "    real, dimension(5,5), target :: mat" // nl // &
            "    mat_ptr => mat" // nl // &
            "end program test"

        arena = create_ast_arena()
        call lex_source(input_code, tokens, error_msg)
        call parse_tokens(tokens, arena, prog_index, error_msg)
        call emit_fortran(arena, prog_index, output_code)

        if (index(output_code, ', contiguous') == 0) then
            print *, "FAIL: CONTIGUOUS attribute not preserved for 2D array"
            print *, "Output:", output_code
            error stop 1
        else
            print *, "PASS: CONTIGUOUS preserved for 2D array pointer"
        end if

        if (index(output_code, ', pointer') == 0) then
            print *, "FAIL: POINTER attribute not preserved for 2D array"
            print *, "Output:", output_code
            error stop 1
        else
            print *, "PASS: POINTER preserved for 2D array pointer"
        end if

        if (index(output_code, ':: mat_ptr') == 0) then
            print *, "FAIL: mat_ptr variable name not preserved"
            print *, "Output:", output_code
            error stop 1
        else
            print *, "PASS: mat_ptr variable name preserved"
        end if
    end subroutine test_contiguous_pointer_2d_array

end program test_issue_1823_contiguous_attribute
