program test_issue_1827_submodule_construct
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    character(len=1), parameter :: nl = new_line('A')
    character(len=*), parameter :: submodule_error = &
        "! ERROR: Unsupported Fortran feature: submodule constructs are not supported"

    call test_submodule_simple()
    call test_submodule_with_contents()
    print *, ""
    print *, "Issue 1827 submodule construct tests completed."

contains

    subroutine test_submodule_simple()
        character(:), allocatable :: input_code, output_code, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        input_code = "submodule (parent_module) child_submodule" // nl // &
                     "    implicit none" // nl // &
                     "end submodule child_submodule"

        print *, ""
        print *, "Test: Simple submodule construct"
        print *, "Input:"
        print *, trim(input_code)

        arena = create_ast_arena()
        call lex_source(input_code, tokens, error_msg)

        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "Lexing error:", trim(error_msg)
            error stop 1
        end if

        call parse_tokens(tokens, arena, prog_index, error_msg)

        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "Parsing error:", trim(error_msg)
            error stop 1
        end if

        call emit_fortran(arena, prog_index, output_code)
        print *, "Output:"
        print *, trim(output_code)

        if (index(output_code, submodule_error) == 0) then
            print *, "FAIL: Expected error message not found"
            error stop 1
        end if

        print *, "PASS: Submodule produces clear error message"
    end subroutine test_submodule_simple

    subroutine test_submodule_with_contents()
        character(:), allocatable :: input_code, output_code, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        input_code = "submodule (parent_module) child_submodule" // nl // &
                     "    implicit none" // nl // &
                     "contains" // nl // &
                     "    module subroutine test()" // nl // &
                     "        print *, 'test'" // nl // &
                     "    end subroutine test" // nl // &
                     "end submodule child_submodule"

        print *, ""
        print *, "Test: Submodule with contents"
        print *, "Input:"
        print *, trim(input_code)

        arena = create_ast_arena()
        call lex_source(input_code, tokens, error_msg)

        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "Lexing error:", trim(error_msg)
            error stop 1
        end if

        call parse_tokens(tokens, arena, prog_index, error_msg)

        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "Parsing error:", trim(error_msg)
            error stop 1
        end if

        call emit_fortran(arena, prog_index, output_code)
        print *, "Output:"
        print *, trim(output_code)

        if (index(output_code, submodule_error) == 0) then
            print *, "FAIL: Expected error message not found"
            error stop 1
        end if

        print *, "PASS: Submodule with contents produces clear error"
    end subroutine test_submodule_with_contents

end program test_issue_1827_submodule_construct
