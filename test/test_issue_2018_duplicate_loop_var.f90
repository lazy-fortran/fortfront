program test_issue_2018_duplicate_loop_var
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    call test_function_with_explicit_loop_var()
    print *, "Issue 2018 duplicate loop variable test completed."

contains

    subroutine test_function_with_explicit_loop_var()
        character(:), allocatable :: source, output
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        call read_example('examples/f90/issue_2018_function_returns_array_duplicate_var.f90', source)

        arena = create_ast_arena()
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "Lexing error:", trim(error_msg)
            error stop 1
        end if

        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "Parsing error:", trim(error_msg)
            error stop 1
        end if

        call emit_fortran(arena, prog_index, output)

        if (count_occurrences(output, 'integer :: i') /= 2) then
            print *, "FAIL: Expected exactly 2 'integer :: i' declarations (one in main, one in function)"
            print *, "Output:", output
            error stop 1
        end if

        if (index(output, 'integer :: i') == 0) then
            print *, "FAIL: Loop variable declaration missing"
            error stop 1
        end if

        print *, "[PASS] Function with explicit loop variable declaration"
    end subroutine test_function_with_explicit_loop_var

    integer function count_occurrences(text, pattern)
        character(*), intent(in) :: text
        character(*), intent(in) :: pattern
        integer :: pos, count

        count = 0
        pos = 1

        do
            pos = index(text(pos:), pattern)
            if (pos == 0) exit
            count = count + 1
            pos = pos + len(pattern)
        end do

        count_occurrences = count
    end function count_occurrences

    subroutine read_example(filepath, content)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: content
        integer :: unit, ios, file_size
        character(len=1), allocatable :: buffer(:)

        open (newunit=unit, file=filepath, status='old', action='read', &
              form='unformatted', access='stream', iostat=ios)
        if (ios /= 0) then
            print *, "Error opening file:", filepath
            error stop 1
        end if

        inquire (unit=unit, size=file_size)
        allocate (buffer(file_size))
        read (unit, iostat=ios) buffer
        close (unit)

        if (ios /= 0) then
            print *, "Error reading file:", filepath
            error stop 1
        end if

        allocate (character(len=file_size) :: content)
        content = transfer(buffer, content)
        deallocate (buffer)
    end subroutine read_example

end program test_issue_2018_duplicate_loop_var
