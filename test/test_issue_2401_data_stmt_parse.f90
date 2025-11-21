program test_issue_2401_data_stmt_parse
    use, intrinsic :: iso_fortran_env, only: output_unit
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use lexer_core, only: token_t
    implicit none

    character(len=:), allocatable :: source, output_code, error_msg
    type(ast_arena_t) :: arena
    type(token_t), allocatable :: tokens(:)
    integer :: root_index
    logical :: test_passed

    test_passed = .true.

    call read_example('examples/f90/data_stmt_parse.f90', source)

    arena = create_ast_arena()
    call lex_source(source, tokens, error_msg)

    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        write (output_unit, '(A)') "FAIL: Lexing error: " // trim(error_msg)
        error stop 1
    end if

    call parse_tokens(tokens, arena, root_index, error_msg)

    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        write (output_unit, '(A)') "FAIL: Parsing error: " // trim(error_msg)
        write (output_unit, '(A)') "Error was: " // error_msg
        error stop 1
    end if

    call emit_fortran(arena, root_index, output_code)

    if (index(output_code, "data") == 0) then
        write (output_unit, '(A)') "FAIL: data keyword missing from output"
        write (output_unit, '(A)') "Output was:"
        write (output_unit, '(A)') output_code
        test_passed = .false.
    end if

    if (index(output_code, "integer") == 0 .and. index(output_code, "dimension") == 0) then
        write (output_unit, '(A)') "FAIL: array declaration missing from output"
        test_passed = .false.
    end if

    if (index(output_code, "print") == 0) then
        write (output_unit, '(A)') "FAIL: print statement missing from output"
        test_passed = .false.
    end if

    if (test_passed) then
        write (output_unit, '(A)') "PASS: Issue #2401 data statement parsed correctly"
    else
        error stop 1
    end if

contains

    subroutine read_example(filepath, content)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: content
        integer :: unit_num, file_size, iostat_val
        character(len=1), allocatable :: buffer(:)

        open (newunit=unit_num, file=filepath, status='old', &
              action='read', form='unformatted', access='stream', iostat=iostat_val)
        if (iostat_val /= 0) then
            write (output_unit, '(A)') "FAIL: Could not open file: " // filepath
            error stop 1
        end if

        inquire (unit=unit_num, size=file_size)
        allocate (buffer(file_size))
        read (unit_num, iostat=iostat_val) buffer

        if (iostat_val /= 0) then
            write (output_unit, '(A)') "FAIL: Could not read file: " // filepath
            close (unit_num)
            error stop 1
        end if

        close (unit_num)
        allocate (character(len=file_size) :: content)
        content = transfer(buffer, content)
    end subroutine read_example

end program test_issue_2401_data_stmt_parse
