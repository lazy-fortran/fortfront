program test_issue_2415_multiple_sets
    use, intrinsic :: iso_fortran_env, only: output_unit
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use lexer_core, only: token_t
    implicit none

    character(len=:), allocatable :: source, first_output, second_output
    character(len=:), allocatable :: error_msg
    type(ast_arena_t) :: arena1, arena2
    type(token_t), allocatable :: tokens1(:), tokens2(:)
    integer :: root1, root2

    call read_example('examples/f90/data_multiple_sets.f90', source)

    arena1 = create_ast_arena()
    call lex_source(source, tokens1, error_msg)
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        write (output_unit, '(A)') "FAIL: First lex error: " // trim(error_msg)
        error stop 1
    end if

    call parse_tokens(tokens1, arena1, root1, error_msg)
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        write (output_unit, '(A)') "FAIL: First parse error: " // trim(error_msg)
        error stop 1
    end if

    call emit_fortran(arena1, root1, first_output)
    write (output_unit, '(A)') "=== First pass output ==="
    write (output_unit, '(A)') first_output
    write (output_unit, '(A)') "========================="

    arena2 = create_ast_arena()
    call lex_source(first_output, tokens2, error_msg)
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        write (output_unit, '(A)') "FAIL: Second lex error: " // trim(error_msg)
        error stop 1
    end if

    call parse_tokens(tokens2, arena2, root2, error_msg)
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        write (output_unit, '(A)') "FAIL: Second parse failed (ROUND-TRIP BUG)"
        write (output_unit, '(A)') "Error: " // trim(error_msg)
        error stop 1
    end if

    call emit_fortran(arena2, root2, second_output)
    write (output_unit, '(A)') "PASS: Multiple DATA sets round-trip succeeded"

contains

    subroutine read_example(filepath, content)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: content
        integer :: unit_num, file_size, iostat_val
        character(len=1), allocatable :: buffer(:)

        open (newunit=unit_num, file=filepath, status='old', &
              action='read', form='unformatted', access='stream', &
              iostat=iostat_val)
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

end program test_issue_2415_multiple_sets
