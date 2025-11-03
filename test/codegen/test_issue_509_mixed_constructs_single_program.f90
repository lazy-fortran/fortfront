program test_issue_509_mixed_constructs_single_program
    use lexer_api, only: lex_source, lex_file
    use parser_api, only: parse_tokens, parse_tokens_safe
    use semantic_api, only: analyze_semantics
    use codegen_api, only: emit_fortran
    use transformation_api, only: transform_lazy_fortran_string, compile_source
    implicit none

    character(len=:), allocatable :: test_code
    character(len=:), allocatable :: result_code
    character(len=:), allocatable :: expected_code
    character(len=:), allocatable :: error_msg

    ! Test code from Issue #509
    call read_example('examples/lf/issue_509_mixed_constructs_input.lf', test_code)

    ! Expected output from Issue #509
    call read_example('examples/f90/issue_509_mixed_constructs_expected.f90', expected_code)

    print *, "Test Issue #509: subroutine and function indentation should be consistent"
    print *, "================================================================"

    ! Transform the test code
    call transform_lazy_fortran_string(test_code, result_code, error_msg)

    if (len_trim(error_msg) > 0) then
        print *, "ERROR: ", error_msg
        stop 1
    end if

    print *, ""
    print *, "Input Code:"
    print *, "----------"
    call print_with_line_numbers(test_code)

    print *, ""
    print *, "Generated Code:"
    print *, "--------------"
    call print_with_line_numbers(result_code)

    print *, ""
    print *, "Expected Code:"
    print *, "-------------"
    call print_with_line_numbers(expected_code)

    print *, ""
    print *, "Analysis:"
    print *, "--------"
    print *, "Current issue: subroutine/function declarations and end statements"
    print *, "should have matching indentation levels"

contains

    subroutine print_with_line_numbers(code)
        character(len=*), intent(in) :: code
        integer :: i, line_num, start_pos
        character(len=:), allocatable :: line

        line_num = 1
        start_pos = 1

        do i = 1, len(code)
            if (code(i:i) == new_line('a')) then
                if (i >= start_pos) then
                    line = code(start_pos:i - 1)
                    write (*, '(I3,A,A)') line_num, ': ', line
                else
                    write (*, '(I3,A)') line_num, ': '
                end if
                line_num = line_num + 1
                start_pos = i + 1
            end if
        end do

        ! Handle last line if no trailing newline
        if (start_pos <= len(code)) then
            line = code(start_pos:)
            write (*, '(I3,A,A)') line_num, ': ', line
        end if
    end subroutine print_with_line_numbers

    subroutine read_example(filepath, content)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: content
        integer :: unit, stat, file_size
        character(len=1), allocatable :: buffer(:)

        open (newunit=unit, file=filepath, status='old', access='stream', &
              form='unformatted', iostat=stat)
        if (stat /= 0) then
            print *, "ERROR: Cannot open file:", trim(filepath)
            error stop 1
        end if

        inquire (unit=unit, size=file_size)
        allocate (character(len=file_size) :: content)
        allocate (buffer(file_size))

        read (unit, iostat=stat) buffer
        close (unit)

        if (stat /= 0) then
            print *, "ERROR: Cannot read file:", trim(filepath)
            error stop 1
        end if

        content = transfer(buffer, content)
    end subroutine read_example

end program test_issue_509_mixed_constructs_single_program
