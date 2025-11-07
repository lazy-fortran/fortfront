program test_issue_2145_intrinsic_return_type
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
                                             iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: errors
    logical :: has_len_trim_decl
    logical :: has_len_decl

    print *, "=== Testing Issue #2145: len_trim return type inference ==="

    call read_example('examples/lf/issue_2145_intrinsic_return_type.lf', source)
    call transform_lazy_fortran_string(source, output, errors)

    has_len_trim_decl = has_integer_declaration(output, 'len_msg')
    has_len_decl = has_integer_declaration(output, 'len_name')

    if (has_len_trim_decl .and. has_len_decl .and. len_trim(errors) == 0) then
        print *, "  PASS: intrinsic len/len_trim results declared as integer"
    else
        print *, "  FAIL: intrinsic declaration incorrect"
        print *, "Output:"
        print *, trim(output)
        if (len_trim(errors) > 0) then
            print *, "Errors:"
            print *, trim(errors)
        end if
        error stop 1
    end if

contains

    include 'common/cli_io_reader.inc'

    subroutine read_example(path, content)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: content
        integer :: status

        call read_all_stdin_or_file(.true., path, content, status)
        if (status /= 0) then
            write (error_unit, '(A)') 'FAIL: failed to read ' // trim(path)
            error stop 1
        end if
    end subroutine read_example

    logical function has_integer_declaration(buffer, var_name)
        character(len=*), intent(in) :: buffer
        character(len=*), intent(in) :: var_name
        integer :: search_pos
        integer :: decl_pos
        integer :: line_end
        character(len=:), allocatable :: line
        character :: nl

        has_integer_declaration = .false.
        if (len(buffer) <= 0) return
        if (len_trim(var_name) == 0) return

        nl = new_line('a')
        search_pos = 1

        do
            if (search_pos > len(buffer)) exit
            decl_pos = index(buffer(search_pos:), 'integer ::')
            if (decl_pos <= 0) exit
            decl_pos = decl_pos + search_pos - 1

            line_end = decl_pos
            do while (line_end <= len(buffer))
                if (buffer(line_end:line_end) == nl) exit
                line_end = line_end + 1
            end do

            if (line_end > len(buffer)) then
                line = buffer(decl_pos:)
            else
                line = buffer(decl_pos:line_end-1)
            end if

            if (index(line, trim(var_name)) > 0) then
                has_integer_declaration = .true.
                return
            end if

            search_pos = line_end + 1
        end do
    end function has_integer_declaration

end program test_issue_2145_intrinsic_return_type
