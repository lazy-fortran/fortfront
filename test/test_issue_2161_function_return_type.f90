program test_issue_2161_function_return_type
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: errors
    logical :: header_ok
    logical :: result1_ok
    logical :: result2_ok

    print *, "=== Testing Issue #2161: function return type inference ==="

    call read_example('examples/lf/issue_playtest5_function_no_return_type.lf', source)
    call transform_lazy_fortran_string(source, output, errors)

    header_ok = has_real_function_header(output, 'compute')
    result1_ok = has_real_declaration(output, 'result1')
    result2_ok = has_real_declaration(output, 'result2')

    if (header_ok .and. result1_ok .and. result2_ok .and. len_trim(errors) == 0) then
        print *, "  PASS: compute return type inferred as real"
    else
        print *, "  FAIL: compute return type inference incorrect"
        print *, "Output:"
        print *, trim(output)
        if (len_trim(errors) > 0) then
            print *, "Errors:"
            print *, trim(errors)
        end if
        error stop 1
    end if

contains

    include 'common/read_example.inc'


    logical function has_real_function_header(buffer, func_name)
        character(len=*), intent(in) :: buffer
        character(len=*), intent(in) :: func_name
        integer :: header_pos
        integer :: line_start
        integer :: line_end
        character(len=:), allocatable :: line
        character :: nl

        has_real_function_header = .false.
        if (len(buffer) <= 0) return
        if (len_trim(func_name) == 0) return

        nl = new_line('a')
        header_pos = index(buffer, 'function ' // trim(func_name))
        if (header_pos <= 0) return

        line_start = header_pos
        do while (line_start > 1)
            if (buffer(line_start-1:line_start-1) == nl) exit
            line_start = line_start - 1
        end do

        line_end = header_pos
        do while (line_end <= len(buffer))
            if (buffer(line_end:line_end) == nl) exit
            line_end = line_end + 1
        end do

        line = buffer(line_start:line_end-1)
        if (index(line, 'real') > 0) has_real_function_header = .true.
    end function has_real_function_header

    logical function has_real_declaration(buffer, var_name)
        character(len=*), intent(in) :: buffer
        character(len=*), intent(in) :: var_name
        integer :: search_pos
        integer :: name_pos
        integer :: line_start
        integer :: line_end
        character(len=:), allocatable :: line
        character :: nl

        has_real_declaration = .false.
        if (len(buffer) <= 0) return
        if (len_trim(var_name) == 0) return

        nl = new_line('a')
        search_pos = 1

        do
            if (search_pos > len(buffer)) exit
            name_pos = index(buffer(search_pos:), trim(var_name))
            if (name_pos <= 0) exit
            name_pos = name_pos + search_pos - 1

            line_start = name_pos
            do while (line_start > 1)
                if (buffer(line_start-1:line_start-1) == nl) exit
                line_start = line_start - 1
            end do

            line_end = name_pos
            do while (line_end <= len(buffer))
                if (buffer(line_end:line_end) == nl) exit
                line_end = line_end + 1
            end do

            line = buffer(line_start:line_end-1)
            if (index(line, '::') > 0) then
                if (index(line, 'real') > 0) then
                    has_real_declaration = .true.
                    return
                end if
            end if

            search_pos = line_end + 1
        end do
    end function has_real_declaration

end program test_issue_2161_function_return_type
