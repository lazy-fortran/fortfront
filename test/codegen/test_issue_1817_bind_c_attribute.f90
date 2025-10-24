program test_issue_1817_bind_c_attribute
    use transformation_api, only: compile_source, compilation_options_t

    character(len=:), allocatable :: input_file, output_file
    character(len=256) :: error_msg
    type(compilation_options_t) :: options
    integer :: unit, io_status
    logical :: found_bind_c_subroutine, found_bind_c_function
    character(len=256) :: line

    found_bind_c_subroutine = .false.
    found_bind_c_function = .false.

    input_file = 'test_issue_1817_input.f90'
    output_file = 'test_issue_1817_output.f90'

    open (newunit=unit, file=input_file, status='replace')
    write (unit, '(a)') 'program test_bind_c'
    write (unit, '(a)') '    use iso_c_binding'
    write (unit, '(a)') '    implicit none'
    write (unit, '(a)') '    '
    write (unit, '(a)') '    interface'
    write (unit, '(a)') '        subroutine c_func(x) bind(c, name="c_func")'
    write (unit, '(a)') '            import :: c_int'
    write (unit, '(a)') '            integer(c_int), value :: x'
    write (unit, '(a)') '        end subroutine c_func'
    write (unit, '(a)') '        '
    write (unit, '(a)') '        function c_add(a, b) result(c) bind(c)'
    write (unit, '(a)') '            import :: c_int'
    write (unit, '(a)') '            integer(c_int), value :: a, b'
    write (unit, '(a)') '            integer(c_int) :: c'
    write (unit, '(a)') '        end function c_add'
    write (unit, '(a)') '    end interface'
    write (unit, '(a)') '    '
    write (unit, '(a)') '    print *, "Testing bind(c)"'
    write (unit, '(a)') 'end program test_bind_c'
    close (unit)

    options%output_file = output_file

    call compile_source(input_file, options, error_msg)
    if (len_trim(error_msg) /= 0) then
        print *, 'Compiler reported error: ', trim(error_msg)
        stop 1
    end if

    open (newunit=unit, file=output_file, status='old', action='read')
    do
        read (unit, '(a)', iostat=io_status) line
        if (io_status /= 0) exit

        if (index(line, 'bind(c') > 0 .and. index(line, 'name="c_func"') > 0 .and. &
            index(line, 'subroutine c_func') > 0) then
            found_bind_c_subroutine = .true.
        end if
        if (index(line, 'bind(c)') > 0 .and. index(line, 'function c_add') > 0) then
            found_bind_c_function = .true.
        end if
    end do
    close (unit)

    if (.not. found_bind_c_subroutine) then
        print *, 'FAIL: bind(c, name=...) not found in subroutine definition'
        stop 1
    end if

    if (.not. found_bind_c_function) then
        print *, 'FAIL: bind(c) not found in function definition'
        stop 1
    end if

    print *, 'PASS: bind(c) attributes preserved correctly'
    stop 0
end program test_issue_1817_bind_c_attribute
