program test_issue_1570_pure_elemental
    use frontend, only: compile_source, compilation_options_t
    implicit none

    character(len=:), allocatable :: input_file, output_file
    character(len=256) :: error_msg
    type(compilation_options_t) :: options
    integer :: unit, io_status
    logical :: found_pure_function, found_elemental_function
    logical :: found_pure_subroutine, found_elemental_subroutine
    logical :: found_recursive_pure
    character(len=256) :: line

    found_pure_function = .false.
    found_elemental_function = .false.
    found_pure_subroutine = .false.
    found_elemental_subroutine = .false.
    found_recursive_pure = .false.

    input_file = 'test_issue_1570_input.f90'
    output_file = 'test_issue_1570_output.f90'

    open (newunit=unit, file=input_file, status='replace')
    write (unit, '(a)') 'module test_pure_elemental_module'
    write (unit, '(a)') '    implicit none'
    write (unit, '(a)') 'contains'
    write (unit, '(a)') '    pure function square(x) result(y)'
    write (unit, '(a)') '        integer, intent(in) :: x'
    write (unit, '(a)') '        integer :: y'
    write (unit, '(a)') '        y = x * x'
    write (unit, '(a)') '    end function square'
    write (unit, '(a)') ''
    write (unit, '(a)') '    elemental function add_one(x) result(y)'
    write (unit, '(a)') '        integer, intent(in) :: x'
    write (unit, '(a)') '        integer :: y'
    write (unit, '(a)') '        y = x + 1'
    write (unit, '(a)') '    end function add_one'
    write (unit, '(a)') ''
    write (unit, '(a)') '    pure subroutine compute_sum(a, b, c)'
    write (unit, '(a)') '        integer, intent(in) :: a, b'
    write (unit, '(a)') '        integer, intent(out) :: c'
    write (unit, '(a)') '        c = a + b'
    write (unit, '(a)') '    end subroutine compute_sum'
    write (unit, '(a)') ''
    write (unit, '(a)') '    elemental subroutine double_value(x)'
    write (unit, '(a)') '        integer, intent(inout) :: x'
    write (unit, '(a)') '        x = x * 2'
    write (unit, '(a)') '    end subroutine double_value'
    write (unit, '(a)') ''
    write (unit, '(a)') '    recursive pure function factorial(n) result(f)'
    write (unit, '(a)') '        integer, intent(in) :: n'
    write (unit, '(a)') '        integer :: f'
    write (unit, '(a)') '        if (n <= 1) then'
    write (unit, '(a)') '            f = 1'
    write (unit, '(a)') '        else'
    write (unit, '(a)') '            f = n * factorial(n - 1)'
    write (unit, '(a)') '        end if'
    write (unit, '(a)') '    end function factorial'
    write (unit, '(a)') 'end module test_pure_elemental_module'
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

        if (index(line, 'pure') > 0 .and. index(line, 'function square') > 0) then
            found_pure_function = .true.
        end if
        if (index(line, 'elemental') > 0 .and. &
            index(line, 'function add_one') > 0) then
            found_elemental_function = .true.
        end if
        if (index(line, 'pure') > 0 .and. &
            index(line, 'subroutine compute_sum') > 0) then
            found_pure_subroutine = .true.
        end if
        if (index(line, 'elemental') > 0 .and. &
            index(line, 'subroutine double_value') > 0) then
            found_elemental_subroutine = .true.
        end if
        if (index(line, 'recursive') > 0 .and. index(line, 'pure') > 0 .and. &
            index(line, 'function factorial') > 0) then
            found_recursive_pure = .true.
        end if
    end do
    close (unit)

    if (.not. found_pure_function) then
        print *, 'FAIL: pure keyword not found in function definition'
        stop 1
    end if

    if (.not. found_elemental_function) then
        print *, 'FAIL: elemental keyword not found in function definition'
        stop 1
    end if

    if (.not. found_pure_subroutine) then
        print *, 'FAIL: pure keyword not found in subroutine definition'
        stop 1
    end if

    if (.not. found_elemental_subroutine) then
        print *, 'FAIL: elemental keyword not found in subroutine definition'
        stop 1
    end if

    if (.not. found_recursive_pure) then
        print *, 'FAIL: recursive pure keywords not found in function definition'
        stop 1
    end if

    print *, 'PASS: All PURE/ELEMENTAL keywords preserved correctly'
    stop 0
end program test_issue_1570_pure_elemental
