program test_issue_2160_global_var_and_calls
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Issue #2160: Main program code completely deleted ==='

    if (.not. test_global_var_with_function()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'Issue #2160 fixed!'
    else
        print *, 'Issue #2160 regression detected!'
        stop 1
    end if

contains

    logical function test_global_var_with_function()
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg
        logical :: has_global_counter
        logical :: has_function_calls
        logical :: has_print_statements
        logical :: has_function_def

        test_global_var_with_function = .true.
        print *, 'Testing global var with function and calls...'

        call read_example('examples/lf/issue_2160_global_var_and_calls.lf', source)

        call transform_lazy_fortran_string(source, output, error_msg, enable_ast_wrapping=.true.)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Unexpected error -', trim(error_msg)
                test_global_var_with_function = .false.
                return
            end if
        end if

        if (.not. allocated(output)) then
            print *, '  FAIL: No output generated'
            test_global_var_with_function = .false.
            return
        end if

        ! DEBUG: Show actual output
        print *, '  DEBUG: Output length:', len(output)
        print *, '  DEBUG: First 500 chars:'
        if (len(output) > 500) then
            print *, output(1:500)
        else
            print *, output
        end if

        ! Check that the output contains the expected elements
        has_global_counter = index(output, 'integer :: counter') > 0
        has_function_calls = index(output, 'x = increment()') > 0 .and. &
                            index(output, 'y = increment()') > 0 .and. &
                            index(output, 'z = increment()') > 0
        has_print_statements = index(output, 'print *, "x, y, z:"') > 0 .and. &
                              index(output, 'print *, "counter:"') > 0
        has_function_def = index(output, 'function increment()') > 0

        if (.not. has_global_counter) then
            print *, '  FAIL: Global counter variable declaration missing'
            test_global_var_with_function = .false.
        end if

        if (.not. has_function_calls) then
            print *, '  FAIL: Function calls missing'
            test_global_var_with_function = .false.
        end if

        if (.not. has_print_statements) then
            print *, '  FAIL: Print statements missing'
            test_global_var_with_function = .false.
        end if

        if (.not. has_function_def) then
            print *, '  FAIL: Function definition missing'
            test_global_var_with_function = .false.
        end if

        if (test_global_var_with_function) then
            print *, '  PASS: All main program code preserved'
        end if
    end function test_global_var_with_function

    subroutine read_example(filepath, content)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: content
        integer :: unit_num, ios, file_size
        character(len=1), allocatable :: buffer(:)
        integer :: i
        character(len=1) :: ch

        open (newunit=unit_num, file=filepath, status='old', &
              action='read', access='stream', iostat=ios)
        if (ios /= 0) then
            print *, 'ERROR: Could not open file:', filepath
            stop 1
        end if

        inquire (unit=unit_num, size=file_size)
        if (file_size <= 0) file_size = 10000

        allocate (buffer(file_size))
        i = 0
        do
            if (i >= file_size) exit
            read (unit_num, iostat=ios) ch
            if (ios /= 0) exit
            i = i + 1
            buffer(i) = ch
        end do

        close (unit_num)

        if (i > 0) then
            allocate (character(len=i) :: content)
            do ios = 1, i
                content(ios:ios) = buffer(ios)
            end do
        else
            content = ''
        end if
    end subroutine read_example

end program test_issue_2160_global_var_and_calls
