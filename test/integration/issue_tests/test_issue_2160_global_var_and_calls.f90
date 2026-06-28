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

    include '../../common/read_example.inc'

    logical function test_global_var_with_function()
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg
        logical :: has_global_counter
        logical :: has_function_calls
        logical :: has_print_statements
        logical :: has_function_def
        integer :: program_count

        test_global_var_with_function = .true.
            print *, 'Testing global var with function and calls...'

            call read_example('examples/lf/issue_2160_global_var_and_calls.lf', source)

            ! Don't use AST wrapping - default path preserves all code
            call transform_lazy_fortran_string(source, output, error_msg)

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

                    ! Check that the output contains the expected elements
                    ! For now, we accept the function as external (TODO: make it contained)
                    has_global_counter = index(output, 'integer :: counter') > 0
                    has_function_calls = index(output, 'x = increment()') > 0 .and. &
                        index(output, 'y = increment()') > 0 .and. &
                        index(output, 'z = increment()') > 0
                    has_print_statements = index(output, 'print *, "x, y, z:"') > 0 .and. &
                        index(output, 'print *, "counter:"') > 0
                    has_function_def = index(output, 'function increment()') > 0 .or. &
                        index(output, 'integer function increment()') > 0

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

                                    program_count = count_program_main_wrappers(output)
                                    if (program_count /= 1) then
                                        print *, '  FAIL: Expected exactly one program main wrapper'
                                        test_global_var_with_function = .false.
                                        end if

                                        if (index(output, 'contains') == 0) then
                                            print *, '  FAIL: Contains block missing in transformed output'
                                            test_global_var_with_function = .false.
                                            end if

                                            if (test_global_var_with_function) then
                                                print *, '  PASS: All main program code preserved'
                                            end if
                                        end function test_global_var_with_function


                                        integer function count_program_main_wrappers(text)
                                            character(len=*), intent(in) :: text
                                            integer :: start_pos, newline_pos
                                            character(len=:), allocatable :: line
                                            character(len=*), parameter :: prefix = 'program main'

                                            count_program_main_wrappers = 0
                                            start_pos = 1

                                            do
                                                if (start_pos > len(text)) exit
                                                newline_pos = index(text(start_pos:), new_line('a'))
                                                if (newline_pos == 0) then
                                                    line = text(start_pos:)
                                                    start_pos = len(text) + 1
                                                else
                                                    line = text(start_pos:start_pos + newline_pos - 2)
                                                    start_pos = start_pos + newline_pos
                                                end if

                                                line = adjustl(line)
                                                if (len(line) >= len(prefix)) then
                                                    if (line(1:len(prefix)) == prefix) then
                                                        count_program_main_wrappers = count_program_main_wrappers + 1
                                                    end if
                                                end if
                                            end do
                                        end function count_program_main_wrappers

                                    end program test_issue_2160_global_var_and_calls
