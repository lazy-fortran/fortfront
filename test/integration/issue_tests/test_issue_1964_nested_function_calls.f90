program test_issue_1964_nested_function_calls
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    all_passed = .true.
    print *, '=== Issue #1964: Nested function call parameter inference ==='

    if (.not. check_nested_call_parameters()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'Issue #1964 fixed!'
    else
        print *, 'Issue #1964 regression detected!'
        stop 1
    end if

contains

    include '../../common/cli_io_reader.inc'

    subroutine read_example(path, content)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: content
        integer :: status

        call read_all_stdin_or_file(.true., path, content, status)
        if (status /= 0) then
            print *, 'FAIL: failed to read ', trim(path)
            error stop 1
        end if
    end subroutine read_example

    logical function check_nested_call_parameters()
        implicit none
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg
        integer :: pos_header
        logical :: has_integer_decl
        logical :: has_real_decl

        check_nested_call_parameters = .true.
        print *, 'Checking nested call argument inference...'

        call read_example('examples/lf/issue_1964_nested_function_calls.lf', &
                          source)
        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: unexpected error -', trim(error_msg)
                check_nested_call_parameters = .false.
                return
            end if
        end if

        if (.not. allocated(output)) then
            print *, '  FAIL: transformation produced no output'
            check_nested_call_parameters = .false.
            return
        end if

        pos_header = index(output, 'integer function multiply')
        if (pos_header <= 0) then
            print *, '  FAIL: multiply function not inferred as integer'
            print *, trim(output)
            check_nested_call_parameters = .false.
            return
        end if

        has_integer_decl = index(output, 'integer :: x') > 0
        if (.not. has_integer_decl) then
            has_integer_decl = index(output, 'integer, intent(in) :: x') > 0
        end if
        if (.not. has_integer_decl) then
            print *, '  FAIL: parameter x not declared as integer'
            print *, trim(output)
            check_nested_call_parameters = .false.
        end if

        has_real_decl = index(output, 'real :: x') > 0
        if (has_real_decl) then
            print *, '  FAIL: parameter x still declared as real'
            print *, trim(output)
            check_nested_call_parameters = .false.
        end if

        if (check_nested_call_parameters) then
            print *, '  PASS: nested call arguments retain integer type'
        end if
    end function check_nested_call_parameters

end program test_issue_1964_nested_function_calls
