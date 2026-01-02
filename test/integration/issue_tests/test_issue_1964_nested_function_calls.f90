program test_issue_1964_nested_function_calls
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    all_passed = check_nested_call_parameters()

    if (all_passed) then
        print *, 'PASS: Issue #1964 - nested call inference stable'
    else
        error stop 'FAIL: Issue #1964 regression detected'
    end if

contains

    include '../../common/cli_io_reader.inc'
    include '../../common/read_example.inc'


    logical function check_nested_call_parameters()
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg
        integer :: pos_header
        logical :: has_integer_decl
        logical :: has_real_decl

        check_nested_call_parameters = .true.

        call read_example('examples/lf/issue_1964_nested_function_calls.lf', &
                          source)
        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                write (error_unit, '(A)') &
                    'FAIL: unexpected error - ' // trim(error_msg)
                check_nested_call_parameters = .false.
                return
            end if
        end if

        if (.not. allocated(output)) then
            write (error_unit, '(A)') 'FAIL: transformation produced no output'
            check_nested_call_parameters = .false.
            return
        end if

        pos_header = index(output, 'integer function multiply')
        if (pos_header <= 0) then
            write (error_unit, '(A)') &
                'FAIL: multiply function not inferred as integer'
            write (error_unit, '(A)') trim(output)
            check_nested_call_parameters = .false.
            return
        end if

        has_integer_decl = index(output, 'integer :: x') > 0
        if (.not. has_integer_decl) then
            has_integer_decl = index(output, 'integer, intent(in) :: x') > 0
        end if
        if (.not. has_integer_decl) then
            write (error_unit, '(A)') 'FAIL: parameter x not declared as integer'
            write (error_unit, '(A)') trim(output)
            check_nested_call_parameters = .false.
        end if

        has_real_decl = index(output, 'real :: x') > 0
        if (has_real_decl) then
            write (error_unit, '(A)') 'FAIL: parameter x still declared as real'
            write (error_unit, '(A)') trim(output)
            check_nested_call_parameters = .false.
        end if
    end function check_nested_call_parameters

end program test_issue_1964_nested_function_calls
