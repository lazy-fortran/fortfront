program test_issue_2151_array_function_broken
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    logical :: success

    print *, "=== Issue #2151: Array-valued function type handling ==="

    call read_example('examples/lf/issue_2151_array_function_broken.lf', source)
    call transform_lazy_fortran_string(source, output, error_msg)

    success = .true.
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            success = .false.
            print *, "ERRORS:"
            print *, trim(error_msg)
        end if
    end if

    if (.not. allocated(output)) then
        success = .false.
        print *, "FAILED: output not allocated"
        stop 1
    end if

    if (success) then
        ! Check that function has result clause
        if (index(output, 'result(') == 0) then
            success = .false.
            print *, "FAILED: function should have result clause"
        end if

        ! Check that result variable is assigned, not function name
        if (index(output, 'make_array_result =') == 0) then
            success = .false.
            print *, "FAILED: should assign to result variable make_array_result"
        end if

        ! Check that function signature doesn't have type prefix
        ! (type should be in result variable declaration)
        if (index(output, 'function make_array(') == 0) then
            success = .false.
            print *, "FAILED: function signature should be untyped"
        end if

        ! Check that result variable is declared with proper type
        if (index(output, 'integer') == 0 .or. index(output, 'allocatable') == 0) then
            success = .false.
            print *, "FAILED: result variable should be declared as integer, allocatable"
        end if

        ! Check that caller variable is declared
        if (index(output, ':: arr') == 0) then
            success = .false.
            print *, "FAILED: caller variable arr should be declared"
        end if
    end if

    if (success) then
        print *, 'PASSED'
    else
        print *, 'FAILED'
        if (allocated(output)) then
            print *, 'OUTPUT:'
            print *, trim(output)
        end if
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
            write (error_unit, '(A)') 'FAIL: failed to read ' // trim(path)
            error stop 1
        end if
    end subroutine read_example

end program test_issue_2151_array_function_broken
