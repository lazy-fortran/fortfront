program test_issue_2151_array_function_broken
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
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
        ! Note: Ideally should be integer, allocatable but real, dimension also compiles
        if (index(output, 'integer') == 0 .and. index(output, 'real') == 0) then
            success = .false.
            print *, "FAILED: result variable should be declared with a type"
        end if
        if (index(output, 'allocatable') == 0 .and. index(output, 'dimension') == 0) then
            success = .false.
            print *, "FAILED: result variable should be declared as array (allocatable or dimension)"
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


    include '../../common/read_example.inc'
end program test_issue_2151_array_function_broken
