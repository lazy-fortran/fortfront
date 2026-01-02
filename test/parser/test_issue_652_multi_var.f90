program test_issue_652_multi_var
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    all_passed = .true.

    if (.not. test_simple_multi_var()) all_passed = .false.
    if (.not. test_multi_var_with_init()) all_passed = .false.
    if (.not. test_multi_var_with_attributes()) all_passed = .false.

    if (all_passed) then
        print *, 'PASS: Issue #652 - multi-variable declarations preserved'
        stop 0
    else
        error stop 'FAIL: Multi-variable declarations not correctly parsed'
    end if

contains

    include '../common/cli_io_reader.inc'
    include '../common/read_example.inc'


    logical function test_simple_multi_var()
        character(len=:), allocatable :: input
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        test_simple_multi_var = .true.

        call read_example('examples/lf/issue_652_multi_var_simple.lf', input)
        call transform_lazy_fortran_string(input, output, error_msg)

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: Compilation error - ' // &
                trim(error_msg)
            test_simple_multi_var = .false.
            return
        end if

        if (.not. (index(output, 'x = 1') > 0 .and. index(output, 'y = 2') > 0 &
                   .and. index(output, 'z = 3') > 0)) then
            write (error_unit, '(A)') 'FAIL: Variable assignments not preserved'
            write (error_unit, '(A)') trim(output)
            test_simple_multi_var = .false.
        end if
    end function test_simple_multi_var

    logical function test_multi_var_with_init()
        character(len=:), allocatable :: input
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        test_multi_var_with_init = .true.

        call read_example('examples/lf/issue_652_multi_var_init.lf', input)
        call transform_lazy_fortran_string(input, output, error_msg)

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: Compilation error - ' // &
                trim(error_msg)
            test_multi_var_with_init = .false.
            return
        end if

        if (index(output, '3.14') == 0) then
            write (error_unit, '(A)') 'FAIL: Initializer not preserved'
            write (error_unit, '(A)') trim(output)
            test_multi_var_with_init = .false.
        end if
    end function test_multi_var_with_init

    logical function test_multi_var_with_attributes()
        character(len=:), allocatable :: input
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        test_multi_var_with_attributes = .true.

        call read_example('examples/lf/issue_652_multi_var_allocatable.lf', &
                          input)
        call transform_lazy_fortran_string(input, output, error_msg)

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: Compilation error - ' // &
                trim(error_msg)
            test_multi_var_with_attributes = .false.
            return
        end if

        if (index(output, 'allocatable') == 0 .or. index(output, 'arr1') == 0 .or. &
            index(output, 'arr2') == 0 .or. index(output, 'arr3') == 0) then
            write (error_unit, '(A)') 'FAIL: Attributes or variables not preserved'
            write (error_unit, '(A)') trim(output)
            test_multi_var_with_attributes = .false.
        end if
    end function test_multi_var_with_attributes

end program test_issue_652_multi_var
