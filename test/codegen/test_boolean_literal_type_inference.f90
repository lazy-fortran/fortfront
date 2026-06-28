program test_boolean_literal_type_inference
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        iostat_eor
    use transformation_api, only: transform_lazy_fortran_string

    logical :: all_passed
    all_passed = .true.

    print *, '=== Testing boolean literal type inference ==='

    if (.not. test_dot_true_infers_logical()) all_passed = .false.
    if (.not. test_bare_true_emits_fortran_and_infers_logical()) all_passed = .false.
    if (.not. test_dot_false_infers_logical()) all_passed = .false.
    if (.not. test_bare_false_emits_fortran_and_infers_logical()) all_passed = .false.

    if (all_passed) then
        print *, 'All boolean literal inference tests passed!'
        stop 0
    else
        print *, 'Some boolean literal inference tests failed!'
        stop 1
    end if

contains

    include '../common/read_example.inc'

    function test_dot_true_infers_logical() result(passed)
        logical :: passed
        character(len=:), allocatable :: out

        out = compile_example('examples/lf/boolean_assign_dot_true.lf')
        print *, 'OUTPUT(.true.):', trim(out)

        passed = .false.
        if (index(out, 'logical :: flag') == 0) then
            print *, 'FAIL: missing logical declaration for .true. assignment'
            return
        end if
        if (index(out, 'flag = .true.') == 0) then
            print *, 'FAIL: missing .true. assignment in output'
            return
        end if
        if (index(out, '!ERROR:') > 0) then
            print *, 'FAIL: unexpected ERROR comments in output'
            return
        end if

        print *, 'PASS: .true. infers logical and emits correctly'
        passed = .true.
    end function test_dot_true_infers_logical

    function test_bare_true_emits_fortran_and_infers_logical() result(passed)
        logical :: passed
        character(len=:), allocatable :: out

        out = compile_example('examples/lf/boolean_assign_bare_true.lf')
        print *, 'OUTPUT(true):', trim(out)

        passed = .false.
        if (index(out, 'logical :: flag') == 0) then
            print *, 'FAIL: missing logical declaration for bare true assignment'
            return
        end if
        if (index(out, 'flag = .true.') == 0) then
            print *, 'FAIL: bare true was not converted to .true.'
            return
        end if
        if (index(out, '!ERROR:') > 0) then
            print *, 'FAIL: unexpected ERROR comments in output'
            return
        end if

        print *, 'PASS: bare true infers logical and emits .true.'
        passed = .true.
    end function test_bare_true_emits_fortran_and_infers_logical

    function test_dot_false_infers_logical() result(passed)
        logical :: passed
        character(len=:), allocatable :: out

        out = compile_example('examples/lf/boolean_assign_dot_false.lf')
        print *, 'OUTPUT(.false.):', trim(out)

        passed = .false.
        if (index(out, 'logical :: flag') == 0) then
            print *, 'FAIL: missing logical declaration for .false. assignment'
            return
        end if
        if (index(out, 'flag = .false.') == 0) then
            print *, 'FAIL: missing .false. assignment in output'
            return
        end if
        if (index(out, '!ERROR:') > 0) then
            print *, 'FAIL: unexpected ERROR comments in output'
            return
        end if

        print *, 'PASS: .false. infers logical and emits correctly'
        passed = .true.
    end function test_dot_false_infers_logical

    function test_bare_false_emits_fortran_and_infers_logical() result(passed)
        logical :: passed
        character(len=:), allocatable :: out

        out = compile_example('examples/lf/boolean_assign_bare_false.lf')
        print *, 'OUTPUT(false):', trim(out)

        passed = .false.
        if (index(out, 'logical :: flag') == 0) then
            print *, 'FAIL: missing logical declaration for bare false assignment'
            return
        end if
        if (index(out, 'flag = .false.') == 0) then
            print *, 'FAIL: bare false was not converted to .false.'
            return
        end if
        if (index(out, '!ERROR:') > 0) then
            print *, 'FAIL: unexpected ERROR comments in output'
            return
        end if

        print *, 'PASS: bare false infers logical and emits .false.'
        passed = .true.
    end function test_bare_false_emits_fortran_and_infers_logical

    function compile_example(path) result(output)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: output
        character(len=:), allocatable :: source
        character(len=:), allocatable :: error_msg

        call read_example(path, source)
        call transform_lazy_fortran_string(source, output, error_msg)
        if (.not. allocated(output)) output = ''
    end function compile_example


end program test_boolean_literal_type_inference
