program test_boolean_literal_type_inference
    use frontend, only: transform_lazy_fortran_string
    implicit none

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

    function test_dot_true_infers_logical() result(passed)
        logical :: passed
        character(len=:), allocatable :: out

        out = compile_and_generate('flag = .true.')
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

        out = compile_and_generate('flag = true')
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

    function compile_and_generate(source_line) result(output)
        character(len=*), intent(in) :: source_line
        character(len=:), allocatable :: output

        character(len=:), allocatable :: source, error_msg
        source = source_line
        call transform_lazy_fortran_string(source, output, error_msg)
        if (.not. allocated(output)) output = ''
    end function compile_and_generate

end program test_boolean_literal_type_inference
