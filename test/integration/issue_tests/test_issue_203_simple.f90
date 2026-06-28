program test_issue_203_simple
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Issue #203: Redundant dimension in allocatable declaration ==='

    if (.not. test_exact_issue_203_example()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'Issue #203 fixed!'
    else
        print *, 'Issue #203 test failed!'
        stop 1
    end if

contains

    logical function test_exact_issue_203_example()
        character(len=:), allocatable :: source, output, error_msg
        logical :: found_no_redundant_dimension

        test_exact_issue_203_example = .true.
        print *, 'Testing exact example from issue #203...'

        source = 'v = [10]' // new_line('a') // &
            'v = [v, v**2]' // new_line('a') // &
            'print*,v'

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Compilation error:', trim(error_msg)
                test_exact_issue_203_example = .false.
                return
            end if
        end if

        found_no_redundant_dimension = .true.

        if (index(output, 'dimension(1)') > 0 .or. &
            index(output, 'dimension(2)') > 0) then
            print *, '  FAIL: Found redundant dimension attribute'
            found_no_redundant_dimension = .false.
        end if

        if (index(output, 'allocatable') > 0 .and. index(output, 'v') > 0) then
            print *, '  Generated declaration contains allocatable v'
            if (index(output, 'v(:)') == 0) then
                print *, '  WARNING: Missing deferred shape (:) on allocatable'
            end if
        end if

        if (found_no_redundant_dimension) then
            print *, '  PASS: No redundant dimension attribute found'
        else
            print *, '  FAIL: Redundant dimension attribute still present'
            test_exact_issue_203_example = .false.
        end if

    end function test_exact_issue_203_example

end program test_issue_203_simple
