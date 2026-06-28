program test_issue_2819_implied_do_locality
    ! End-to-end: implied-DO index locality is enforced through the transform
    ! pipeline. A valid nested implied-DO transforms cleanly; an out-of-scope
    ! index reference and a shadowing nested index are reported as errors.
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_with_context, transform_context_t, &
        INPUT_MODE_STANDARD
    implicit none

    logical :: all_passed

    all_passed = .true.

    call check_valid(all_passed)
    call check_rejected('examples/f90/issue_2819_implied_do_index_out_of_scope.f90', &
        'out-of-scope index reference', all_passed)
    call check_rejected('examples/f90/issue_2819_implied_do_index_shadow.f90', &
        'shadowing nested index', all_passed)

    if (all_passed) then
        write (*, '(a)') 'PASS: implied-DO index locality enforced'
    else
        stop 1
    end if

contains

    subroutine check_valid(passed)
        logical, intent(inout) :: passed
        type(transform_context_t) :: context
        character(len=:), allocatable :: source, transformed, error_msg

        call read_example( &
            'examples/f90/issue_2819_implied_do_index_locality_valid.f90', source)
        context%input_mode = INPUT_MODE_STANDARD
        call transform_with_context(source, transformed, error_msg, context)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                write (error_unit, '(a)') 'FAIL: valid implied-DO rejected: '// &
                    trim(error_msg)
                passed = .false.
            end if
        end if
    end subroutine check_valid

    subroutine check_rejected(path, label, passed)
        character(len=*), intent(in) :: path
        character(len=*), intent(in) :: label
        logical, intent(inout) :: passed
        type(transform_context_t) :: context
        character(len=:), allocatable :: source, transformed, error_msg

        call read_example(path, source)
        context%input_mode = INPUT_MODE_STANDARD
        call transform_with_context(source, transformed, error_msg, context)

        if (.not. error_reported(error_msg)) then
            write (error_unit, '(a)') 'FAIL: '//label//' not rejected'
            passed = .false.
        end if
    end subroutine check_rejected

    function error_reported(error_msg) result(reported)
        character(len=:), allocatable, intent(in) :: error_msg
        logical :: reported

        reported = .false.
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) reported = .true.
        end if
    end function error_reported

    include '../../common/read_example.inc'
end program test_issue_2819_implied_do_locality
