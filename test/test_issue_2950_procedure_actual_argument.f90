program test_issue_2950_procedure_actual_argument
    ! Issue #2950: a bare identifier that names a procedure - an INTRINSIC
    ! declared name or a contained procedure - is a procedure reference when
    ! it appears as an actual argument, never an implicitly typed variable.
    ! Fabricating "real :: dcos" or "real :: expression" turns valid source
    ! into source gfortran rejects and misleads downstream consumers, which
    ! then resolve the name as an undeclared scalar.
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        & iostat_eor
    use string_utils_mod, only: to_lower
    use transformation_api, only: transform_context_t, transform_with_context, &
        & INPUT_MODE_STANDARD
    implicit none

    character(len=:), allocatable :: source_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    character(len=:), allocatable :: lower_output
    type(transform_context_t) :: ctx

    call read_example('examples/f90/issue_2950_procedure_actual_argument.f90', &
        & source_code)

    ctx%input_mode = INPUT_MODE_STANDARD
    ctx%has_filename = .true.
    ctx%source_name = 'issue_2950_procedure_actual_argument'

    call transform_with_context(source_code, output_code, error_msg, ctx)
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: transform_with_context error: '// &
            & trim(error_msg)
        error stop 1
    end if

    if (.not. allocated(output_code)) then
        write (error_unit, '(A)') 'FAIL: transform_with_context produced no output'
        error stop 1
    end if

    lower_output = to_lower(output_code)

    call assert_absent(lower_output, ':: dcos', &
        & 'FAIL: intrinsic dcos declared as a variable')
    call assert_absent(lower_output, ':: expression', &
        & 'FAIL: contained function expression declared as a variable')
    call assert_absent(lower_output, 'real :: f', &
        & 'FAIL: dummy declared by an interface body typed as a variable')
    call assert_contains(lower_output, 'intrinsic dcos', &
        & 'FAIL: intrinsic statement for dcos lost')
    call assert_contains(lower_output, 'call apply(expression)', &
        & 'FAIL: procedure actual argument lost')

    print *, 'PASS: Issue #2950 procedure actual arguments stay procedures'

contains

    include 'common/read_example.inc'

    subroutine assert_absent(text, pattern, failure_message)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: pattern
        character(len=*), intent(in) :: failure_message

        if (index(text, pattern) > 0) then
            write (error_unit, '(A)') trim(failure_message)
            error stop 1
        end if
    end subroutine assert_absent

    subroutine assert_contains(text, pattern, failure_message)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: pattern
        character(len=*), intent(in) :: failure_message

        if (index(text, pattern) == 0) then
            write (error_unit, '(A)') trim(failure_message)
            error stop 1
        end if
    end subroutine assert_contains

end program test_issue_2950_procedure_actual_argument
