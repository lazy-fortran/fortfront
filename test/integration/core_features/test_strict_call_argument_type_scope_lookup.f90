program test_strict_call_argument_type_scope_lookup
    use, intrinsic :: iso_fortran_env, only: &
        error_unit, input_unit, iostat_end, iostat_eor
    use transformation_api, only: transform_context_t, transform_with_context, &
        INPUT_MODE_STANDARD, OPERATING_MODE_STRICT
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    type(transform_context_t) :: ctx

    ctx%input_mode = INPUT_MODE_STANDARD
    ctx%has_filename = .false.
    ctx%operating_mode = OPERATING_MODE_STRICT

    call read_example( &
        'examples/f90/issue_2644_strict_arg_checker_scope_lookup.f90', source)

    call transform_with_context(source, output, error_msg, ctx)
    call assert_no_error(error_msg)

    print *, 'PASS: strict arg checker uses scoped interface lookup'

contains

    include '../../common/read_example.inc'


    subroutine assert_no_error(msg)
        character(len=:), allocatable, intent(in) :: msg

        if (.not. allocated(msg)) return
        if (len_trim(msg) == 0) return
        write (error_unit, '(A)') 'FAIL: unexpected error message:'
        write (error_unit, '(A)') trim(msg)
        error stop 1
    end subroutine assert_no_error

end program test_strict_call_argument_type_scope_lookup
