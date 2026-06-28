program test_strict_allocatable_lhs_reallocation
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_context_t, &
        transform_with_context, &
        INPUT_MODE_LAZY, &
        OPERATING_MODE_INFER, &
        OPERATING_MODE_STRICT
    implicit none

    character(len=:), allocatable :: error_msg
    character(len=:), allocatable :: output
    character(len=:), allocatable :: source
    type(transform_context_t) :: ctx

    call read_example('examples/lf/issue_2594_allocatable_lhs_realloc.lf', source)

    ctx%input_mode = INPUT_MODE_LAZY
    ctx%has_filename = .false.

    ctx%operating_mode = OPERATING_MODE_INFER
    call transform_with_context(source, output, error_msg, ctx)
    call assert_no_error(error_msg)

    ctx%operating_mode = OPERATING_MODE_STRICT
    call transform_with_context(source, output, error_msg, ctx)
    call assert_contains(error_msg, 'forbids automatic allocatable array '// &
        'reallocation')

    print *, 'PASS: strict mode forbids allocatable lhs reallocation'

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

    subroutine assert_contains(msg, needle)
        character(len=:), allocatable, intent(in) :: msg
        character(len=*), intent(in) :: needle

        if (.not. allocated(msg)) then
            write (error_unit, '(A)') 'FAIL: expected error message'
            error stop 1
        end if
        if (index(msg, needle) == 0) then
            write (error_unit, '(A)') 'FAIL: expected message to contain: ' // &
                trim(needle)
            write (error_unit, '(A)') 'Got:'
            write (error_unit, '(A)') trim(msg)
            error stop 1
        end if
    end subroutine assert_contains

end program test_strict_allocatable_lhs_reallocation
