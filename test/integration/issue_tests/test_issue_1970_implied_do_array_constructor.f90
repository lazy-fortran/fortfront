program test_issue_1970_implied_do_array_constructor
    use, intrinsic :: iso_fortran_env, only: error_unit
    use, intrinsic :: iso_fortran_env, only: input_unit, iostat_end, iostat_eor
    use transformation_api, only: transform_with_context, transform_context_t, &
        INPUT_MODE_STANDARD
    implicit none

    type(transform_context_t) :: context
    character(len=:), allocatable :: source
    character(len=:), allocatable :: transformed
    character(len=:), allocatable :: error_msg
    logical :: has_real_decl

    call read_example( &
        'examples/f90/issue_1970_implied_do_array_constructor.f90', source)

    context%input_mode = INPUT_MODE_STANDARD
    call transform_with_context(source, transformed, error_msg, context)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            write (error_unit, '(a)') 'FAIL: unexpected error: ' // &
                trim(error_msg)
            stop 1
        end if
    end if

    has_real_decl = index(transformed, 'real :: halves(5)') > 0
    if (.not. has_real_decl) then
        write (error_unit, '(a)') 'FAIL: real declaration missing'
        write (error_unit, '(a)') transformed
        stop 1
    end if

    if (index(transformed, 'integer :: halves(5)') > 0) then
        write (error_unit, '(a)') 'FAIL: integer declaration still present'
        write (error_unit, '(a)') transformed
        stop 1
    end if

    write (error_unit, '(a)') &
        'PASS: implied-do array constructor infers real element type'


contains


    include '../../common/read_example.inc'
end program test_issue_1970_implied_do_array_constructor
