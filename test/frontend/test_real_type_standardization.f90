program test_real_type_standardization
    use transformation_api, only: transform_lazy_fortran_string, &
        transform_with_context, transform_context_t, &
        INPUT_MODE_STANDARD
    implicit none

    logical :: ok

    ok = .true.
    call test_lazy_real_assignment_standardizes(ok)
    call test_standard_generic_preserves_real(ok)

    if (.not. ok) stop 1
    print *, 'PASS: real type standardization respects input mode'

contains

    subroutine test_lazy_real_assignment_standardizes(ok)
        logical, intent(inout) :: ok
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        call transform_lazy_fortran_string('x = 1.0', output, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, 'FAIL: lazy real assignment returned error: ', trim(error_msg)
            ok = .false.
            return
        end if

        if (index(output, 'use, intrinsic :: iso_fortran_env, only: dp => real64') == 0) then
            print *, 'FAIL: lazy real assignment did not import real64 kind'
            print *, trim(output)
            ok = .false.
        end if

        if (index(output, 'real(dp) :: x') == 0) then
            print *, 'FAIL: lazy real assignment did not standardize real kind'
            print *, trim(output)
            ok = .false.
        end if
    end subroutine test_lazy_real_assignment_standardizes

    subroutine test_standard_generic_preserves_real(ok)
        logical, intent(inout) :: ok
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg
        type(transform_context_t) :: context

        source = 'module m'//new_line('a')// &
            '    interface g'//new_line('a')// &
            '        module procedure f'//new_line('a')// &
            '    end interface g'//new_line('a')// &
            'contains'//new_line('a')// &
            '    real function f(x)'//new_line('a')// &
            '        real, intent(in) :: x'//new_line('a')// &
            '        f = x'//new_line('a')// &
            '    end function f'//new_line('a')// &
            'end module m'

        context%input_mode = INPUT_MODE_STANDARD

        call transform_with_context(source, output, error_msg, context)

        if (len_trim(error_msg) > 0) then
            print *, 'FAIL: standard generic returned error: ', trim(error_msg)
            ok = .false.
            return
        end if

        if (index(output, 'real function f(x)') == 0 .or. &
            index(output, 'real, intent(in) :: x') == 0) then
            print *, 'FAIL: standard generic real declarations were changed'
            print *, trim(output)
            ok = .false.
        end if
    end subroutine test_standard_generic_preserves_real

end program test_real_type_standardization
