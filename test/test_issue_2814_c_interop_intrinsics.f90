program test_issue_2814_c_interop_intrinsics
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        & iostat_eor
    use frontend_transformation, only: INPUT_MODE_STANDARD
    use transformation_api, only: transform_with_context, transform_context_t
    use intrinsic_registry, only: is_intrinsic_function, is_intrinsic_subroutine
    implicit none

    call check_registry()
    call check_c_funloc()
    call check_c_f_procpointer()

    print *, 'PASS: C_FUNLOC and C_F_PROCPOINTER intrinsics handled'

contains

    subroutine check_registry()
        if (.not. is_intrinsic_function('c_funloc')) then
            write (error_unit, '(A)') 'FAIL: c_funloc not registered as intrinsic'
            error stop 1
        end if
        if (.not. is_intrinsic_function('C_FUNLOC')) then
            write (error_unit, '(A)') 'FAIL: c_funloc registry not case-insensitive'
            error stop 1
        end if
        if (.not. is_intrinsic_subroutine('c_f_procpointer')) then
            write (error_unit, '(A)') &
                'FAIL: c_f_procpointer not registered as intrinsic subroutine'
            error stop 1
        end if
    end subroutine check_registry

    subroutine check_c_funloc()
        character(len=:), allocatable :: source_code, output_code, error_msg
        type(transform_context_t) :: ctx

        call read_example('examples/f90/issue_2814_c_funloc.f90', source_code)
        ctx%input_mode = INPUT_MODE_STANDARD
        ctx%has_filename = .true.
        ctx%source_name = 'issue_2814_c_funloc'
        call transform_with_context(source_code, output_code, error_msg, ctx)

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: c_funloc transform error: '// &
                trim(error_msg)
            error stop 1
        end if
        if (index(output_code, 'external :: c_funloc') > 0 .or. &
            index(output_code, 'external :: C_FUNLOC') > 0) then
            write (error_unit, '(A)') 'FAIL: c_funloc emitted as external'
            write (error_unit, '(A)') output_code
            error stop 1
        end if
        if (index(output_code, ':: c_sub') > 0) then
            write (error_unit, '(A)') &
                'FAIL: procedure argument c_sub declared as variable'
            write (error_unit, '(A)') output_code
            error stop 1
        end if
        if (index(output_code, 'c_funloc(c_sub)') == 0) then
            write (error_unit, '(A)') 'FAIL: c_funloc call not preserved'
            write (error_unit, '(A)') output_code
            error stop 1
        end if
    end subroutine check_c_funloc

    subroutine check_c_f_procpointer()
        character(len=:), allocatable :: source_code, output_code, error_msg
        type(transform_context_t) :: ctx

        call read_example('examples/f90/issue_2814_c_f_procpointer.f90', source_code)
        ctx%input_mode = INPUT_MODE_STANDARD
        ctx%has_filename = .true.
        ctx%source_name = 'issue_2814_c_f_procpointer'
        call transform_with_context(source_code, output_code, error_msg, ctx)

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: c_f_procpointer transform error: '// &
                trim(error_msg)
            error stop 1
        end if
        if (index(output_code, 'call c_f_procpointer(cfptr, fsub)') == 0) then
            write (error_unit, '(A)') 'FAIL: c_f_procpointer call not preserved'
            write (error_unit, '(A)') output_code
            error stop 1
        end if
    end subroutine check_c_f_procpointer

    include 'common/read_example.inc'
end program test_issue_2814_c_interop_intrinsics
