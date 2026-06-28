program test_issue_2820_coverage_backfill
    ! Behavioral coverage for working-but-previously-untested features
    ! surfaced by the F77-F2003 + LF/Infer standard-coverage audit (#2820):
    ! SAVE, ALLOCATE(MOLD=/SOURCE=), CPU_TIME/SYSTEM_CLOCK, the ISO_C_BINDING
    ! pointer intrinsics, and INFER first-assignment type inference.
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    call check_save()
    call check_allocate_mold_source()
    call check_timing_intrinsics()
    call check_c_interop_pointers()
    call check_infer_first_assignment()
    call check_dp_injection()
    call check_dp_collision()

    print *, ""
    print *, "Issue 2820 coverage backfill tests completed."

contains

    include 'common/read_example.inc'

    subroutine transform_example(example_path, output)
        character(len=*), intent(in) :: example_path
        character(:), allocatable, intent(out) :: output
        character(:), allocatable :: input_code, error_msg

        call read_example(example_path, input_code)
        call transform_lazy_fortran_string(input_code, output, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                write (error_unit, '(A)') example_path//' transform error: '// &
                    trim(error_msg)
                error stop 1
            end if
        end if
    end subroutine transform_example

    subroutine require(condition, label, output)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: label
        character(len=*), intent(in) :: output

        if (.not. condition) then
            write (error_unit, '(A)') 'FAIL: '//label
            write (error_unit, '(A)') trim(output)
            error stop 1
        end if
        print *, '[PASS] '//label
    end subroutine require

    subroutine check_save()
        character(:), allocatable :: out

        call transform_example('examples/f90/issue_2820_save_attribute.f90', out)
        call require(index(out, 'save') > 0 .and. index(out, 'counter') > 0, &
            'SAVE attribute preserved', out)
    end subroutine check_save

    subroutine check_allocate_mold_source()
        character(:), allocatable :: out

        call transform_example( &
            'examples/f90/issue_2820_allocate_mold_source.f90', out)
        call require(index(out, 'mold=') > 0 .and. index(out, 'source=') > 0, &
            'ALLOCATE MOLD= and SOURCE= preserved', out)
    end subroutine check_allocate_mold_source

    subroutine check_timing_intrinsics()
        character(:), allocatable :: out

        call transform_example( &
            'examples/f90/issue_2820_intrinsics_timing.f90', out)
        call require(index(out, 'call cpu_time') > 0 .and. &
            index(out, 'call system_clock') > 0, &
            'CPU_TIME and SYSTEM_CLOCK calls preserved', out)
    end subroutine check_timing_intrinsics

    subroutine check_c_interop_pointers()
        character(:), allocatable :: out
        logical :: base_intrinsics, shape_and_two_arg

        call transform_example( &
            'examples/f90/issue_2820_c_interop_pointers.f90', out)
        base_intrinsics = index(out, 'c_loc') > 0 .and. &
            index(out, 'c_f_pointer') > 0 .and. &
            index(out, 'c_associated') > 0 .and. &
            index(out, 'iso_c_binding') > 0
        ! SHAPE-bearing C_F_POINTER and two-argument C_ASSOCIATED forms.
        shape_and_two_arg = index(out, 'c_f_pointer(cp2, parr, [4])') > 0 .and. &
            index(out, 'c_associated(cp2, cp2)') > 0
        call require(base_intrinsics .and. shape_and_two_arg, &
            'ISO_C_BINDING pointer intrinsics preserved', out)
    end subroutine check_c_interop_pointers

    subroutine check_infer_first_assignment()
        character(:), allocatable :: out
        logical :: y_is_real8

        call transform_example( &
            'examples/lf/issue_2820_infer_first_assignment.lf', out)
        y_is_real8 = index(out, 'double precision :: y') > 0 .or. &
            index(out, 'real(8) :: y') > 0 .or. &
            index(out, 'real(dp) :: y') > 0
        call require(index(out, 'integer :: x') > 0 .and. y_is_real8, &
            'INFER declares x integer and y real8 from first assignment', &
            out)
    end subroutine check_infer_first_assignment

    subroutine check_dp_injection()
        character(:), allocatable :: out

        ! INFER mode injects the dp kind parameter when dp is referenced
        ! but never defined by the user.
        call transform_example('examples/lf/issue_2820_dp_injection.lf', out)
        call require(index(out, 'dp => real64') > 0 .and. &
            index(out, '1.0_dp') > 0, &
            'dp predefined symbol injected when undefined', out)
    end subroutine check_dp_injection

    subroutine check_dp_collision()
        character(:), allocatable :: out

        ! A user-defined dp must be preserved; the injected literal kind is
        ! renamed (lf_dp_kind) so the user symbol is not shadowed.
        call transform_example('examples/lf/issue_2820_dp_collision.lf', out)
        call require(index(out, 'integer, parameter :: dp = kind(1.0d0)') > 0 &
            .and. index(out, 'lf_dp_kind => real64') > 0 &
            .and. index(out, '1.0_lf_dp_kind') > 0, &
            'dp injection avoids collision with user-defined dp', out)
    end subroutine check_dp_collision

end program test_issue_2820_coverage_backfill
