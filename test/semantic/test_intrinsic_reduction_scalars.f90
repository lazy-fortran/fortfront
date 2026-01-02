program test_intrinsic_reduction_scalars
    use, intrinsic :: iso_fortran_env, only: error_unit
    use, intrinsic :: iso_fortran_env, only: input_unit, iostat_end, iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: input_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg

    call read_example('examples/lf/issue_1961_array_reduction_intrinsics.lf', &
                      input_code)

    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    if (len_trim(error_msg) /= 0) then
        print *, "Lazy Fortran transform failed unexpectedly"
        print *, trim(error_msg)
        stop 1
    end if

    call ensure_contains(output_code, ", total")
    call ensure_contains(output_code, ":: high")
    call ensure_contains(output_code, ", low")
    call ensure_contains(output_code, ", prod")
    call ensure_contains(output_code, "logical :: has_true")
    call ensure_contains(output_code, "integer :: true_count")

    call ensure_absent(output_code, "allocatable :: total")
    call ensure_absent(output_code, ":: total(:")
    call ensure_absent(output_code, "allocatable :: high")
    call ensure_absent(output_code, ":: high(:")
    call ensure_absent(output_code, "allocatable :: low")
    call ensure_absent(output_code, ":: low(:")
    call ensure_absent(output_code, "allocatable :: prod")
    call ensure_absent(output_code, ":: prod(:")
    call ensure_absent(output_code, "allocatable :: has_true")
    call ensure_absent(output_code, "allocatable :: true_count")

contains

    include '../common/read_example.inc'


    subroutine ensure_contains(text, pattern)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: pattern

        if (index(text, pattern) == 0) then
            print *, "Expected substring missing:", trim(pattern)
            stop 1
        end if
    end subroutine ensure_contains

    subroutine ensure_absent(text, pattern)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: pattern

        if (index(text, pattern) /= 0) then
            print *, "Unexpected substring present:", trim(pattern)
            stop 1
        end if
    end subroutine ensure_absent

end program test_intrinsic_reduction_scalars
