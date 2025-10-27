program test_intrinsic_reduction_scalars
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: input_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg

    input_code = "program demo" // new_line('a') // &
                 "    arr = [1.0_dp, 2.0_dp, 3.0_dp]" // new_line('a') // &
                 "    total = sum(arr)" // new_line('a') // &
                 "    high = maxval(arr)" // new_line('a') // &
                 "    low = minval(arr)" // new_line('a') // &
                 "    prod = product(arr)" // new_line('a') // &
                 "    flags = [.true., .false., .true.]" // new_line('a') // &
                 "    has_true = any(flags)" // new_line('a') // &
                 "    true_count = count(flags)" // new_line('a') // &
                 "end program demo"

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
