program test_issue_2815_ieee_arithmetic
    use transformation_api, only: transform_lazy_fortran_string
    use intrinsic_registry, only: is_intrinsic_function, is_intrinsic_subroutine
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: errors
    logical :: ok

    print *, "=== Issue #2815: IEEE intrinsic modules ==="

    call check_registry()
    call check_inference()

    print *, "ALL TESTS PASSED"

contains

    include 'common/read_example.inc'

    subroutine check_registry()
        ! IEEE procedures must be recognized as intrinsic.
        call expect(is_intrinsic_function("ieee_is_nan"), &
                    "ieee_is_nan is intrinsic function")
        call expect(is_intrinsic_function("ieee_value"), &
                    "ieee_value is intrinsic function")
        call expect(is_intrinsic_function("ieee_copy_sign"), &
                    "ieee_copy_sign is intrinsic function")
        call expect(is_intrinsic_subroutine("ieee_set_rounding_mode"), &
                    "ieee_set_rounding_mode is intrinsic subroutine")
        call expect(is_intrinsic_subroutine("ieee_get_flag"), &
                    "ieee_get_flag is intrinsic subroutine")
    end subroutine check_registry

    subroutine check_inference()
        ! Lazy-Fortran end-to-end: IEEE function results must be typed
        ! by their return type, not declared as external reals.
        call read_example('examples/lf/issue_2815_ieee_arithmetic.lf', source)
        call transform_lazy_fortran_string(source, output, errors)

        call expect(len_trim(errors) == 0, "transform produced no errors")
        call expect(has_decl(output, "logical", "is_bad"), &
                    "ieee_is_nan result declared logical")
        call expect(has_decl(output, "logical", "finite"), &
                    "ieee_is_finite result declared logical")
        call expect(has_decl(output, "real(dp)", "y"), &
                    "ieee_copy_sign result declared real")
        ok = index(output, "external :: ieee_") == 0 .and. &
             index(output, "external::ieee_") == 0
        call expect(ok, "no spurious external IEEE declaration")
        call expect(index(output, "use, intrinsic :: ieee_arithmetic") > 0, &
                    "intrinsic use of ieee_arithmetic preserved")
    end subroutine check_inference

    logical function has_decl(buffer, type_name, var_name) result(found)
        character(len=*), intent(in) :: buffer
        character(len=*), intent(in) :: type_name
        character(len=*), intent(in) :: var_name

        integer :: search_pos, decl_pos, line_end
        character :: nl
        character(len=:), allocatable :: line

        found = .false.
        nl = new_line('a')
        search_pos = 1
        do
            if (search_pos > len(buffer)) exit
            decl_pos = index(buffer(search_pos:), trim(type_name)//' ::')
            if (decl_pos <= 0) exit
            decl_pos = decl_pos + search_pos - 1
            line_end = decl_pos
            do while (line_end <= len(buffer))
                if (buffer(line_end:line_end) == nl) exit
                line_end = line_end + 1
            end do
            if (line_end > len(buffer)) then
                line = buffer(decl_pos:)
            else
                line = buffer(decl_pos:line_end - 1)
            end if
            if (index(line, trim(var_name)) > 0) then
                found = .true.
                return
            end if
            search_pos = line_end + 1
        end do
    end function has_decl

    subroutine expect(condition, label)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: label

        if (condition) then
            print *, "  PASS: ", label
        else
            print *, "  FAIL: ", label
            if (allocated(output)) then
                print *, "Output:"
                print *, trim(output)
            end if
            error stop 1
        end if
    end subroutine expect

end program test_issue_2815_ieee_arithmetic
