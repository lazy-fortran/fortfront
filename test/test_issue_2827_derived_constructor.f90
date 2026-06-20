program test_issue_2827_derived_constructor
    use transformation_api, only: transform_lazy_fortran_string
    use string_utils_mod, only: to_lower
    implicit none

    character(:), allocatable :: input_code
    character(:), allocatable :: output_code
    character(:), allocatable :: error_msg
    character(:), allocatable :: lowered

    call read_example('examples/lf/issue_2827_derived_constructor_inference.lf', &
                      input_code)
    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, "FAIL: transform error: ", trim(error_msg)
            error stop 1
        end if
    end if

    lowered = to_lower(output_code)

    ! Constructor target inferred as the derived type, not double/real.
    if (index(lowered, "type(point_t) :: p") == 0) then
        print *, "FAIL: p not declared as type(point_t)"
        print *, output_code
        error stop 1
    end if

    ! No external declaration for the type constructor name.
    if (index(lowered, ", external") > 0) then
        print *, "FAIL: spurious external declaration emitted"
        print *, output_code
        error stop 1
    end if

    ! Member access preserved (not corrupted to a renamed scalar).
    if (index(lowered, "p%x") == 0) then
        print *, "FAIL: member access p%x not preserved"
        print *, output_code
        error stop 1
    end if

    print *, "PASS: issue 2827 derived-type constructor inference"

contains

    include 'common/read_example.inc'

end program test_issue_2827_derived_constructor
