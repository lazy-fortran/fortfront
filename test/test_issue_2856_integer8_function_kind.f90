program test_issue_2856_integer8_function_kind
    use transformation_api, only: transform_lazy_fortran_string
    use string_utils_mod, only: to_lower
    implicit none

    character(:), allocatable :: input_code
    character(:), allocatable :: output_code
    character(:), allocatable :: error_msg
    character(:), allocatable :: lowered

    call read_example('examples/f90/issue_2856_integer8_function_result.f90', &
        input_code)
    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, "FAIL: transform error: ", trim(error_msg)
            error stop 1
        end if
    end if

    lowered = to_lower(output_code)

    ! The declared result kind must survive standardization: either preserved
    ! on the function statement or restated as a body result declaration.
    if (index(lowered, "integer(8)") == 0 .and. &
        index(lowered, "integer(kind=8)") == 0) then
        print *, "FAIL: integer(8) result kind dropped"
        print *, output_code
        error stop 1
    end if

    print *, "PASS: issue 2856 integer(8) function result kind preserved"

contains

    include 'common/read_example.inc'

end program test_issue_2856_integer8_function_kind
