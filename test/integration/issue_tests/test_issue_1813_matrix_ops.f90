program test_issue_1813_matrix_ops
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use string_utils_mod, only: to_lower
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    call test_matrix_literal_addition()

contains

    include '../../common/read_example.inc'

    subroutine test_matrix_literal_addition()
        character(len=:), allocatable :: input, output, error_msg
        character(len=:), allocatable :: lowered

        call read_example('examples/lf/matrix_literal_addition.lf', input)

        call transform_lazy_fortran_string(input, output, error_msg)

        if (len(error_msg) > 0) then
            print *, 'ERROR: ', error_msg
            error stop 1
        end if

        lowered = to_lower(output)

        if (index(lowered, 'integer :: a(2,2)') == 0) then
            print *, 'FAIL: a not declared as (2,2)'
            print *, output
            error stop 1
        end if

        if (index(lowered, 'integer :: b(2,2)') == 0) then
            print *, 'FAIL: b not declared as (2,2)'
            print *, output
            error stop 1
        end if

        if (index(lowered, 'integer :: c(2,2)') == 0) then
            print *, 'FAIL: c not declared as (2,2), issue #1813 not fixed'
            print *, output
            error stop 1
        end if

        if (index(lowered, 'c = a + b') == 0) then
            print *, 'FAIL: Matrix addition not preserved'
            print *, output
            error stop 1
        end if

        print *, 'PASS: test_matrix_literal_addition'
    end subroutine test_matrix_literal_addition

end program test_issue_1813_matrix_ops
