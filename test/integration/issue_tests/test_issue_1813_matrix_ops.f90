program test_issue_1813_matrix_ops
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    call test_matrix_literal_addition()

contains

    subroutine test_matrix_literal_addition()
        character(len=:), allocatable :: input, output, error_msg

        input = 'A = [[1, 2], [3, 4]]' // new_line('A') // &
                'B = [[5, 6], [7, 8]]' // new_line('A') // &
                'C = A + B' // new_line('A') // &
                'print *, C'

        call transform_lazy_fortran_string(input, output, error_msg)

        if (len(error_msg) > 0) then
            print *, 'ERROR: ', error_msg
            error stop 1
        end if

        if (index(output, 'integer :: A(2,2)') == 0) then
            print *, 'FAIL: A not declared as (2,2)'
            print *, output
            error stop 1
        end if

        if (index(output, 'integer :: B(2,2)') == 0) then
            print *, 'FAIL: B not declared as (2,2)'
            print *, output
            error stop 1
        end if

        if (index(output, 'integer :: C(2,2)') == 0) then
            print *, 'FAIL: C not declared as (2,2), issue #1813 not fixed'
            print *, output
            error stop 1
        end if

        if (index(output, 'C = A + B') == 0) then
            print *, 'FAIL: Matrix addition not preserved'
            print *, output
            error stop 1
        end if

        print *, 'PASS: test_matrix_literal_addition'
    end subroutine test_matrix_literal_addition

end program test_issue_1813_matrix_ops
