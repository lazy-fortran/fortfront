program test_issue_1703_lazy_matrices
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use transformation_api, only: transform_lazy_fortran_string

    call test_reshape_integer_matrices()
    call test_reshape_real_matrices()
    call test_reshape_mixed_operations()

contains

    subroutine test_reshape_integer_matrices()
        character(len=:), allocatable :: input, output, error_msg

        input = 'A = reshape([1, 2, 3, 4], [2, 2])' // new_line('A') // &
                'B = reshape([5, 6, 7, 8], [2, 2])' // new_line('A') // &
                'C = A + B' // new_line('A') // &
                'print *, C(1,:)' // new_line('A') // &
                'print *, C(2,:)'

        call transform_lazy_fortran_string(input, output, error_msg)

        if (len(error_msg) > 0) then
            print *, 'ERROR: ', error_msg
            error stop 1
        end if

        if (index(output, 'integer') == 0 .and. index(output, 'real') == 0) then
            print *, 'FAIL: No type declaration found for matrices'
            print *, output
            error stop 1
        end if

        if (index(output, '(:,:)') == 0 .and. index(output, '(2,2)') == 0 .and. &
            index(output, '(2, 2)') == 0) then
            print *, 'FAIL: Matrices not declared as 2D arrays'
            print *, output
            error stop 1
        end if

        if (index(output, ':: A') == 0) then
            print *, 'FAIL: Variable A not declared'
            print *, output
            error stop 1
        end if

        if (index(output, 'A = reshape') == 0) then
            print *, 'FAIL: reshape assignment not preserved'
            print *, output
            error stop 1
        end if

        print *, 'PASS: test_reshape_integer_matrices'
    end subroutine test_reshape_integer_matrices

    subroutine test_reshape_real_matrices()
        character(len=:), allocatable :: input, output, error_msg

        input = 'X = reshape([1.0, 2.0, 3.0, 4.0], [2, 2])' // new_line('A') // &
                'print *, X(1,1)'

        call transform_lazy_fortran_string(input, output, error_msg)

        if (len(error_msg) > 0) then
            print *, 'ERROR: ', error_msg
            error stop 1
        end if

        if (index(output, 'real') == 0) then
            print *, 'FAIL: Real matrices not inferred as real type'
            print *, output
            error stop 1
        end if

        if (index(output, '(:,:)') == 0 .and. index(output, '(2,2)') == 0 .and. &
            index(output, '(2, 2)') == 0) then
            print *, 'FAIL: Real matrices not declared as 2D arrays'
            print *, output
            error stop 1
        end if

        print *, 'PASS: test_reshape_real_matrices'
    end subroutine test_reshape_real_matrices

    subroutine test_reshape_mixed_operations()
        character(len=:), allocatable :: input, output, error_msg

        input = 'M = reshape([1, 2, 3, 4, 5, 6], [2, 3])' // new_line('A') // &
                'N = M * 2' // new_line('A') // &
                'print *, N(1,:)'

        call transform_lazy_fortran_string(input, output, error_msg)

        if (len(error_msg) > 0) then
            print *, 'ERROR: ', error_msg
            error stop 1
        end if

        if (index(output, '(:,:)') == 0 .and. index(output, '(2,3)') == 0 .and. &
            index(output, '(2, 3)') == 0) then
            print *, 'FAIL: 2x3 matrices not declared correctly'
            print *, output
            error stop 1
        end if

        print *, 'PASS: test_reshape_mixed_operations'
    end subroutine test_reshape_mixed_operations

end program test_issue_1703_lazy_matrices
