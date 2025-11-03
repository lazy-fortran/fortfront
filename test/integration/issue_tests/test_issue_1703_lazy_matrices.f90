program test_issue_1703_lazy_matrices
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use string_utils_mod, only: to_lower
    use transformation_api, only: transform_lazy_fortran_string

    call test_reshape_integer_matrices()
    call test_reshape_real_matrices()
    call test_reshape_mixed_operations()

contains

    subroutine test_reshape_integer_matrices()
        character(len=:), allocatable :: input, output, error_msg
        character(len=:), allocatable :: lowered

        call read_example('examples/lf/reshape_integer_matrices.lf', input)

        call transform_lazy_fortran_string(input, output, error_msg)

        if (len(error_msg) > 0) then
            print *, 'ERROR: ', error_msg
            error stop 1
        end if

        lowered = to_lower(output)

        if (index(lowered, 'integer') == 0 .and. index(lowered, 'real') == 0) then
            print *, 'FAIL: No type declaration found for matrices'
            print *, output
            error stop 1
        end if

        if (index(lowered, '(:,:)') == 0 .and. index(lowered, '(2,2)') == 0 .and. &
            index(lowered, '(2, 2)') == 0) then
            print *, 'FAIL: Matrices not declared as 2D arrays'
            print *, output
            error stop 1
        end if

        if (index(lowered, ':: a') == 0) then
            print *, 'FAIL: Variable a not declared'
            print *, output
            error stop 1
        end if

        if (index(lowered, 'a = reshape') == 0) then
            print *, 'FAIL: reshape assignment not preserved'
            print *, output
            error stop 1
        end if

        print *, 'PASS: test_reshape_integer_matrices'
    end subroutine test_reshape_integer_matrices

    subroutine test_reshape_real_matrices()
        character(len=:), allocatable :: input, output, error_msg
        character(len=:), allocatable :: lowered

        call read_example('examples/lf/reshape_real_matrix.lf', input)

        call transform_lazy_fortran_string(input, output, error_msg)

        if (len(error_msg) > 0) then
            print *, 'ERROR: ', error_msg
            error stop 1
        end if

        lowered = to_lower(output)

        if (index(lowered, 'real') == 0) then
            print *, 'FAIL: Real matrices not inferred as real type'
            print *, output
            error stop 1
        end if

        if (index(lowered, '(:,:)') == 0 .and. index(lowered, '(2,2)') == 0 .and. &
            index(lowered, '(2, 2)') == 0) then
            print *, 'FAIL: Real matrices not declared as 2D arrays'
            print *, output
            error stop 1
        end if

        print *, 'PASS: test_reshape_real_matrices'
    end subroutine test_reshape_real_matrices

    subroutine test_reshape_mixed_operations()
        character(len=:), allocatable :: input, output, error_msg
        character(len=:), allocatable :: lowered

        call read_example('examples/lf/reshape_mixed_operations.lf', input)

        call transform_lazy_fortran_string(input, output, error_msg)

        if (len(error_msg) > 0) then
            print *, 'ERROR: ', error_msg
            error stop 1
        end if

        lowered = to_lower(output)

        if (index(lowered, '(:,:)') == 0 .and. index(lowered, '(2,3)') == 0 .and. &
            index(lowered, '(2, 3)') == 0) then
            print *, 'FAIL: 2x3 matrices not declared correctly'
            print *, output
            error stop 1
        end if

        print *, 'PASS: test_reshape_mixed_operations'
    end subroutine test_reshape_mixed_operations

    include '../../common/cli_io_reader.inc'

    subroutine read_example(path, content)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: content
        integer :: status

        call read_all_stdin_or_file(.true., path, content, status)
        if (status /= 0) then
            write (error_unit, '(A)') 'FAIL: failed to read ' // trim(path)
            error stop 1
        end if
    end subroutine read_example

end program test_issue_1703_lazy_matrices
