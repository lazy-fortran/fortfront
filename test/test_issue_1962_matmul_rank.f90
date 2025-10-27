program test_issue_1962_matmul_rank
    use transformation_api, only: transform_lazy_fortran_string
    use string_utils_mod, only: to_lower
    implicit none

    character(:), allocatable :: input_code
    character(:), allocatable :: output_code
    character(:), allocatable :: error_msg
    character(:), allocatable :: lowered_output

    call build_input(input_code)
    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    if (len_trim(error_msg) > 0) then
        print *, 'FAIL: matmul transformation reported an error'
        print *, trim(error_msg)
        error stop 1
    end if

    lowered_output = to_lower(output_code)

    if (index(lowered_output, 'real, allocatable :: c(:,:)') == 0) then
        print *, 'FAIL: matmul result not inferred as rank-2 array'
        print *, 'Output:' // new_line('a') // trim(output_code)
        error stop 1
    end if

    if (index(lowered_output, ':: c(:)') /= 0) then
        print *, 'FAIL: matmul result still inferred as rank-1 array'
        print *, 'Output:' // new_line('a') // trim(output_code)
        error stop 1
    end if

    if (index(lowered_output, 'c = matmul(a, b)') == 0) then
        print *, 'FAIL: matmul assignment missing from transformed output'
        print *, 'Output:' // new_line('a') // trim(output_code)
        error stop 1
    end if

    print *, 'PASS: matmul result inferred as rank-2 array'

contains

    subroutine build_input(code)
        character(:), allocatable, intent(out) :: code
        character(len=:), allocatable :: nl

        nl = new_line('a')
        code = 'a = reshape([1.0, 2.0, 3.0, 4.0], [2, 2])' // nl // &
               'b = reshape([5.0, 6.0, 7.0, 8.0], [2, 2])' // nl // &
               'c = matmul(a, b)' // nl // &
               "print *, 'c(1,1) =', c(1,1)"
    end subroutine build_input

end program test_issue_1962_matmul_rank
