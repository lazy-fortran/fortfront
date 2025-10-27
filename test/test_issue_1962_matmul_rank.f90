program test_issue_1962_matmul_rank
    use, intrinsic :: iso_fortran_env, only: error_unit
    use, intrinsic :: iso_fortran_env, only: input_unit, iostat_end, iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    use string_utils_mod, only: to_lower
    implicit none

    character(:), allocatable :: input_code
    character(:), allocatable :: output_code
    character(:), allocatable :: error_msg
    character(:), allocatable :: lowered_output

    call read_example('examples/lf/issue_1962_matmul_rank.lf', input_code)
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

    include 'common/cli_io_reader.inc'

    subroutine read_example(path, content)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: content
        integer :: status

        call read_all_stdin_or_file(.true., path, content, status)
        if (status /= 0) then
            write (error_unit, '(a)') 'FAIL: failed to load example'
            stop 1
        end if
    end subroutine read_example

end program test_issue_1962_matmul_rank
