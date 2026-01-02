program test_issue_1962_matmul_rank
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    use string_utils_mod, only: to_lower
    implicit none

    character(len=:), allocatable :: input_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    character(len=:), allocatable :: lowered_output

    call read_example('examples/lf/issue_1962_matmul_rank.lf', input_code)
    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: matmul transformation reported error'
        write (error_unit, '(A)') trim(error_msg)
        error stop 1
    end if

    lowered_output = to_lower(output_code)

    if (.not. has_rank_two_declaration(lowered_output)) then
        write (error_unit, '(A)') 'FAIL: matmul result not inferred as rank-2 array'
        write (error_unit, '(A)') trim(output_code)
        error stop 1
    end if

    if (index(lowered_output, ':: c(:)') /= 0) then
        write (error_unit, '(A)') 'FAIL: matmul result still inferred as rank-1 array'
        write (error_unit, '(A)') trim(output_code)
        error stop 1
    end if

    if (index(lowered_output, 'c = matmul(a, b)') == 0) then
        write (error_unit, '(A)') 'FAIL: matmul assignment missing from output'
        write (error_unit, '(A)') trim(output_code)
        error stop 1
    end if

    print *, 'PASS: matmul result inferred as rank-2 array'

contains

    include 'common/cli_io_reader.inc'
    include 'common/read_example.inc'

    logical function has_rank_two_declaration(text) result(has_rank_two)
        character(len=*), intent(in) :: text
        integer :: decl_pos
        integer :: close_pos
        integer :: relative_close

        has_rank_two = index(text, ':: c(:,:)') > 0
        if (has_rank_two) return

        decl_pos = index(text, ':: c(')
        if (decl_pos <= 0) return

        relative_close = index(text(decl_pos:), ')')
        if (relative_close <= 0) return

        close_pos = decl_pos + relative_close - 1
        if (index(text(decl_pos:close_pos), ',') > 0) has_rank_two = .true.
    end function has_rank_two_declaration


end program test_issue_1962_matmul_rank
