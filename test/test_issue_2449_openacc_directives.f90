program test_issue_2449_openacc_directives
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        & iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    call test_basic_parallel_loop()
    call test_openacc_clauses()
    call test_fixed_form_openacc()

    print *, 'PASS: All OpenACC directive tests passed'

contains

    include 'common/cli_io_reader.inc'
    include 'common/read_example.inc'

    subroutine test_basic_parallel_loop()
        character(len=:), allocatable :: source_code
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg

        call read_example('examples/f90/issue_2449_openacc_directives.f90', &
            source_code)
        call transform_lazy_fortran_string(source_code, output_code, error_msg)

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: transform error: ' // trim(error_msg)
            error stop 1
        end if

        if (index(output_code, '!$acc parallel loop') == 0) then
            write (error_unit, '(A)') 'FAIL: missing !$acc parallel loop'
            error stop 1
        end if

        if (index(output_code, '!$acc end parallel loop') == 0) then
            write (error_unit, '(A)') 'FAIL: missing !$acc end parallel loop'
            error stop 1
        end if
    end subroutine test_basic_parallel_loop

    subroutine test_openacc_clauses()
        character(len=:), allocatable :: source_code
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg

        call read_example('examples/f90/issue_2449_openacc_clauses.f90', source_code)
        call transform_lazy_fortran_string(source_code, output_code, error_msg)

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: transform error: ' // trim(error_msg)
            error stop 1
        end if

        if (index(output_code, 'copyin(a, b)') == 0) then
            write (error_unit, '(A)') 'FAIL: missing copyin clause'
            error stop 1
        end if

        if (index(output_code, 'copyout(c)') == 0) then
            write (error_unit, '(A)') 'FAIL: missing copyout clause'
            error stop 1
        end if

        if (index(output_code, 'async(1)') == 0) then
            write (error_unit, '(A)') 'FAIL: missing async clause'
            error stop 1
        end if

        if (index(output_code, '!$acc update host(a)') == 0) then
            write (error_unit, '(A)') 'FAIL: missing update directive'
            error stop 1
        end if
    end subroutine test_openacc_clauses

    subroutine test_fixed_form_openacc()
        character(len=:), allocatable :: source_code
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg

        call read_example('examples/f90/issue_2449_openacc_fixed.f', source_code)
        call transform_lazy_fortran_string(source_code, output_code, error_msg)

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: transform error: ' // trim(error_msg)
            error stop 1
        end if

        if (index(output_code, '!$acc parallel loop') == 0) then
            write (error_unit, '(A)') &
                'FAIL: missing !$acc parallel loop in fixed-form'
            error stop 1
        end if
    end subroutine test_fixed_form_openacc


end program test_issue_2449_openacc_directives
