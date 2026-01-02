program test_issue_2659_implicit_contains_pure_subroutine
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: input_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    logical :: has_pure_subroutine

    call read_example('examples/lf/issue_2659_implicit_contains_pure_subroutine.lf', &
                      input_code)

    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: transformation returned error'
        write (error_unit, '(A)') trim(error_msg)
        write (error_unit, '(A)') trim(output_code)
        error stop 1
    end if

    has_pure_subroutine = index(output_code, 'pure subroutine bump') > 0
    if (.not. has_pure_subroutine) then
        write (error_unit, '(A)') 'FAIL: missing pure subroutine prefix in contains'
        write (error_unit, '(A)') trim(output_code)
        error stop 1
    end if

    print *, 'PASS: procedure prefixes preserved for subroutines in implicit contains'


contains


    include '../common/read_example.inc'
end program test_issue_2659_implicit_contains_pure_subroutine
