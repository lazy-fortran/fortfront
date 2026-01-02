program test_issue_1966_double_precision_inference
    use, intrinsic :: iso_fortran_env, only: dp => real64, error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    all_passed = check_double_expression()

    if (all_passed) then
        print *, 'PASS: Issue #1966 - double precision inference correct'
    else
        error stop 'FAIL: Issue #1966 regression detected'
    end if

contains

    include '../../common/read_example.inc'


    logical function check_double_expression()
        use, intrinsic :: iso_fortran_env, only: dp => real64
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg
        integer :: pos_integer_decl
        integer :: pos_double_decl
        real(dp) :: expected_area

        check_double_expression = .true.

        call read_example( &
            'examples/lf/issue_1966_double_precision_expression.lf', source)
        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                write (error_unit, '(A)') &
                    'FAIL: Unexpected error - ' // trim(error_msg)
                check_double_expression = .false.
                return
            end if
        end if

        if (.not. allocated(output)) then
            write (error_unit, '(A)') 'FAIL: No output generated'
            check_double_expression = .false.
            return
        end if

        pos_integer_decl = index(output, 'integer :: area')
        pos_double_decl = index(output, 'double precision :: area')

        if (pos_integer_decl > 0) then
            write (error_unit, '(A)') 'FAIL: area declared as integer'
            check_double_expression = .false.
        end if

        if (pos_double_decl == 0) then
            write (error_unit, '(A)') 'FAIL: area not declared as double precision'
            check_double_expression = .false.
        end if

        expected_area = 3.141592653589793_dp * 5.0_dp**2
        if (check_double_expression) then
            print *, 'Computed reference area (dp) =', expected_area
        end if
    end function check_double_expression

end program test_issue_1966_double_precision_inference
