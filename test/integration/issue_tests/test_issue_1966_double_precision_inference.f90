program test_issue_1966_double_precision_inference
    use, intrinsic :: iso_fortran_env, only: dp => real64, error_unit, &
                                              input_unit, iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    all_passed = .true.
    print *, '=== Issue #1966: Double precision expression inference ==='

    if (.not. check_double_expression()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'Issue #1966 fixed!'
    else
        print *, 'Issue #1966 regression detected!'
        stop 1
    end if

contains

    include '../../common/cli_io_reader.inc'

    subroutine read_example(path, content)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: content
        integer :: status

        call read_all_stdin_or_file(.true., path, content, status)
        if (status /= 0) then
            print *, 'FAIL: failed to read ', trim(path)
            error stop 1
        end if
    end subroutine read_example

    logical function check_double_expression()
        use, intrinsic :: iso_fortran_env, only: dp => real64
        implicit none
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg
        integer :: pos_integer_decl
        integer :: pos_double_decl
        real(dp) :: expected_area

        check_double_expression = .true.
        print *, 'Testing double precision expression assignment...'

        call read_example('examples/lf/issue_1966_double_precision_expression.lf', &
                          source)
        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Unexpected error -', trim(error_msg)
                check_double_expression = .false.
                return
            end if
        end if

        if (.not. allocated(output)) then
            print *, '  FAIL: No output generated'
            check_double_expression = .false.
            return
        end if

        pos_integer_decl = index(output, 'integer :: area')
        pos_double_decl = index(output, 'double precision :: area')

        if (pos_integer_decl > 0) then
            print *, '  FAIL: area declared as integer'
            check_double_expression = .false.
        end if

        if (pos_double_decl == 0) then
            print *, '  FAIL: area not declared as double precision'
            check_double_expression = .false.
        end if

        expected_area = 3.141592653589793_dp * 5.0_dp**2
        print *, '  Expected area (dp) =', expected_area

        if (check_double_expression) then
            print *, '  PASS: area inferred as double precision'
        end if
    end function check_double_expression

end program test_issue_1966_double_precision_inference
