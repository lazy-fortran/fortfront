program test_real_literal_kind_inference
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed
    real(dp), parameter :: zero_dp = 0.0_dp

    if (zero_dp /= 0.0_dp) then
        stop 1
    end if

    all_passed = .true.

    if (.not. test_double_exponent_infers_double()) all_passed = .false.
    if (.not. test_kind_suffix_infers_double()) all_passed = .false.

    if (all_passed) then
        stop 0
    else
        stop 1
    end if

contains

    function test_double_exponent_infers_double() result(passed)
        logical :: passed
        character(len=:), allocatable :: output

        output = compile_lazy_line('c = 1.0d0')
        passed = check_output(output, 'double precision :: c', 'c = 1.0d0')
    end function test_double_exponent_infers_double

    function test_kind_suffix_infers_double() result(passed)
        logical :: passed
        character(len=:), allocatable :: output

        output = compile_lazy_line('d = 3.14159_8')
        passed = check_output(output, 'double precision :: d', 'd = 3.14159_8')
    end function test_kind_suffix_infers_double

    function check_output(output, declaration, assignment) result(passed)
        character(len=*), intent(in) :: output
        character(len=*), intent(in) :: declaration
        character(len=*), intent(in) :: assignment
        logical :: passed

        passed = index(output, declaration) > 0
        if (passed) passed = index(output, assignment) > 0
        if (passed) passed = index(output, '!ERROR:') == 0
        if (.not. passed) then
            print *, 'Generated output:'
            print *, trim(output)
        end if
    end function check_output

    function compile_lazy_line(source_line) result(output)
        character(len=*), intent(in) :: source_line
        character(len=:), allocatable :: output
        character(len=:), allocatable :: source
        character(len=:), allocatable :: error_msg

        source = source_line
        call transform_lazy_fortran_string(source, output, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, 'Unexpected error:'
            print *, trim(error_msg)
        end if
        if (.not. allocated(output)) output = ''
    end function compile_lazy_line

end program test_real_literal_kind_inference
