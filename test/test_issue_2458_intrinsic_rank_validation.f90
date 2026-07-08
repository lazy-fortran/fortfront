program test_issue_2458_intrinsic_rank_validation
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_lazy_fortran_string
    implicit none
    logical :: test_passed
    character(len=:), allocatable :: source, output, error_msg

    test_passed = .true.

    call test_multi_array_declaration()
    call test_old_style_multi_array_declaration()

    if (test_passed) then
        print *, "test_issue_2458_intrinsic_rank_validation PASSED"
    else
        print *, "test_issue_2458_intrinsic_rank_validation FAILED"
        error stop 1
    end if

contains

    include 'common/read_example.inc'

    subroutine test_multi_array_declaration()
        call read_example( &
            'examples/f90/issue_2458_multi_array_declaration.f90', source)
        call transform_lazy_fortran_string(source, output, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "ERROR: Transformation failed:", trim(error_msg)
            test_passed = .false.
            return
        end if

        if (index(output, 'real, intent(in) :: a(:, :), b(:, :)') == 0 .and. &
            index(output, 'real(8), intent(in) :: a(:, :), b(:, :)') == 0 .and. &
            index(output, 'real(dp), intent(in) :: a(:, :), b(:, :)') == 0) then
            print *, "FAIL: Multi-array declaration not preserved"
            print *, "Expected: real, intent(in) :: a(:, :), b(:, :)"
            print *, "Output:", trim(output)
            test_passed = .false.
            return
        end if

        if (index(output, 'transpose(b)') == 0) then
            print *, "FAIL: transpose call missing"
            test_passed = .false.
            return
        end if
    end subroutine test_multi_array_declaration

    subroutine test_old_style_multi_array_declaration()
        source = 'program p'//new_line('a')// &
            '  implicit none'//new_line('a')// &
            '  integer a(3), b(5)'//new_line('a')// &
            '  print *, a(1), b(5)'//new_line('a')// &
            'end program p'

        call transform_lazy_fortran_string(source, output, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "ERROR: Transformation failed:", trim(error_msg)
            test_passed = .false.
            return
        end if

        if (index(output, 'integer :: a(3)') == 0 .or. &
            index(output, 'integer :: b(5)') == 0) then
            print *, "FAIL: old-style multi-array declaration not preserved"
            print *, "Output:", trim(output)
            test_passed = .false.
            return
        end if
    end subroutine test_old_style_multi_array_declaration


end program test_issue_2458_intrinsic_rank_validation
