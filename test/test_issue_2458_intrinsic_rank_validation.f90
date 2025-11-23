program test_issue_2458_intrinsic_rank_validation
    use transformation_api, only: transform_lazy_fortran_string
    implicit none
    logical :: test_passed
    character(len=:), allocatable :: source, output, error_msg

    test_passed = .true.

    call test_multi_array_declaration()

    if (test_passed) then
        print *, "test_issue_2458_intrinsic_rank_validation PASSED"
    else
        print *, "test_issue_2458_intrinsic_rank_validation FAILED"
        error stop 1
    end if

contains

    subroutine test_multi_array_declaration()
        call read_example( &
            'examples/f90/issue_2458_multi_array_declaration.f90', source)
        call transform_lazy_fortran_string(source, output, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "ERROR: Transformation failed:", trim(error_msg)
            test_passed = .false.
            return
        end if

        if (index(output, 'real, intent(in) :: a(:, :), b(:, :)') == 0) then
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

    subroutine read_example(filepath, content)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: content
        integer :: unit, ios
        character(len=10000) :: line

        content = ""
        open (newunit=unit, file=filepath, status='old', &
            action='read', iostat=ios)
        if (ios /= 0) then
            print *, "ERROR: Failed to open example file:", filepath
            error stop 1
        end if

        do
            read (unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (len(content) > 0) content = content // new_line('a')
            content = content // trim(line)
        end do

        close (unit)
    end subroutine read_example

end program test_issue_2458_intrinsic_rank_validation
