program test_issue_1573_pointer_assignment_lost
    ! Test for issue #1573: Pointer assignment (=>) lost during transformation
    ! Bug: Pointer assignment is silently dropped when followed by print then regular assignment
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
                                             iostat_eor
    use transformation_api, only: transform_lazy_fortran_string

    call test_pointer_assignment_preserved_case1()
    call test_pointer_assignment_preserved_case2()
    call test_pointer_assignment_lost_bug()
    print *, ""
    print *, "PASS: All pointer assignment tests passed! Issue #1573 has been fixed."

contains

    include '../common/cli_io_reader.inc'
    include '../common/read_example.inc'

    subroutine test_pointer_assignment_preserved_case1()
        ! This case works: pointer assignment -> regular assignment -> print
        character(len=:), allocatable :: input_code, output_code, error_msg

        call read_example('examples/lf/pointer_assignment_before_print.lf', &
                          input_code)

        call transform_lazy_fortran_string(input_code, output_code, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "FAIL case1: transformation error:", trim(error_msg)
            error stop 1
        end if

        if (index(output_code, "p => x") <= 0) then
            print *, "FAIL case1: pointer assignment lost"
            print *, "Expected: p => x"
            print *, "Output:", trim(output_code)
            error stop 1
        end if

        print *, "PASS case1: pointer assignment preserved (assignment before print)"
    end subroutine test_pointer_assignment_preserved_case1

    subroutine test_pointer_assignment_preserved_case2()
        ! This case works: pointer assignment with no subsequent regular assignment
        character(len=:), allocatable :: input_code, output_code, error_msg

        call read_example('examples/lf/pointer_assignment_direct_print.lf', &
                          input_code)

        call transform_lazy_fortran_string(input_code, output_code, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "FAIL case2: transformation error:", trim(error_msg)
            error stop 1
        end if

        if (index(output_code, "p => x") <= 0) then
            print *, "FAIL case2: pointer assignment lost"
            print *, "Expected: p => x"
            print *, "Output:", trim(output_code)
            error stop 1
        end if

        print *, "PASS case2: pointer assignment preserved (no assignment after)"
    end subroutine test_pointer_assignment_preserved_case2

    subroutine test_pointer_assignment_lost_bug()
        ! BUG: This case fails - pointer assignment -> print -> regular assignment
        character(len=:), allocatable :: input_code, output_code, error_msg

        call read_example('examples/lf/pointer_assignment_print_then_assign.lf', &
                          input_code)

        call transform_lazy_fortran_string(input_code, output_code, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "FAIL case3 (bug): transformation error:", trim(error_msg)
            error stop 1
        end if

        if (index(output_code, "p => x") <= 0) then
            print *, "FAIL case3 (bug): pointer assignment lost after fix"
            print *, "Expected: p => x"
            print *, "Actual output:"
            print *, trim(output_code)
            error stop 1
        end if

        print *, "PASS case3 (bug fixed): pointer assignment preserved (print before assignment)"
    end subroutine test_pointer_assignment_lost_bug


end program test_issue_1573_pointer_assignment_lost
