program test_issue_2025_internal_procedures
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    call test_program_with_multiple_internal_procedures()
    print *, ""
    print *, "Issue 2025 internal procedure tests completed."

contains

    subroutine test_program_with_multiple_internal_procedures()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg
        integer :: contains_count
        integer :: header_pos
        integer :: decl_pos

        input_code = "program test_combined_features" // new_line('A') // &
                     "    implicit none" // new_line('A') // new_line('A') // &
                     "    print *, 'Testing combined features'" // new_line('A') // &
                     "    call test_function()" // new_line('A') // &
                     "    call test_subroutine()" // new_line('A') // &
                     "contains" // new_line('A') // new_line('A') // &
                     "    subroutine test_function()" // new_line('A') // &
                     "        integer :: result" // new_line('A') // &
                     "        result = factorial(5)" // new_line('A') // &
                     "        print *, 'Factorial(5) =', result" // new_line('A') // &
                     "    end subroutine test_function" // new_line('A') // &
                     "    recursive integer function factorial(n) result(res)" // &
                     new_line('A') // &
                     "        integer, intent(in) :: n" // new_line('A') // &
                     "        if (n <= 1) then" // new_line('A') // &
                     "            res = 1" // new_line('A') // &
                     "        else" // new_line('A') // &
                     "            res = n * factorial(n - 1)" // new_line('A') // &
                     "        end if" // new_line('A') // &
                     "    end function factorial" // new_line('A') // &
                     "    subroutine test_subroutine()" // new_line('A') // &
                     "        integer, dimension(5) :: arr" // new_line('A') // &
                     "        arr = [1, 2, 3, 4, 5]" // new_line('A') // &
                     "        print *, 'Sum:', sum(arr)" // new_line('A') // &
                     "    end subroutine test_subroutine" // new_line('A') // &
                     "end program test_combined_features"

        call transform_lazy_fortran_string(input_code, output_code, error_msg)

        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "FAIL: transformation error ->", trim(error_msg)
            error stop 1
        end if

        contains_count = count_occurrences(output_code, 'contains')
        if (contains_count /= 1) then
            print *, "FAIL: expected single contains, found", contains_count
            print *, trim(output_code)
            error stop 1
        end if

        if (index(output_code, 'contains' // new_line('A') // 'contains') > 0) then
            print *, "FAIL: duplicate contains statements remain"
            print *, trim(output_code)
            error stop 1
        end if

        if (index(output_code, 'contains' // new_line('A') // new_line('A') // &
            'subroutine test_function') == 0) then
            print *, "FAIL: contains section formatting incorrect"
            print *, trim(output_code)
            error stop 1
        end if

        if (index(output_code, 'subroutine test_function') == 0) then
            print *, "FAIL: test_function not emitted"
            print *, trim(output_code)
            error stop 1
        end if

        if (index(output_code, 'recursive integer function factorial') == 0) then
            print *, "FAIL: factorial function not emitted"
            print *, trim(output_code)
            error stop 1
        end if

        if (index(output_code, 'subroutine test_subroutine') == 0) then
            print *, "FAIL: test_subroutine not emitted"
            print *, trim(output_code)
            error stop 1
        end if

        header_pos = index(output_code, 'subroutine test_function')
        decl_pos = index(output_code, 'integer :: result')
        if (decl_pos == 0 .or. decl_pos < header_pos) then
            print *, "FAIL: result declaration missing or misplaced"
            print *, trim(output_code)
            error stop 1
        end if

        header_pos = index(output_code, 'subroutine test_subroutine')
        decl_pos = index(output_code, 'integer :: arr(5)')
        if (decl_pos == 0 .or. decl_pos < header_pos) then
            print *, "FAIL: arr declaration missing or misplaced"
            print *, trim(output_code)
            error stop 1
        end if

        print *, "[PASS] multi internal procedure program retains structure"
    end subroutine test_program_with_multiple_internal_procedures

    integer function count_occurrences(text, pattern) result(count)
        character(*), intent(in) :: text
        character(*), intent(in) :: pattern
        character(:), allocatable :: trimmed_pattern
        integer :: start_pos
        integer :: found_pos

        count = 0
        trimmed_pattern = trim(pattern)
        if (len_trim(trimmed_pattern) == 0) return

        start_pos = 1
        do
            found_pos = index(text(start_pos:), trimmed_pattern)
            if (found_pos == 0) exit
            count = count + 1
            start_pos = start_pos + found_pos
            if (start_pos > len(text)) exit
        end do
    end function count_occurrences

end program test_issue_2025_internal_procedures
