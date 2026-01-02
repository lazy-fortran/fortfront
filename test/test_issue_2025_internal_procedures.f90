program test_issue_2025_internal_procedures
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    call test_program_with_multiple_internal_procedures()
    print *, ''
    print *, 'Issue 2025 internal procedure tests completed.'

contains

    include 'common/read_example.inc'


    subroutine test_program_with_multiple_internal_procedures()
        character(len=:), allocatable :: input_code
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg
        integer :: contains_count
        integer :: header_pos
        integer :: decl_pos

        call read_example( &
            'examples/f90/issue_2025_multi_internal_proc_structure_collapse.f90', &
            input_code)

        call transform_lazy_fortran_string(input_code, output_code, error_msg)

        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: transformation error -> ' // &
                trim(error_msg)
            error stop 1
        end if

        contains_count = count_occurrences(output_code, 'contains')
        if (contains_count /= 1) then
            write (error_unit, '(A,I0)') &
                'FAIL: expected single contains, found ', contains_count
            write (error_unit, '(A)') trim(output_code)
            error stop 1
        end if

        if (index(output_code, 'contains'//new_line('A')//'contains') > 0) &
            then
            write (error_unit, '(A)') 'FAIL: duplicate contains statements remain'
            write (error_unit, '(A)') trim(output_code)
            error stop 1
        end if

        if (index(output_code, 'contains'//new_line('A')//new_line('A')// &
                  '    subroutine test_function') == 0) then
            write (error_unit, '(A)') 'FAIL: contains section formatting incorrect'
            write (error_unit, '(A)') trim(output_code)
            error stop 1
        end if

        if (index(output_code, 'subroutine test_function') == 0) then
            write (error_unit, '(A)') 'FAIL: test_function not emitted'
            write (error_unit, '(A)') trim(output_code)
            error stop 1
        end if

        if (index(output_code, 'recursive integer function factorial') == 0) then
            write (error_unit, '(A)') 'FAIL: factorial function not emitted'
            write (error_unit, '(A)') trim(output_code)
            error stop 1
        end if

        if (index(output_code, 'subroutine test_subroutine') == 0) then
            write (error_unit, '(A)') 'FAIL: test_subroutine not emitted'
            write (error_unit, '(A)') trim(output_code)
            error stop 1
        end if

        header_pos = index(output_code, 'subroutine test_function')
        decl_pos = index(output_code, 'integer :: result')
        if (decl_pos == 0 .or. decl_pos < header_pos) then
            write (error_unit, '(A)') &
                'FAIL: result declaration missing or misplaced'
            write (error_unit, '(A)') trim(output_code)
            error stop 1
        end if

        header_pos = index(output_code, 'subroutine test_subroutine')
        decl_pos = index(output_code, 'integer :: arr(5)')
        if (decl_pos == 0 .or. decl_pos < header_pos) then
            write (error_unit, '(A)') &
                'FAIL: arr declaration missing or misplaced'
            write (error_unit, '(A)') trim(output_code)
            error stop 1
        end if

        print *, '[PASS] multi internal procedure program retains structure'
    end subroutine test_program_with_multiple_internal_procedures

    integer function count_occurrences(text, pattern) result(count)
        character(*), intent(in) :: text
        character(*), intent(in) :: pattern
        character(len=:), allocatable :: trimmed_pattern
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
