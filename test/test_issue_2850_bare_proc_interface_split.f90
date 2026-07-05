program test_issue_2850_bare_proc_interface_split
    use, intrinsic :: iso_fortran_env, only: error_unit
    use string_utils_mod, only: to_lower
    use transformation_api, only: transform_context_t, transform_with_context, &
        INPUT_MODE_STANDARD
    implicit none

    character(len=:), allocatable :: source_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    type(transform_context_t) :: ctx

    ! Test 1: bare subroutine with interface block
    call read_example('examples/f90/issue_2850_bare_subroutine_interface.f90', &
        source_code)

    ctx%input_mode = INPUT_MODE_STANDARD
    ctx%has_filename = .true.
    ctx%source_name = 'issue_2850_bare_subroutine_interface'

    call transform_with_context(source_code, output_code, error_msg, ctx)
    call assert_no_error(error_msg)

    ! Must NOT produce a duplicate "program main" node
    call assert_no_duplicate_program(output_code, &
        'bare subroutine with interface must not split into duplicate program')

    ! Must contain the full subroutine with all statements
    call assert_contains(output_code, 'call callee(x)', &
        'subroutine body must include statements after interface block')
    call assert_contains(output_code, 'x = 1', &
        'subroutine body must include assignment after interface block')

    ! Test 2: bare function with interface block
    call read_example('examples/f90/issue_2850_bare_function_interface.f90', &
        source_code)

    ctx%source_name = 'issue_2850_bare_function_interface'

    call transform_with_context(source_code, output_code, error_msg, ctx)
    call assert_no_error(error_msg)

    call assert_no_duplicate_program(output_code, &
        'bare function with interface must not split into duplicate program')
    call assert_contains(output_code, 'y = 2', &
        'function body must include statements after interface block')

    print *, 'PASS: Issue #2850 - no duplicate program_node for bare proc with interface'

contains

    include 'common/read_example.inc'

    subroutine assert_no_error(message)
        character(len=:), allocatable, intent(in) :: message
        if (.not. allocated(message)) return
        if (len_trim(message) == 0) return
        write (error_unit, '(A)') 'FAIL: ' // trim(message)
        error stop 1
    end subroutine assert_no_error

    subroutine assert_contains(text, pattern, failure_message)
        character(len=:), allocatable, intent(in) :: text
        character(len=*), intent(in) :: pattern
        character(len=*), intent(in) :: failure_message
        if (index(to_lower(text), to_lower(pattern)) == 0) then
            write (error_unit, '(A)') trim(failure_message)
            call print_debug(text)
            error stop 1
        end if
    end subroutine assert_contains

    subroutine assert_no_duplicate_program(output_code, failure_message)
        character(len=:), allocatable, intent(in) :: output_code
        character(len=*), intent(in) :: failure_message
        integer :: program_count, i
        character(len=:), allocatable :: lines(:)
        character(len=100) :: lower_line

        call split_lines(output_code, lines)
        program_count = 0
        do i = 1, size(lines)
            lower_line = to_lower(adjustl(lines(i)))
            if (lower_line == 'program main' .or. &
                (len(lower_line) >= 7 .and. lower_line(1:7) == 'program' .and. &
                lower_line(8:8) == ' ')) then
                program_count = program_count + 1
            end if
        end do
        if (program_count >= 1) then
            write (error_unit, '(A,I0,A)') 'FAIL: spurious program node(s): ', &
                program_count, ' (expected 0 for bare procedure)'
            call print_debug(output_code)
            error stop 1
        end if
    end subroutine assert_no_duplicate_program

    subroutine split_lines(text, lines)
        character(len=:), allocatable, intent(in) :: text
        character(len=:), allocatable, intent(out) :: lines(:)
        integer :: count, i, start, idx, line_len

        count = 1
        do i = 1, len(text)
            if (text(i:i) == new_line('A')) count = count + 1
        end do
        line_len = len(text)
        allocate (character(len=line_len) :: lines(count))
        start = 1
        idx = 1
        do i = 1, len(text)
            if (text(i:i) == new_line('A')) then
                lines(idx) = text(start:i - 1)
                idx = idx + 1
                start = i + 1
            end if
        end do
        lines(idx) = text(start:)
    end subroutine split_lines

    subroutine print_debug(text)
        character(len=:), allocatable, intent(in) :: text
        write (error_unit, '(A)') '---- output ----'
        write (error_unit, '(A)') trim(text)
        write (error_unit, '(A)') '----------------'
    end subroutine print_debug

end program test_issue_2850_bare_proc_interface_split
