program test_issue_2389_end_statements
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        & iostat_eor
    use string_utils_mod, only: to_lower
    use transformation_api, only: transform_context_t, transform_with_context, &
        & INPUT_MODE_STANDARD
    implicit none

    character(len=:), allocatable :: source_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    type(transform_context_t) :: ctx

    call read_example('examples/f90/issue_2389_interface_unit_split.f90', &
        & source_code)

    ctx%input_mode = INPUT_MODE_STANDARD
    ctx%has_filename = .true.
    ctx%source_name = 'issue_2389_interface_unit_split'

    call transform_with_context(source_code, output_code, error_msg, ctx)
    call assert_no_error(error_msg)

    call assert_contains(output_code, 'end interface', &
        & 'FAIL: missing end interface marker')
    call assert_contains(output_code, 'subroutine setup_value', &
        & 'FAIL: procedure setup_value missing after transform')
    call assert_interface_attached(output_code)

    print *, 'PASS: Issue #2389 END statements stay aligned with interface blocks'

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
            error stop 1
        end if
    end subroutine assert_contains

    subroutine assert_interface_attached(output_code)
        character(len=:), allocatable, intent(in) :: output_code
        character(len=:), allocatable :: lines(:)
        integer :: line_count
        integer :: i
        logical :: saw_end_interface

        call split_lines(output_code, lines, line_count)

        saw_end_interface = .false.
        do i = 1, line_count
            if (trim(adjustl(to_lower(lines(i)))) == 'end interface') then
                saw_end_interface = .true.
                exit
            end if
        end do

        if (.not. saw_end_interface) then
            write (error_unit, '(A)') 'FAIL: end interface not found in output'
            error stop 1
        end if

        do i = i + 1, line_count
            if (len_trim(lines(i)) == 0) cycle
            if (trim(adjustl(to_lower(lines(i)))) == 'end') then
                call print_debug_output(output_code)
                write (error_unit, '(A)') 'FAIL: stray END inserted after interface'
                error stop 1
            else if (index(adjustl(to_lower(lines(i))), 'integer :: value') == 1) then
                return
            else
                return
            end if
        end do

        write (error_unit, '(A)') &
            'FAIL: main body not preserved immediately after interface block'
        error stop 1
    end subroutine assert_interface_attached

    subroutine split_lines(text, lines, count)
        character(len=:), allocatable, intent(in) :: text
        character(len=:), allocatable, intent(out) :: lines(:)
        integer, intent(out) :: count
        integer :: i, start, idx, line_len

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

    subroutine print_debug_output(text)
        character(len=:), allocatable, intent(in) :: text

        write (error_unit, '(A)') '---- transformed output ----'
        write (error_unit, '(A)') trim(text)
        write (error_unit, '(A)') '----------------------------'
    end subroutine print_debug_output

end program test_issue_2389_end_statements
