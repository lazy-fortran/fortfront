program test_close_in_control_flow
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    print *, '=== CLOSE statements inside control flow constructs ==='

    all_passed = .true.
    if (.not. run_close_test('simple IF block', source_if_block(), 1)) &
        & all_passed = .false.
    if (.not. run_close_test('nested IF blocks', source_nested_if(), 1)) &
        & all_passed = .false.
    if (.not. run_close_test('DO loop', source_do_loop(), 1)) &
        & all_passed = .false.
    if (.not. run_close_test('SELECT CASE branches', source_select_case(), 2)) &
        & all_passed = .false.
    if (.not. run_close_test('multiple CLOSE statements', source_multiple_close(), 2)) &
        & all_passed = .false.

    if (all_passed) then
        print *, 'All control flow CLOSE tests passed!'
    else
        print *, 'Control flow CLOSE tests failed'
        stop 1
    end if

contains

    logical function run_close_test(name, source, expected_count)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: source
        integer, intent(in) :: expected_count
        character(len=:), allocatable :: output, error_msg, lowered
        integer :: close_hits

        run_close_test = .true.
        print *, 'Running:', trim(name)

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL:', trim(name), 'reported error'
                print *, '    ', trim(error_msg)
                run_close_test = .false.
                return
            end if
        end if

        lowered = to_lowercase(output)
        close_hits = count_occurrences(lowered, 'close(') + &
                     count_occurrences(lowered, 'close (')

        if (close_hits /= expected_count) then
            print *, '  FAIL:', trim(name), 'expected', expected_count, &
                     'CLOSE statements'
            print *, '    Observed:', close_hits
            print *, '    Output fragment:'
            print *, trim(output)
            run_close_test = .false.
        else
            print *, '  PASS:', trim(name)
        end if
    end function run_close_test

    function source_if_block() result(source)
        character(len=:), allocatable :: source
        source = 'program close_if_block' // new_line('a') // &
                 '    implicit none' // new_line('a') // &
                 '    integer :: unit' // new_line('a') // &
                 '    unit = 10' // new_line('a') // &
                 '    open(unit=unit, status=''scratch'')' // new_line('a') // &
                 '    if (unit > 0) then' // new_line('a') // &
                 '        close(unit)' // new_line('a') // &
                 '    end if' // new_line('a') // &
                 'end program close_if_block'
    end function source_if_block

    function source_nested_if() result(source)
        character(len=:), allocatable :: source
        source = 'program close_nested_if' // new_line('a') // &
                 '    implicit none' // new_line('a') // &
                 '    integer :: unit' // new_line('a') // &
                 '    unit = 20' // new_line('a') // &
                 '    open(unit=unit, status=''scratch'')' // new_line('a') // &
                 '    if (unit > 0) then' // new_line('a') // &
                 '        if (unit > 10) then' // new_line('a') // &
                 '            close(unit)' // new_line('a') // &
                 '        end if' // new_line('a') // &
                 '    end if' // new_line('a') // &
                 'end program close_nested_if'
    end function source_nested_if

    function source_do_loop() result(source)
        character(len=:), allocatable :: source
        source = 'program close_do_loop' // new_line('a') // &
                 '    implicit none' // new_line('a') // &
                 '    integer :: unit' // new_line('a') // &
                 '    integer :: i' // new_line('a') // &
                 '    unit = 30' // new_line('a') // &
                 '    open(unit=unit, status=''scratch'')' // new_line('a') // &
                 '    do i = 1, 2' // new_line('a') // &
                 '        if (i == 2) then' // new_line('a') // &
                 '            close(unit)' // new_line('a') // &
                 '        end if' // new_line('a') // &
                 '    end do' // new_line('a') // &
                 'end program close_do_loop'
    end function source_do_loop

    function source_select_case() result(source)
        character(len=:), allocatable :: source
        source = 'program close_select_case' // new_line('a') // &
                 '    implicit none' // new_line('a') // &
                 '    integer :: unit' // new_line('a') // &
                 '    integer :: choice' // new_line('a') // &
                 '    unit = 40' // new_line('a') // &
                 '    choice = 1' // new_line('a') // &
                 '    open(unit=unit, status=''scratch'')' // new_line('a') // &
                 '    select case (choice)' // new_line('a') // &
                 '    case (1)' // new_line('a') // &
                 '        close(unit)' // new_line('a') // &
                 '    case default' // new_line('a') // &
                 '        close(unit)' // new_line('a') // &
                 '    end select' // new_line('a') // &
                 'end program close_select_case'
    end function source_select_case

    function source_multiple_close() result(source)
        character(len=:), allocatable :: source
        source = 'program close_multiple' // new_line('a') // &
                 '    implicit none' // new_line('a') // &
                 '    integer :: unit' // new_line('a') // &
                 '    unit = 50' // new_line('a') // &
                 '    open(unit=unit, status=''scratch'')' // new_line('a') // &
                 '    if (unit > 0) then' // new_line('a') // &
                 '        close(unit)' // new_line('a') // &
                 '        close(unit)' // new_line('a') // &
                 '    end if' // new_line('a') // &
                 'end program close_multiple'
    end function source_multiple_close

    integer function count_occurrences(buffer, pattern) result(total)
        character(len=*), intent(in) :: buffer
        character(len=*), intent(in) :: pattern
        integer :: start, found, pattern_len

        total = 0
        pattern_len = len(pattern)
        if (pattern_len <= 0) return

        start = 1
        do
            if (start > len(buffer)) exit
            found = index(buffer(start:), pattern)
            if (found == 0) exit
            total = total + 1
            start = start + found + pattern_len - 1
        end do
    end function count_occurrences

    function to_lowercase(text) result(lowered)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: lowered
        character(len=len(text)) :: temp
        integer :: i, code_point

        temp = text
        do i = 1, len(temp)
            code_point = iachar(temp(i:i))
            if (code_point >= iachar('A') .and. code_point <= iachar('Z')) then
                temp(i:i) = achar(code_point + 32)
            end if
        end do
        lowered = temp
    end function to_lowercase

end program test_close_in_control_flow
