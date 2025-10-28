program test_close_in_control_flow
    use fortfront, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    implicit none

    logical :: all_passed

    print *, '=== CLOSE statements inside control flow constructs ==='

    all_passed = .true.
    if (.not. run_close_test('simple IF block', &
                             'examples/f90/close_if_block.f90', 1)) &
        & all_passed = .false.
    if (.not. run_close_test('nested IF blocks', &
                             'examples/f90/close_nested_if.f90', 1)) &
        & all_passed = .false.
    if (.not. run_close_test('DO loop', &
                             'examples/f90/close_do_loop.f90', 1)) &
        & all_passed = .false.
    if (.not. run_close_test('SELECT CASE branches', &
                             'examples/f90/close_select_case.f90', 2)) &
        & all_passed = .false.
    if (.not. run_close_test('multiple CLOSE statements', &
                             'examples/f90/close_multiple.f90', 2)) &
        & all_passed = .false.

    if (all_passed) then
        print *, 'All control flow CLOSE tests passed!'
    else
        print *, 'Control flow CLOSE tests failed'
        stop 1
    end if

contains

    logical function run_close_test(name, example_path, expected_count)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: example_path
        integer, intent(in) :: expected_count
        character(len=:), allocatable :: source, output, error_msg, lowered
        integer :: close_hits

        run_close_test = .true.
        print *, 'Running:', trim(name)

        call read_example(example_path, source)
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

    subroutine read_example(path, content)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: content
        integer :: unit, ios, file_size
        character(len=1), allocatable :: buffer(:)

        ! Open file and get size
        open(newunit=unit, file=path, status='old', action='read', &
             form='formatted', iostat=ios)
        if (ios /= 0) then
            print *, 'FAIL: failed to open ', trim(path)
            error stop 1
        end if

        ! Read entire file
        inquire(unit=unit, size=file_size)
        if (file_size > 0) then
            allocate(buffer(file_size))
            read(unit, '(A)', iostat=ios) buffer
        end if

        ! Read file line by line into content
        rewind(unit)
        content = ''
        do
            block
                character(len=10000) :: line
                read(unit, '(A)', iostat=ios) line
                if (ios == iostat_end) exit
                if (ios /= 0) then
                    print *, 'FAIL: error reading ', trim(path)
                    close(unit)
                    error stop 1
                end if
                if (len(content) > 0) then
                    content = content // new_line('a') // trim(line)
                else
                    content = trim(line)
                end if
            end block
        end do

        close(unit)
    end subroutine read_example

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
