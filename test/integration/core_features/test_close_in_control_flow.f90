program test_close_in_control_flow
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    all_passed = .true.
    if (.not. run_close_test('simple IF block', 'examples/f90/close_if_block.f90', &
                             1)) all_passed = .false.
    if (.not. run_close_test('nested IF blocks', 'examples/f90/close_nested_if.f90', &
                             1)) all_passed = .false.
    if (.not. run_close_test('DO loop', 'examples/f90/close_do_loop.f90', 1)) &
        all_passed = .false.
    if (.not. run_close_test('SELECT CASE branches', &
                             'examples/f90/close_select_case.f90', 2)) &
        all_passed = .false.
    if (.not. run_close_test('multiple CLOSE statements', &
                             'examples/f90/close_multiple.f90', 2)) &
        all_passed = .false.

    if (all_passed) then
        print *, 'PASS: CLOSE statements preserved within control flow'
    else
        error stop 'FAIL: CLOSE control flow regression detected'
    end if

contains

    include '../../common/read_example.inc'


    logical function run_close_test(name, example_path, expected_count)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: example_path
        integer, intent(in) :: expected_count
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: lowered
        integer :: close_hits

        run_close_test = .true.

        call read_example(example_path, source)
        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                write (error_unit, '(A)') 'FAIL: ' // trim(name) // ' reported error'
                write (error_unit, '(A)') trim(error_msg)
                run_close_test = .false.
                return
            end if
        end if

        lowered = to_lowercase(output)
        close_hits = count_occurrences(lowered, 'close(') + &
                     count_occurrences(lowered, 'close (')

        if (close_hits /= expected_count) then
            write (error_unit, '(A,A,I0)') 'FAIL: ', trim(name) // ' expected ', &
                expected_count
            write (error_unit, '(A,I0)') 'Observed CLOSE statements: ', close_hits
            write (error_unit, '(A)') trim(output)
            run_close_test = .false.
        end if
    end function run_close_test

    integer function count_occurrences(buffer, pattern) result(total)
        character(len=*), intent(in) :: buffer
        character(len=*), intent(in) :: pattern
        integer :: start
        integer :: found
        integer :: pattern_len

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
        integer :: i
        integer :: code_point

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
