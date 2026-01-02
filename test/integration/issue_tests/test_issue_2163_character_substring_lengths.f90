program test_issue_2163_character_substring_lengths
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Issue #2163: Character substring length inference ==='

    if (.not. check_character_substring_lengths()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'Issue #2163 fixed!'
    else
        print *, 'Issue #2163 regression detected!'
        stop 1
    end if

contains

    include '../../common/cli_io_reader.inc'
    include '../../common/read_example.inc'


    logical function check_character_substring_lengths()
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg
        logical :: text_ok
        logical :: first_ok
        logical :: last_ok
        logical :: middle_ok

        check_character_substring_lengths = .true.

        call read_example('examples/lf/issue_2163_character_substring_lengths.lf', &
                          source)
        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                write (error_unit, '(A,A)') '  FAIL: Unexpected error - ', &
                    trim(error_msg)
                check_character_substring_lengths = .false.
                return
            end if
        end if

        if (.not. allocated(output)) then
            write (error_unit, '(A)') '  FAIL: No output generated'
            check_character_substring_lengths = .false.
            return
        end if

        text_ok = contains_typed_variable(output, 'character(len=11)', 'text')
        first_ok = contains_typed_variable(output, 'character(len=5)', 'first')
        last_ok = contains_typed_variable(output, 'character(len=5)', 'last')
        middle_ok = contains_typed_variable(output, 'character(len=7)', 'middle')

        if (.not. text_ok) then
            write (error_unit, '(A)') '  FAIL: Missing declaration for text'
            check_character_substring_lengths = .false.
        end if

        if (.not. first_ok) then
            write (error_unit, '(A)') '  FAIL: first not inferred as len=5'
            check_character_substring_lengths = .false.
        end if

        if (.not. last_ok) then
            write (error_unit, '(A)') '  FAIL: last not inferred as len=5'
            check_character_substring_lengths = .false.
        end if

        if (.not. middle_ok) then
            write (error_unit, '(A)') '  FAIL: middle not inferred as len=7'
            check_character_substring_lengths = .false.
        end if

        if (check_character_substring_lengths) then
            print *, '  PASS: substring lengths inferred correctly'
        end if
    end function check_character_substring_lengths

    logical function contains_typed_variable(text, type_spec, var_name)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: type_spec
        character(len=*), intent(in) :: var_name
        integer :: start_pos
        integer :: segment_len
        character(len=:), allocatable :: line

        contains_typed_variable = .false.
        start_pos = 1

        do
            if (start_pos > len(text)) exit
            segment_len = index(text(start_pos:), new_line('a'))
            if (segment_len == 0) then
                line = text(start_pos:)
                start_pos = len(text) + 1
            else
                line = text(start_pos:start_pos + segment_len - 2)
                start_pos = start_pos + segment_len
            end if

            if (len(line) == 0) cycle
            if (index(line, trim(type_spec)) > 0) then
                if (index(line, trim(var_name)) > 0) then
                    contains_typed_variable = .true.
                    return
                end if
            end if
        end do
    end function contains_typed_variable

end program test_issue_2163_character_substring_lengths
