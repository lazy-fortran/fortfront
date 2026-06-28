program test_issue_2010_deferred_char_allocatable
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=*), parameter :: example_path = 'examples/f90/' // &
        'issue_2010_deferred_char_allocatable.f90'
    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    character(len=512) :: line_buffer
    integer :: unit
    integer :: ios
    logical :: first_line

    source = ''
    first_line = .true.
    open (newunit=unit, file=example_path, status='old', action='read', &
        iostat=ios)
    if (ios /= 0) then
        print *, 'FAIL: could not open example file'
        stop 1
    end if

    do
        read (unit, '(A)', iostat=ios) line_buffer
        if (ios /= 0) exit
        if (first_line) then
            source = trim(line_buffer)
            first_line = .false.
        else
            source = source // new_line('a') // trim(line_buffer)
        end if
    end do
    close (unit)

    if (first_line) then
        print *, 'FAIL: example file was empty'
        stop 1
    end if

    source = source // new_line('a')

    call transform_lazy_fortran_string(source, output, error_msg)
    call assert_no_error(error_msg)
    call assert_contains(output, 'allocate(character(len=n) :: str)')
    call assert_not_contains(output, 'allocate()')
    call assert_type_spec_only_in_allocate(output)

    print *, 'PASS: Issue #2010 - deferred-length allocate preserved'

contains

    subroutine assert_no_error(message)
        character(len=:), allocatable, intent(in) :: message

        if (allocated(message)) then
            if (len_trim(message) > 0) then
                print *, 'FAIL: transformation error:', trim(message)
                stop 1
            end if
        end if
    end subroutine assert_no_error

    subroutine assert_contains(text, substring)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: substring

        if (index(text, substring) == 0) then
            print *, 'FAIL: expected substring missing:', trim(substring)
            stop 1
        end if
    end subroutine assert_contains

    subroutine assert_not_contains(text, substring)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: substring

        if (index(text, substring) /= 0) then
            print *, 'FAIL: unexpected substring present:', trim(substring)
            stop 1
        end if
    end subroutine assert_not_contains

    subroutine assert_type_spec_only_in_allocate(text)
        character(len=*), intent(in) :: text
        character(len=*), parameter :: target = 'character(len=n) :: str'
        integer :: search_start
        integer :: relative_pos
        integer :: absolute_pos

        search_start = 1
        do
            relative_pos = index(text(search_start:), target)
            if (relative_pos == 0) exit
            absolute_pos = search_start + relative_pos - 1
            if (absolute_pos <= 1) then
                print *, 'FAIL: type-spec appeared outside allocate:', target
                stop 1
            end if
            if (text(absolute_pos - 1:absolute_pos - 1) /= "(") then
                print *, 'FAIL: duplicated declaration detected:', target
                stop 1
            end if
            search_start = absolute_pos + len(target)
        end do
    end subroutine assert_type_spec_only_in_allocate

end program test_issue_2010_deferred_char_allocatable
