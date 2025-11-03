program test_issue_1971_inquiry_intrinsics
    use fortfront
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    logical :: success

    print *, "=== Codegen: inquiry intrinsics return scalar integers ==="

    call read_example('examples/lf/issue_1971_inquiry_intrinsics.lf', source)
    call transform_lazy_fortran_string(source, output, error_msg)

    success = .true.
    if (.not. allocated(output)) success = .false.
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) success = .false.
    end if

    if (success) then
        if (.not. has_integer_scalars(output, [character(len=8) :: 'lb', 'n', &
                                               'ub'])) then
            success = .false.
        end if
        if (index(output, 'allocatable ::') /= 0) success = .false.
        if (index(output, 'size(a)') == 0) success = .false.
        if (index(output, 'lbound(a, 1)') == 0) success = .false.
        if (index(output, 'ubound(a, 1)') == 0) success = .false.
    end if

    if (success) then
        print *, "PASSED"
    else
        print *, "FAILED: inquiry intrinsic inference incorrect"
        if (allocated(output)) then
            print *, "OUTPUT:"
            print *, trim(output)
        end if
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "ERRORS:"
                print *, trim(error_msg)
            end if
        end if
        stop 1
    end if

contains

    logical function has_integer_scalars(text, names)
        character(len=*), intent(in) :: text
        character(len=*), dimension(:), intent(in) :: names
        integer :: pos
        integer :: start_pos
        integer :: end_pos
        integer :: text_len
        integer :: i
        character(len=:), allocatable :: line
        character(len=:), allocatable :: tail
        character(len=64) :: token
        integer :: sep
        logical, allocatable :: found(:)
        character(1), parameter :: nl = new_line('a')
        integer, parameter :: prefix_len = 10

        text_len = len(text)
        allocate (found(size(names)))
        found = .false.

        pos = index(text, 'integer ::')
        do while (pos > 0)
            start_pos = pos
            do while (start_pos > 1 .and. text(start_pos - 1:start_pos - 1) /= nl)
                start_pos = start_pos - 1
            end do

            end_pos = pos
            do while (end_pos <= text_len .and. text(end_pos:end_pos) /= nl)
                end_pos = end_pos + 1
            end do

            if (end_pos > text_len) then
                line = text(start_pos:)
            else
                line = text(start_pos:end_pos - 1)
            end if

            line = adjustl(line)
            if (index(line, 'integer ::') == 1) then
                if (len(line) > prefix_len) then
                    tail = adjustl(line(prefix_len + 1:))
                else
                    tail = ''
                end if

                do
                    if (len_trim(tail) == 0) exit
                    sep = index(tail, ',')
                    if (sep == 0) then
                        token = trim(tail)
                        tail = ''
                    else
                        token = trim(tail(1:sep - 1))
                        tail = adjustl(tail(sep + 1:))
                    end if

                    if (len_trim(token) == 0) cycle
                    do i = 1, size(names)
                        if (.not. found(i)) then
                            if (trim(token) == trim(names(i))) then
                                found(i) = .true.
                            end if
                        end if
                    end do
                end do
            end if

            if (end_pos > text_len) exit
            pos = index(text(end_pos:), 'integer ::')
            if (pos > 0) pos = pos + end_pos - 1
        end do

        has_integer_scalars = all(found)
    end function has_integer_scalars

    subroutine read_example(filepath, content)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: content
        integer :: unit, ios, file_size
        character(len=1), allocatable :: buffer(:)
        integer :: i

        open (newunit=unit, file=filepath, status='old', &
              access='stream', form='unformatted', iostat=ios)
        if (ios /= 0) error stop 'Failed to open example file'

        inquire (unit=unit, size=file_size)
        allocate (buffer(file_size))
        read (unit, iostat=ios) buffer
        close (unit)

        if (ios /= 0) error stop 'Failed to read example file'

        allocate (character(len=file_size) :: content)
        do i = 1, file_size
            content(i:i) = buffer(i)
        end do
    end subroutine read_example

end program test_issue_1971_inquiry_intrinsics
