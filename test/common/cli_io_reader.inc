    subroutine read_all_stdin_or_file(from_file, filename, text, status)
        logical, intent(in) :: from_file
        character(len=*), intent(in), optional :: filename
        character(len=:), allocatable, intent(out) :: text
        integer, intent(out) :: status
        integer :: unit_num, ios

        status = 0
        if (from_file) then
            if (.not. present(filename)) then
                write (error_unit, '(A)') 'Filename is required when reading from file'
                status = 1
                return
            end if
            open (newunit=unit_num, file=filename, status='old', action='read', &
                iostat=ios)
            if (ios /= 0) then
                write (error_unit, '(A,A)') 'Cannot open file: ', filename
                status = 2
                return
            end if
            call read_all_from_unit(unit_num, text, status)
            close (unit_num)
        else
            call read_all_from_unit(input_unit, text, status)
        end if
    end subroutine read_all_stdin_or_file

    subroutine read_all_from_unit(unit_num, text, status)
        integer, intent(in) :: unit_num
        character(len=:), allocatable, intent(out) :: text
        integer, intent(out) :: status
        character(len=:), allocatable :: buffer, tmp_text
        integer :: total_size, capacity, ios, chunk_size
        integer, parameter :: INITIAL_CAPACITY = 8192
        integer, parameter :: BUFFER_CHUNK_SIZE = 4096

        status = 0
        capacity = INITIAL_CAPACITY
        total_size = 0

        allocate (character(len=capacity) :: text)
        allocate (character(len=BUFFER_CHUNK_SIZE) :: buffer)

        do
            do
                read (unit_num, '(A)', advance='no', iostat=ios, size=chunk_size) &
                    buffer
                select case (ios)
                case (iostat_end)
                    if (chunk_size > 0) then
                        call append_chunk(buffer(1:chunk_size), text, total_size, &
                            capacity, status)
                        if (status /= 0) return
                    end if
                    exit
                case (iostat_eor)
                    if (chunk_size > 0) then
                        call append_chunk(buffer(1:chunk_size), text, total_size, &
                            capacity, status)
                        if (status /= 0) return
                    end if
                    call append_newline(text, total_size, capacity, status)
                    if (status /= 0) return
                    exit
                case (0)
                    if (chunk_size > 0) then
                        call append_chunk(buffer(1:chunk_size), text, total_size, &
                            capacity, status)
                        if (status /= 0) return
                    end if
                case default
                    write (error_unit, '(A,I0,A)') 'Error reading input (iostat=', &
                        ios, ')'
                    status = 3
                    return
                end select
            end do
            if (ios == iostat_end) exit
        end do

        if (total_size == 0) then
            allocate (character(len=0) :: tmp_text)
        else
            allocate (character(len=total_size) :: tmp_text)
            tmp_text = text(1:total_size)
        end if
        call move_alloc(tmp_text, text)
    end subroutine read_all_from_unit

    subroutine append_chunk(chunk, text, total_size, capacity, status)
        character(len=*), intent(in) :: chunk
        character(len=:), allocatable, intent(inout) :: text
        integer, intent(inout) :: total_size, capacity
        integer, intent(inout) :: status
        character(len=:), allocatable :: tmp
        integer :: needed
        integer, parameter :: MAX_INPUT_SIZE = 10485760

        needed = total_size + len(chunk)
        if (needed > MAX_INPUT_SIZE) then
            write (error_unit, '(A,I0,A)') 'Input exceeds maximum size (', &
                MAX_INPUT_SIZE, ' bytes)'
            status = 4
            return
        end if
        if (needed > capacity) then
            do while (capacity < needed .and. capacity < MAX_INPUT_SIZE)
                capacity = min(capacity * 2, MAX_INPUT_SIZE)
            end do
            if (capacity < needed) then
                write (error_unit, '(A)') 'Input too large'
                status = 4
                return
            end if
            allocate (character(len=capacity) :: tmp)
            if (total_size > 0) tmp(1:total_size) = text(1:total_size)
            call move_alloc(tmp, text)
        end if
        text(total_size + 1:total_size + len(chunk)) = chunk
        total_size = total_size + len(chunk)
    end subroutine append_chunk

    subroutine append_newline(text, total_size, capacity, status)
        character(len=:), allocatable, intent(inout) :: text
        integer, intent(inout) :: total_size, capacity
        integer, intent(inout) :: status
        character(len=:), allocatable :: tmp
        integer :: needed
        integer, parameter :: MAX_INPUT_SIZE = 10485760

        needed = total_size + 1
        if (needed > MAX_INPUT_SIZE) then
            write (error_unit, '(A,I0,A)') 'Input exceeds maximum size (', &
                MAX_INPUT_SIZE, ' bytes)'
            status = 4
            return
        end if
        if (needed > capacity) then
            do while (capacity < needed .and. capacity < MAX_INPUT_SIZE)
                capacity = min(capacity * 2, MAX_INPUT_SIZE)
            end do
            if (capacity < needed) then
                write (error_unit, '(A)') 'Input too large'
                status = 4
                return
            end if
            allocate (character(len=capacity) :: tmp)
            if (total_size > 0) tmp(1:total_size) = text(1:total_size)
            call move_alloc(tmp, text)
        end if
        text(total_size + 1:total_size + 1) = new_line('A')
        total_size = total_size + 1
    end subroutine append_newline
