    subroutine read_all_stdin_or_file(from_file, filename, text, status)
        use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
        implicit none
        logical, intent(in) :: from_file
        character(len=*), intent(in), optional :: filename
        character(len=:), allocatable, intent(out) :: text
        integer, intent(out) :: status

        status = 0
        if (from_file) then
            if (.not. present(filename)) then
                write (error_unit, '(A)') 'Filename is required when reading from file'
                status = 1
                return
            end if
            call read_all_from_file(filename, text, status)
        else
            call read_all_from_unit(input_unit, text, status)
        end if
    end subroutine read_all_stdin_or_file

    subroutine read_all_from_file(filename, text, status)
        use, intrinsic :: iso_fortran_env, only: error_unit
        implicit none
        character(len=*), intent(in) :: filename
        character(len=:), allocatable, intent(out) :: text
        integer, intent(out) :: status
        character(len=:), allocatable :: open_filename
        character(len=128) :: os_value
        integer :: os_len, os_status, i
        integer :: unit_num, ios, file_size, alloc_stat
        integer, parameter :: MAX_INPUT_SIZE = 10485760

        status = 0
        open_filename = filename

        call get_environment_variable('OS', value=os_value, length=os_len, &
                                      status=os_status)
        if (os_status == 0) then
            if (trim(os_value(1:os_len)) == 'Windows_NT') then
                do i = 1, len(open_filename)
                    if (open_filename(i:i) == '/') open_filename(i:i) = '\'
                end do
            end if
        end if

        open (newunit=unit_num, file=open_filename, status='old', action='read', &
              access='stream', form='unformatted', iostat=ios)
        if (ios /= 0) then
            write (error_unit, '(A,A)') 'Cannot open file: ', filename
            status = 2
            return
        end if

        inquire (unit=unit_num, size=file_size)
        if (file_size < 0) then
            write (error_unit, '(A,A)') 'Could not determine size of file: ', filename
            status = 3
            close (unit_num)
            return
        end if

        if (file_size > MAX_INPUT_SIZE) then
            write (error_unit, '(A,I0,A)') 'Input exceeds maximum size (', &
                MAX_INPUT_SIZE, ' bytes)'
            status = 4
            close (unit_num)
            return
        end if

        allocate (character(len=file_size) :: text, stat=alloc_stat)
        if (alloc_stat /= 0) then
            write (error_unit, '(A,I0,A)') 'Failed to allocate text buffer (', &
                file_size, ' bytes)'
            status = 5
            close (unit_num)
            return
        end if

        if (file_size > 0) then
            read (unit_num, iostat=ios) text
            if (ios /= 0) then
                write (error_unit, '(A,A)') 'Failed to read file: ', filename
                status = 3
                close (unit_num)
                return
            end if
        end if
        close (unit_num)

        call cli_reader_strip_carriage_returns(text, status)
        if (status /= 0) return
    end subroutine read_all_from_file

    subroutine cli_reader_strip_carriage_returns(text, status)
        use, intrinsic :: iso_fortran_env, only: error_unit
        implicit none
        character(len=:), allocatable, intent(inout) :: text
        integer, intent(inout) :: status
        character(len=:), allocatable :: tmp
        integer :: i, out_len, pos, alloc_stat

        if (status /= 0) return

        out_len = 0
        do i = 1, len(text)
            if (text(i:i) /= achar(13)) out_len = out_len + 1
        end do

        allocate (character(len=out_len) :: tmp, stat=alloc_stat)
        if (alloc_stat /= 0) then
            write (error_unit, '(A,I0,A)') 'Failed to allocate text buffer (', &
                out_len, ' bytes)'
            status = 5
            return
        end if

        pos = 0
        do i = 1, len(text)
            if (text(i:i) /= achar(13)) then
                pos = pos + 1
                tmp(pos:pos) = text(i:i)
            end if
        end do

        call move_alloc(tmp, text)
    end subroutine cli_reader_strip_carriage_returns

    subroutine read_all_from_unit(unit_num, text, status)
        use, intrinsic :: iso_fortran_env, only: error_unit, iostat_end, iostat_eor
        implicit none
        integer, intent(in) :: unit_num
        character(len=:), allocatable, intent(out) :: text
        integer, intent(out) :: status
        character(len=:), allocatable :: buffer, tmp_text
        integer :: total_size, capacity, ios, chunk_size, alloc_stat
        integer, parameter :: INITIAL_CAPACITY = 8192
        integer, parameter :: BUFFER_CHUNK_SIZE = 4096

        status = 0
        capacity = INITIAL_CAPACITY
        total_size = 0

        allocate (character(len=capacity) :: text, stat=alloc_stat)
        if (alloc_stat /= 0) then
            write (error_unit, '(A,I0,A)') &
                'Failed to allocate input buffer (', capacity, ' bytes)'
            status = 5
            return
        end if
        allocate (character(len=BUFFER_CHUNK_SIZE) :: buffer, stat=alloc_stat)
        if (alloc_stat /= 0) then
            write (error_unit, '(A,I0,A)') &
                'Failed to allocate read buffer (', BUFFER_CHUNK_SIZE, ' bytes)'
            status = 5
            return
        end if

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
            allocate (character(len=0) :: tmp_text, stat=alloc_stat)
        else
            allocate (character(len=total_size) :: tmp_text, stat=alloc_stat)
            if (alloc_stat == 0) tmp_text = text(1:total_size)
        end if
        if (alloc_stat /= 0) then
            write (error_unit, '(A,I0,A)') &
                'Failed to allocate final text buffer (', total_size, ' bytes)'
            status = 5
            return
        end if
        call move_alloc(tmp_text, text)
    end subroutine read_all_from_unit

    subroutine append_chunk(chunk, text, total_size, capacity, status)
        use, intrinsic :: iso_fortran_env, only: error_unit
        implicit none
        character(len=*), intent(in) :: chunk
        character(len=:), allocatable, intent(inout) :: text
        integer, intent(inout) :: total_size, capacity
        integer, intent(inout) :: status
        character(len=:), allocatable :: tmp
        integer :: needed, alloc_stat
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
            allocate (character(len=capacity) :: tmp, stat=alloc_stat)
            if (alloc_stat /= 0) then
                write (error_unit, '(A,I0,A)') &
                    'Failed to allocate memory for input expansion (', capacity, &
                    ' bytes)'
                status = 5
                return
            end if
            if (total_size > 0) tmp(1:total_size) = text(1:total_size)
            call move_alloc(tmp, text)
        end if
        text(total_size + 1:total_size + len(chunk)) = chunk
        total_size = total_size + len(chunk)
    end subroutine append_chunk

    subroutine append_newline(text, total_size, capacity, status)
        use, intrinsic :: iso_fortran_env, only: error_unit
        implicit none
        character(len=:), allocatable, intent(inout) :: text
        integer, intent(inout) :: total_size, capacity
        integer, intent(inout) :: status
        character(len=:), allocatable :: tmp
        integer :: needed, alloc_stat
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
            allocate (character(len=capacity) :: tmp, stat=alloc_stat)
            if (alloc_stat /= 0) then
                write (error_unit, '(A,I0,A)') &
                    'Failed to allocate memory for newline expansion (', capacity, &
                    ' bytes)'
                status = 5
                return
            end if
            if (total_size > 0) tmp(1:total_size) = text(1:total_size)
            call move_alloc(tmp, text)
        end if
        text(total_size + 1:total_size + 1) = new_line('A')
        total_size = total_size + 1
    end subroutine append_newline
