module cli_io
    use, intrinsic :: iso_fortran_env, only: error_unit, iostat_end, iostat_eor, input_unit
    implicit none
    private

    integer, parameter :: MAX_INPUT_SIZE = 10485760  ! 10MB safety limit
    integer, parameter :: INITIAL_CAPACITY = 8192

    public :: read_all_from_unit
    public :: read_all_stdin_or_file

contains

    subroutine read_all_stdin_or_file(from_file, filename, text, status)
        logical, intent(in) :: from_file
        character(len=*), intent(in), optional :: filename
        character(len=:), allocatable, intent(out) :: text
        integer, intent(out) :: status
        integer :: unit, ios

        status = 0
        if (from_file) then
            if (.not. present(filename)) then
                write(error_unit, '(A)') 'read_all_stdin_or_file: filename is required when from_file=.true.'
                status = 1
                return
            end if
            open (newunit=unit, file=filename, status='old', action='read', iostat=ios)
            if (ios /= 0) then
                write (error_unit, '(A,A)') 'Cannot open file: ', filename
                status = 2
                return
            end if
            call read_all_from_unit(unit, text, status)
            close (unit)
        else
            call read_all_from_unit(input_unit, text, status)
        end if
    end subroutine read_all_stdin_or_file

    subroutine read_all_from_unit(unit, text, status)
        integer, intent(in) :: unit
        character(len=:), allocatable, intent(out) :: text
        integer, intent(out) :: status
        character(len=:), allocatable :: buffer, temp_text
        integer :: total_size, capacity
        integer :: ios, sz

        status = 0
        capacity = INITIAL_CAPACITY
        total_size = 0
        allocate (character(len=capacity) :: text)

        allocate (character(len=4096) :: buffer)

        do
            ! Read one logical record in chunks using non-advancing I/O
            do
                read (unit, '(A)', advance='no', iostat=ios, size=sz) buffer

                if (ios == iostat_end) then
                    if (sz > 0) then
                        call append_chunk(buffer(1:sz), text, total_size, capacity, status)
                        if (status /= 0) return
                    end if
                    exit  ! End of file
                else if (ios == iostat_eor) then
                    if (sz > 0) then
                        call append_chunk(buffer(1:sz), text, total_size, capacity, status)
                        if (status /= 0) return
                    end if
                    call append_newline(text, total_size, capacity, status)
                    if (status /= 0) return
                    exit  ! End of record
                else if (ios == 0) then
                    if (sz > 0) then
                        call append_chunk(buffer(1:sz), text, total_size, capacity, status)
                        if (status /= 0) return
                    end if
                else
                    write (error_unit, '(A,I0,A)') 'Error reading input (iostat=', ios, ')'
                    status = 3
                    return
                end if
            end do

            if (ios == iostat_end) exit
        end do

        ! Trim to actual size
        if (total_size == 0) then
            allocate (character(len=0) :: temp_text)
        else
            allocate (character(len=total_size) :: temp_text)
            temp_text = text(1:total_size)
        end if
        call move_alloc(temp_text, text)
    end subroutine read_all_from_unit

    subroutine append_chunk(chunk, text, total_size, capacity, status)
        character(len=*), intent(in) :: chunk
        character(len=:), allocatable, intent(inout) :: text
        integer, intent(inout) :: total_size, capacity
        integer, intent(inout) :: status
        character(len=:), allocatable :: tmp
        integer :: need

        need = total_size + len(chunk)
        if (need > MAX_INPUT_SIZE) then
            write (error_unit, '(A,I0,A)') 'Input exceeds maximum size (', MAX_INPUT_SIZE, ' bytes)'
            status = 4
            return
        end if
        if (need > capacity) then
            do while (capacity < need .and. capacity < MAX_INPUT_SIZE)
                capacity = min(capacity * 2, MAX_INPUT_SIZE)
            end do
            if (capacity < need) then
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
        integer :: need
        need = total_size + 1
        if (need > MAX_INPUT_SIZE) then
            write (error_unit, '(A,I0,A)') 'Input exceeds maximum size (', MAX_INPUT_SIZE, ' bytes)'
            status = 4
            return
        end if
        if (need > capacity) then
            do while (capacity < need .and. capacity < MAX_INPUT_SIZE)
                capacity = min(capacity * 2, MAX_INPUT_SIZE)
            end do
            if (capacity < need) then
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

end module cli_io
