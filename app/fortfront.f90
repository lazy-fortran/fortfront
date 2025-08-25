program fortfront_cli
    use iso_fortran_env, only: input_unit, output_unit, error_unit, iostat_end
    use frontend, only: transform_lazy_fortran_string
    implicit none
    
    character(len=:), allocatable :: input_text, output_text, error_msg
    character(len=:), allocatable :: temp_text
    character(len=4096) :: buffer
    integer :: io_stat, total_size, capacity, line_len
    integer, parameter :: MAX_INPUT_SIZE = 10485760  ! 10MB safety limit
    integer, parameter :: INITIAL_CAPACITY = 8192
    
    ! Secure line-by-line reading with O(N) memory allocation
    capacity = INITIAL_CAPACITY
    allocate(character(len=capacity) :: input_text)
    total_size = 0
    
    do
        ! Read line by line for proper redirection and pipe support
        read(5, '(A)', iostat=io_stat) buffer
        
        ! Handle end conditions
        if (io_stat == iostat_end) exit
        if (io_stat > 0) then
            write(error_unit, '(A)') 'Error reading input'
            stop 1
        end if
        
        ! Get actual line length (trimmed content)
        line_len = len_trim(buffer)
        
        ! Security check: prevent memory exhaustion attacks  
        ! Account for content + newline + potential growth
        if (total_size + line_len + 1 > MAX_INPUT_SIZE) then
            write(error_unit, '(A,I0,A)') 'Input exceeds maximum size (', &
                MAX_INPUT_SIZE, ' bytes)'
            stop 1
        end if
        
        ! Grow buffer if needed (double when full)
        if (total_size + line_len + 1 > capacity) then
            do while (capacity < total_size + line_len + 1 .and. capacity <= MAX_INPUT_SIZE)
                capacity = min(capacity * 2, MAX_INPUT_SIZE)
            end do
            
            if (capacity > MAX_INPUT_SIZE) then
                write(error_unit, '(A,I0,A)') 'Input exceeds maximum size (', &
                    MAX_INPUT_SIZE, ' bytes)'
                stop 1
            end if
            
            ! Reallocate with larger capacity
            allocate(character(len=capacity) :: temp_text)
            if (total_size > 0) then
                temp_text(1:total_size) = input_text(1:total_size)
            end if
            call move_alloc(temp_text, input_text)
        end if
        
        ! Add line content if not empty
        if (line_len > 0) then
            input_text(total_size+1:total_size+line_len) = buffer(1:line_len)
            total_size = total_size + line_len
        end if
        
        ! Always add newline to preserve source structure
        input_text(total_size+1:total_size+1) = new_line('A')
        total_size = total_size + 1
    end do
    
    ! Trim to actual size to save memory
    if (total_size == 0) then
        allocate(character(len=0) :: temp_text)
    else
        allocate(character(len=total_size) :: temp_text)
        temp_text = input_text(1:total_size)
    end if
    call move_alloc(temp_text, input_text)
    
    ! Handle empty input gracefully - no special error messages needed
    ! Empty input is valid and should generate minimal program
    
    ! Transform lazy fortran to standard fortran
    call transform_lazy_fortran_string(input_text, output_text, error_msg)
    
    ! For syntax/parsing errors, the error information is included in output_text
    ! Only stop with error code for system-level failures
    if (error_msg /= "" .and. index(error_msg, "Cannot open") > 0) then
        ! System-level error (file I/O, etc.) - report to stderr and exit with error
        write(error_unit, '(A)') trim(error_msg)
        stop 1
    else if (error_msg /= "") then
        ! Syntax/parsing error - report to stderr but continue with output
        write(error_unit, '(A)') trim(error_msg)
    end if
    
    ! Write output to stdout
    if (allocated(output_text) .and. len(output_text) > 0) then
        write(output_unit, '(A)', advance='no') output_text
    else
        write(error_unit, '(A)') 'No output generated'
        stop 1
    end if
    
end program fortfront_cli