program fortfront_cli
    use iso_fortran_env, only: input_unit, output_unit, error_unit, iostat_end
    use frontend, only: transform_lazy_fortran_string
    implicit none
    
    character(len=:), allocatable :: input_text, output_text, error_msg
    character(len=:), allocatable :: temp_text, arg_str, filename
    character(len=4096) :: buffer
    integer :: io_stat, total_size, capacity, line_len, file_unit
    integer :: num_args, arg_len, i
    integer, parameter :: MAX_INPUT_SIZE = 10485760  ! 10MB safety limit
    integer, parameter :: INITIAL_CAPACITY = 8192
    logical :: from_file, show_help, show_version
    
    ! Process command line arguments
    num_args = command_argument_count()
    show_help = .false.
    show_version = .false.
    from_file = .false.
    
    ! Handle command line arguments
    if (num_args > 0) then
        call get_command_argument(1, length=arg_len)
        allocate(character(len=arg_len) :: arg_str)
        call get_command_argument(1, value=arg_str)
        
        if (arg_str == "--help" .or. arg_str == "-h") then
            show_help = .true.
        else if (arg_str == "--version" .or. arg_str == "-v") then
            show_version = .true.
        else
            ! Treat as filename
            from_file = .true.
            filename = arg_str
        end if
    end if
    
    ! Handle help option
    if (show_help) then
        write(output_unit, '(A)') 'fortfront - Lazy Fortran to Standard Fortran Transpiler'
        write(output_unit, '(A)') ''
        write(output_unit, '(A)') 'USAGE:'
        write(output_unit, '(A)') '    fortfront [OPTIONS] [FILE]'
        write(output_unit, '(A)') ''
        write(output_unit, '(A)') 'ARGUMENTS:'
        write(output_unit, '(A)') '    FILE    Input file (reads from stdin if not specified)'
        write(output_unit, '(A)') ''
        write(output_unit, '(A)') 'OPTIONS:'
        write(output_unit, '(A)') '    -h, --help     Show this help message'
        write(output_unit, '(A)') '    -v, --version  Show version information'
        write(output_unit, '(A)') ''
        write(output_unit, '(A)') 'EXAMPLES:'
        write(output_unit, '(A)') '    fortfront input.lf        # Transpile file'
        write(output_unit, '(A)') '    cat input.lf | fortfront  # Transpile from stdin'
        write(output_unit, '(A)') '    echo "x = 5" | fortfront  # Transpile string'
        stop 0
    end if
    
    ! Handle version option
    if (show_version) then
        write(output_unit, '(A)') 'fortfront 0.1.0'
        write(output_unit, '(A)') 'Lazy Fortran to Standard Fortran Transpiler'
        write(output_unit, '(A)') 'https://github.com/krystophny/fortfront'
        stop 0
    end if
    
    ! Read input (from file or stdin)
    capacity = INITIAL_CAPACITY
    allocate(character(len=capacity) :: input_text)
    total_size = 0
    
    if (from_file) then
        ! Read from file
        open(newunit=file_unit, file=filename, status='old', action='read', &
             iostat=io_stat)
        if (io_stat /= 0) then
            write(error_unit, '(A,A)') 'Cannot open file: ', filename
            stop 1
        end if
        
        do
            read(file_unit, '(A)', iostat=io_stat) buffer
            if (io_stat == iostat_end) exit
            if (io_stat > 0) then
                write(error_unit, '(A,A)') 'Error reading file: ', filename
                close(file_unit)
                stop 1
            end if
            
            call append_line_to_input(buffer, input_text, total_size, capacity)
        end do
        close(file_unit)
    else
        ! Read from stdin (original behavior preserved)
        do
            read(input_unit, '(A)', iostat=io_stat) buffer
            if (io_stat == iostat_end) exit
            if (io_stat > 0) then
                write(error_unit, '(A)') 'Error reading input'
                stop 1
            end if
            
            call append_line_to_input(buffer, input_text, total_size, capacity)
        end do
    end if
    
    ! Trim to actual size to save memory
    if (total_size == 0) then
        allocate(character(len=0) :: temp_text)
    else
        allocate(character(len=total_size) :: temp_text)
        temp_text = input_text(1:total_size)
    end if
    call move_alloc(temp_text, input_text)
    
    ! Transform lazy fortran to standard fortran
    call transform_lazy_fortran_string(input_text, output_text, error_msg)
    
    ! Handle errors
    if (error_msg /= "" .and. index(error_msg, "Cannot open") > 0) then
        write(error_unit, '(A)') trim(error_msg)
        stop 1
    else if (error_msg /= "") then
        write(error_unit, '(A)') trim(error_msg)
    end if
    
    ! Write output to stdout
    if (allocated(output_text) .and. len(output_text) > 0) then
        write(output_unit, '(A)', advance='no') output_text
    else
        write(error_unit, '(A)') 'No output generated'
        stop 1
    end if

contains

    subroutine append_line_to_input(buffer, input_text, total_size, capacity)
        character(len=*), intent(in) :: buffer
        character(len=:), allocatable, intent(inout) :: input_text
        integer, intent(inout) :: total_size, capacity
        character(len=:), allocatable :: temp_text
        integer :: line_len
        
        line_len = len_trim(buffer)
        
        ! Security check: prevent memory exhaustion attacks
        if (total_size + line_len + 1 > MAX_INPUT_SIZE) then
            write(error_unit, '(A,I0,A)') 'Input exceeds maximum size (', &
                MAX_INPUT_SIZE, ' bytes)'
            stop 1
        end if
        
        ! Grow buffer if needed
        if (total_size + line_len + 1 > capacity) then
            do while (capacity < total_size + line_len + 1 .and. &
                     capacity <= MAX_INPUT_SIZE)
                capacity = min(capacity * 2, MAX_INPUT_SIZE)
            end do
            
            if (capacity > MAX_INPUT_SIZE) then
                write(error_unit, '(A,I0,A)') 'Input exceeds maximum size (', &
                    MAX_INPUT_SIZE, ' bytes)'
                stop 1
            end if
            
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
    end subroutine append_line_to_input
    
end program fortfront_cli