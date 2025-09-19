program fortfront_cli
    use iso_fortran_env, only: input_unit, output_unit, error_unit, iostat_end
    use frontend, only: transform_lazy_fortran_string
    use debug_trace, only: trace_init, trace_enter, trace_leave
    implicit none
    
    character(len=:), allocatable :: input_text, output_text, error_msg
    character(len=:), allocatable :: temp_text, arg_str, filename
    character(len=4096) :: buffer
    integer :: io_stat, total_size, capacity, file_unit
    integer :: alloc_stat
    integer :: num_args, arg_len, i
    integer, parameter :: MAX_INPUT_SIZE = 10485760  ! 10MB safety limit
    integer, parameter :: INITIAL_CAPACITY = 8192
    integer, parameter :: EXIT_SUCCESS = 0
    integer, parameter :: EXIT_FAILURE = 1
    logical :: from_file, show_help, show_version
    logical :: trace_enabled
    character(len=:), allocatable :: trace_file_path
    trace_enabled = .true.
    trace_file_path = 'cli_trace.txt'
    block
        integer :: s
        character(len=8) :: tv
        character(len=512) :: tf
        call get_environment_variable('FORTFRONT_TRACE', tv, status=s)
        if (s == 0) then
            call get_environment_variable('FORTFRONT_TRACE_FILE', tf, status=s)
            if (s == 0 .and. len_trim(tf) > 0) trace_file_path = trim(tf)
        end if
    end block
    call cli_trace('CLI: start')
    call trace_init()
    call trace_enter('cli:main')
    ! Process command line arguments
    num_args = command_argument_count()
    show_help = .false.
    show_version = .false.
    from_file = .false.

    ! Handle command line arguments
    if (num_args > 0) then
        do i = 1, num_args
            call get_command_argument(i, length=arg_len)
            allocate(character(len=arg_len) :: arg_str, stat=alloc_stat)
            if (alloc_stat /= 0) then
                write(error_unit, '(A,I0)') 'Memory allocation failed for command argument (stat=', alloc_stat, ')'
                stop EXIT_FAILURE
            end if
            call get_command_argument(i, value=arg_str)

            select case (trim(arg_str))
            case ('--help', '-h')
                show_help = .true.
            case ('--version', '-v')
                show_version = .true.
            case default
                if ((len(arg_str) >= 1 .and. arg_str(1:1) == '-')) then
                    write(error_unit, '(A,A)') 'Error: Unknown option ', trim(arg_str)
                    write(error_unit, '(A)') ''
                    write(error_unit, '(A)') 'Try ''fortfront --help'' for usage information.'
                    stop EXIT_FAILURE
                end if

                if (from_file) then
                    write(error_unit, '(A)') 'Error: Multiple input files not supported.'
                    write(error_unit, '(A)') 'fortfront processes one file at a time or reads from stdin.'
                    write(error_unit, '(A)') ''
                    write(error_unit, '(A)') 'Try ''fortfront --help'' for usage information.'
                    stop EXIT_FAILURE
                end if

                from_file = .true.
                filename = arg_str
            end select

            deallocate(arg_str, stat=alloc_stat)
            if (alloc_stat /= 0) then
                write(error_unit, '(A,I0)') 'Memory deallocation failed for command argument (stat=', alloc_stat, ')'
                stop EXIT_FAILURE
            end if
        end do
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
        call trace_leave('cli:main')
        stop EXIT_SUCCESS
    end if
    
    ! Handle version option
    if (show_version) then
        write(output_unit, '(A)') 'fortfront 0.1.0'
        write(output_unit, '(A)') 'Lazy Fortran to Standard Fortran Transpiler'
        write(output_unit, '(A)') 'https://github.com/lazy-fortran/fortfront'
        call trace_leave('cli:main')
        stop EXIT_SUCCESS
    end if
    
    ! Read input (from file or stdin)
    capacity = INITIAL_CAPACITY
    allocate(character(len=capacity) :: input_text, stat=alloc_stat)
    if (alloc_stat /= 0) then
        write(error_unit, '(A,I0)') 'Memory allocation failed for input buffer (stat=', alloc_stat, ')'
        stop EXIT_FAILURE
    end if
    total_size = 0
    
    call trace_enter('cli:read_input')
    if (from_file) then
        ! Read from file
        open(newunit=file_unit, file=filename, status='old', action='read', &
             iostat=io_stat)
        if (io_stat /= 0) then
            write(error_unit, '(A,A)') 'Cannot open file: ', filename
            stop EXIT_FAILURE
        end if
        
        do
            read(file_unit, '(A)', iostat=io_stat) buffer
            if (io_stat == iostat_end) exit
            if (io_stat > 0) then
                write(error_unit, '(A,A)') 'Error reading file: ', filename
                close(file_unit)
                stop EXIT_FAILURE
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
                stop EXIT_FAILURE
            end if
            
            call append_line_to_input(buffer, input_text, total_size, capacity)
        end do
    end if
    call trace_leave('cli:read_input')
    block
        character(len=64) :: tmp_msg
        write(tmp_msg, '("CLI: read input done (bytes=",I0,")")') total_size
        call cli_trace(tmp_msg)
    end block

    ! Trim to actual size to save memory
    if (total_size == 0) then
        allocate(character(len=0) :: temp_text, stat=alloc_stat)
        if (alloc_stat /= 0) then
            write(error_unit, '(A,I0)') 'Memory allocation failed for temp buffer (stat=', alloc_stat, ')'
            stop EXIT_FAILURE
        end if
    else
        allocate(character(len=total_size) :: temp_text, stat=alloc_stat)
        if (alloc_stat /= 0) then
            write(error_unit, '(A,I0)') 'Memory allocation failed for sized temp buffer (stat=', alloc_stat, ')'
            stop EXIT_FAILURE
        end if
        temp_text = input_text(1:total_size)
    end if
    call move_alloc(temp_text, input_text)
    
    ! Transform lazy fortran to standard fortran
    call trace_enter('cli:transform')
    call cli_trace('CLI: transform begin')
    call transform_lazy_fortran_string(input_text, output_text, error_msg)
    call trace_leave('cli:transform')
    call cli_trace('CLI: transform end')

    ! Always write any generated output to stdout first
    if (allocated(output_text) .and. len(output_text) > 0) then
        write(output_unit, '(A)', advance='no') output_text
        ! Ensure a trailing newline for line-buffered environments (e.g., Windows pipes)
        if (output_text(len(output_text):len(output_text)) /= new_line('A')) then
            write(output_unit, '(A)') ''
        end if
        flush(output_unit)
    end if

    ! Handle errors: print diagnostics and return non-zero exit
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            write(error_unit, '(A)') trim(error_msg)
            if (index(error_msg, '[SYNTAX_ERROR]') > 0 .or. &
                index(error_msg, '[VALIDATION') > 0 .or. &
                index(error_msg, '[PARSER_') > 0) then
                stop EXIT_FAILURE
            end if
            ! Unrecognized input reports are advisory only; continue with success to
            ! match historical CLI behaviour for pipeline fallbacks.
        end if
    end if

    ! If no output was generated and no error was reported, treat as failure
    if (.not. allocated(output_text) .or. len(output_text) == 0) then
        write(error_unit, '(A)') 'No output generated'
        stop EXIT_FAILURE
    end if

    call trace_leave('cli:main')

contains

    subroutine cli_trace(message)
        character(len=*), intent(in) :: message
        integer :: unit_id, ios
        if (.not. trace_enabled) return
        if (.not. allocated(trace_file_path)) return
        open(newunit=unit_id, file=trace_file_path, status='unknown', position='append', action='write', iostat=ios)
        if (ios /= 0) return
        write(unit_id, '(A)') trim(message)
        flush(unit_id)
        close(unit_id)
    end subroutine cli_trace

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
            stop EXIT_FAILURE
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
            stop EXIT_FAILURE
            end if
            
            allocate(character(len=capacity) :: temp_text, stat=alloc_stat)
            if (alloc_stat /= 0) then
                write(error_unit, '(A,I0)') 'Memory allocation failed while growing input buffer (stat=', alloc_stat, ')'
                stop EXIT_FAILURE
            end if
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
