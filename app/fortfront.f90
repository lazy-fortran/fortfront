program fortfront_cli
    use, intrinsic :: iso_fortran_env, only: input_unit, output_unit, error_unit, iostat_end
    use frontend, only: transform_lazy_fortran_string
    use debug_trace, only: trace_init, trace_enter, trace_leave
    use cli_env, only: init_cli_trace
    use cli_io, only: read_all_stdin_or_file
    implicit none
    
    character(len=:), allocatable :: input_text, output_text, error_msg
    character(len=:), allocatable :: temp_text, arg_str, filename
    integer :: io_stat
    integer :: alloc_stat
    integer :: num_args, arg_len, i
    integer, parameter :: EXIT_SUCCESS = 0
    integer, parameter :: EXIT_FAILURE = 1
    logical :: from_file, show_help, show_version
    logical :: trace_enabled
    character(len=:), allocatable :: trace_file_path
    call init_cli_trace(trace_enabled, trace_file_path)
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
    
    ! Read input (from file or stdin) using robust chunked reader
    call trace_enter('cli:read_input')
    if (from_file) then
        call read_all_stdin_or_file(.true., filename, input_text, io_stat)
    else
        call read_all_stdin_or_file(.false., text=input_text, status=io_stat)
    end if
    call trace_leave('cli:read_input')
    block
        character(len=64) :: tmp_msg
        write(tmp_msg, '("CLI: read input done (bytes=",I0,")")') merge(len(input_text), 0, allocated(input_text))
        call cli_trace(tmp_msg)
    end block

    ! Trim to actual size to save memory
    ! input_text is already trimmed by reader
    
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

    ! input reading utilities moved to cli_io module
end program fortfront_cli
