program fortfront_cli
    use, intrinsic :: iso_fortran_env, only: input_unit, output_unit, error_unit
    use frontend, only: transform_lazy_fortran_string
    use debug_trace, only: trace_init, trace_enter, trace_leave
    use cli_env, only: init_cli_trace, parse_trace_option, parse_trace_flag_value
    use cli_io, only: read_all_stdin_or_file
    use process_exit, only: exit_quiet
    implicit none
    
    character(len=:), allocatable :: input_text, output_text, error_msg
    character(len=:), allocatable :: arg_str, filename
    integer :: io_stat
    integer :: alloc_stat
    integer :: num_args, arg_len, i
    integer :: next_len
    integer, parameter :: EXIT_SUCCESS = 0
    integer, parameter :: EXIT_FAILURE = 1
    logical :: from_file, show_help, show_version
    logical :: trace_enabled
    character(len=:), allocatable :: trace_file_path
    logical :: has_trace_override, has_trace_file_override
    character(len=:), allocatable :: trace_file_opt
    character(len=:), allocatable :: next_arg
    character(len=:), allocatable :: optval
    logical :: end_of_options
    call init_cli_trace(trace_enabled, trace_file_path)
    call cli_trace('CLI: start')
    call trace_init()
    call trace_enter('cli:main')
    ! Process command line arguments
    num_args = command_argument_count()
    show_help = .false.
    show_version = .false.
    from_file = .false.
    has_trace_override = .false.
    has_trace_file_override = .false.
    end_of_options = .false.

    ! Handle command line arguments
    if (num_args > 0) then
        i = 1
        do while (i <= num_args)
            call get_command_argument(i, length=arg_len)
            allocate(character(len=arg_len) :: arg_str, stat=alloc_stat)
            if (alloc_stat /= 0) then
                write(error_unit, '(A,I0,A)') &
                    'Memory allocation failed for command argument (stat=', &
                    alloc_stat, ')'
                call exit_quiet(EXIT_FAILURE)
            end if
            call get_command_argument(i, value=arg_str)

            if (.not. end_of_options) then
                select case (trim(arg_str))
                case ('--')
                    end_of_options = .true.
                    deallocate(arg_str, stat=alloc_stat)
                    if (alloc_stat /= 0) then
                        write(error_unit, '(A,I0,A)') &
                            'Memory deallocation failed for command argument (stat=', &
                            alloc_stat, ')'
                        call exit_quiet(EXIT_FAILURE)
                    end if
                    i = i + 1
                    cycle
                case ('--help', '-h')
                    show_help = .true.
                case ('--version', '-v')
                    show_version = .true.
                case default
                    block
                        logical :: rec, is_file
                        call parse_trace_option(trim(arg_str), rec, is_file, optval)
                        if (rec) then
                            if (is_file) then
                                if (len_trim(optval) == 0) then
                                    if (i < num_args) then
                                        call get_command_argument(i+1, length=next_len)
                                        allocate(character(len=next_len) :: next_arg)
                                        call get_command_argument(i+1, value=next_arg)
                                        if (len_trim(next_arg) == 0 .or. next_arg(1:1) == '-') then
                                            write(error_unit, '(A)') 'Error: --trace-file requires a path'
                                            call exit_quiet(EXIT_FAILURE)
                                        end if
                                        trace_file_opt = next_arg
                                        has_trace_file_override = .true.
                                        deallocate(next_arg)
                                        i = i + 1
                                    else
                                        write(error_unit, '(A)') 'Error: --trace-file requires a path'
                                        call exit_quiet(EXIT_FAILURE)
                                    end if
                                else
                                    trace_file_opt = optval
                                    has_trace_file_override = .true.
                                end if
                            else
                                trace_enabled = parse_trace_flag_value(optval)
                                has_trace_override = .true.
                            end if
                            ! advance to next argument and clean up current
                            deallocate(arg_str, stat=alloc_stat)
                            if (alloc_stat /= 0) then
                                write(error_unit, '(A,I0,A)') &
                                    'Memory deallocation failed for command argument (stat=', &
                                    alloc_stat, ')'
                                call exit_quiet(EXIT_FAILURE)
                            end if
                            i = i + 1
                            cycle
                        end if
                    end block
                    if ((len(arg_str) >= 1 .and. arg_str(1:1) == '-')) then
                        write(error_unit, '(A,A)') 'Error: Unknown option ', trim(arg_str)
                        write(error_unit, '(A)') ''
                        write(error_unit, '(A)') 'Try ''fortfront --help'' for usage information.'
                        call exit_quiet(EXIT_FAILURE)
                    end if
                end select
            end if

            ! Treat as filename (honoring end_of_options or non-option token)
            if (from_file) then
                write(error_unit, '(A)') 'Error: Multiple input files not supported.'
                write(error_unit, '(A)') 'fortfront processes one file at a time or reads from stdin.'
                write(error_unit, '(A)') ''
                write(error_unit, '(A)') 'Try ''fortfront --help'' for usage information.'
                call exit_quiet(EXIT_FAILURE)
            end if
            from_file = .true.
            filename = arg_str

            deallocate(arg_str, stat=alloc_stat)
            if (alloc_stat /= 0) then
                write(error_unit, '(A,I0,A)') &
                    'Memory deallocation failed for command argument (stat=', &
                    alloc_stat, ')'
                call exit_quiet(EXIT_FAILURE)
            end if
            i = i + 1
        end do
    end if

    
    ! Handle help option
    if (show_help) then
        write(output_unit, '(A)') 'fortfront - Lazy Fortran to Standard Fortran Transpiler'
        write(output_unit, '(A)') ''
        write(output_unit, '(A)') 'USAGE:'
        write(output_unit, '(A)') '    fortfront [OPTIONS] [--] [FILE]'
        write(output_unit, '(A)') ''
        write(output_unit, '(A)') 'ARGUMENTS:'
        write(output_unit, '(A)') '    FILE    Input file (reads from stdin if not specified)'
        write(output_unit, '(A)') ''
        write(output_unit, '(A)') 'OPTIONS:'
        write(output_unit, '(A)') '    -h, --help     Show this help message'
        write(output_unit, '(A)') '    -v, --version  Show version information'
        write(output_unit, '(A)') '        --trace[=on|off]   Enable/disable tracing (overrides env)'
        write(output_unit, '(A)') '        --trace-file <path>  Trace output file (overrides env)'
        write(output_unit, '(A)') '        --   End of options; treat following token as FILE even if it starts with -'
        write(output_unit, '(A)') ''
        write(output_unit, '(A)') 'EXAMPLES:'
        write(output_unit, '(A)') '    fortfront input.lf        # Transpile file'
        write(output_unit, '(A)') '    cat input.lf | fortfront  # Transpile from stdin'
        write(output_unit, '(A)') '    echo "x = 5" | fortfront  # Transpile string'
        write(output_unit, '(A)') '    fortfront -- -file.lf     # Filename begins with a hyphen'
        call trace_leave('cli:main')
        call exit_quiet(EXIT_SUCCESS)
    end if
    
    ! Handle version option
    if (show_version) then
        write(output_unit, '(A)') 'fortfront 0.1.0'
        write(output_unit, '(A)') 'Lazy Fortran to Standard Fortran Transpiler'
        write(output_unit, '(A)') 'https://github.com/lazy-fortran/fortfront'
        call trace_leave('cli:main')
        call exit_quiet(EXIT_SUCCESS)
    end if
    
    ! Read input (from file or stdin) using robust chunked reader
    call trace_enter('cli:read_input')
    if (from_file) then
        call read_all_stdin_or_file(.true., filename, input_text, io_stat)
    else
        call read_all_stdin_or_file(.false., text=input_text, status=io_stat)
    end if
    call trace_leave('cli:read_input')
    if (io_stat /= 0) then
        call trace_leave('cli:main')
        call exit_quiet(EXIT_FAILURE)
    end if
    block
        character(len=64) :: tmp_msg
        write(tmp_msg, '("CLI: read input done (bytes=",I0,")")') merge(len(input_text), 0, allocated(input_text))
        call cli_trace(tmp_msg)
    end block

    ! Trim to actual size to save memory
    ! input_text is already trimmed by reader
    
    ! Apply CLI trace overrides if provided
    if (has_trace_override) then
        if (.not. trace_enabled) then
            ! Explicit disable wins regardless of file override
            if (allocated(trace_file_path)) deallocate(trace_file_path)
        end if
    end if
    if (has_trace_file_override) then
        if (allocated(trace_file_path)) deallocate(trace_file_path)
        trace_file_path = trim(trace_file_opt)
    end if

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
                call exit_quiet(EXIT_FAILURE)
            end if
            ! Unrecognized input reports are advisory only; continue with success to
            ! match historical CLI behaviour for pipeline fallbacks.
        end if
    end if

    ! If no output was generated and no error was reported, treat as failure
    if (.not. allocated(output_text) .or. len(output_text) == 0) then
        write(error_unit, '(A)') 'No output generated'
        call exit_quiet(EXIT_FAILURE)
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
