program fortfront_cli
    use, intrinsic :: iso_fortran_env, only: &
        input_unit, output_unit, error_unit, iostat_end, iostat_eor
    use transformation_api, only: transform_lazy_fortran_string, &
                                  transform_with_context, transform_context_t, &
                                  INPUT_MODE_LAZY, &
                                  INPUT_MODE_STANDARD, OPERATING_MODE_INFER, &
                                  OPERATING_MODE_STRICT
    use debug_trace, only: trace_finalize, trace_init, trace_enter, trace_leave
    use cli_env, only: init_cli_trace, parse_trace_option, parse_trace_flag_value
    use process_exit, only: exit_quiet
    use lexer_api, only: token_t, tokenize_core, TK_KEYWORD, &
                         TK_IDENTIFIER, to_lower
    use stdout_sanitizer, only: sanitize_redirected_stdout
    use frontend_core, only: normalize_fixed_form_source_text, &
                             is_fixed_form_file
    use frontend_program_unit_detection, only: detect_explicit_program_unit
    use frontend_tooling_api, only: read_file_contents, message_has_error
    implicit none

    character(len=:), allocatable :: input_text, output_text, error_msg
    character(len=:), allocatable :: arg_str, filename
    integer :: io_stat
    integer :: alloc_stat
    integer :: num_args, arg_len, i
    integer :: next_len
    integer, parameter :: EXIT_SUCCESS = 0
    integer, parameter :: EXIT_FAILURE = 1
    integer, parameter :: MAX_INPUT_SIZE = 10485760
    integer, parameter :: INITIAL_CAPACITY = 8192
    integer, parameter :: BUFFER_CHUNK_SIZE = 4096
    logical :: from_file, show_help, show_version
    logical :: trace_enabled
    character(len=:), allocatable :: trace_file_path
    logical :: has_trace_override, has_trace_file_override
    character(len=:), allocatable :: trace_file_opt
    character(len=:), allocatable :: next_arg
    character(len=:), allocatable :: optval
    logical :: end_of_options
    logical :: input_is_empty
    integer :: operating_mode
    logical :: has_fatal_error
    call init_cli_trace(trace_enabled, trace_file_path)
    call cli_trace('CLI: start')
    call trace_init()
    call trace_enter('cli:main')
    call sanitize_redirected_stdout()
    ! Process command line arguments
    num_args = command_argument_count()
    show_help = .false.
    show_version = .false.
    from_file = .false.
    has_trace_override = .false.
    has_trace_file_override = .false.
    end_of_options = .false.
    operating_mode = OPERATING_MODE_INFER
    has_fatal_error = .false.

    ! Handle command line arguments
    if (num_args > 0) then
        i = 1
        do while (i <= num_args)
            call get_command_argument(i, length=arg_len)
            allocate (character(len=arg_len) :: arg_str, stat=alloc_stat)
            if (alloc_stat /= 0) then
                write (error_unit, '(A,I0,A)') &
                    'Memory allocation failed for command argument (stat=', &
                    alloc_stat, ')'
                call exit_quiet(EXIT_FAILURE)
            end if
            call get_command_argument(i, value=arg_str)

            if (.not. end_of_options) then
                select case (to_lower(trim(arg_str)))
                case ('--')
                    end_of_options = .true.
                    deallocate (arg_str, stat=alloc_stat)
                    if (alloc_stat /= 0) then
                        call report_deallocation_failure('command argument', &
                                                         alloc_stat)
                    end if
                    i = i + 1
                    cycle
                case ('--help', '-h')
                    show_help = .true.
                    deallocate (arg_str, stat=alloc_stat)
                    if (alloc_stat /= 0) then
                        call report_deallocation_failure('command argument', &
                                                         alloc_stat)
                    end if
                    i = i + 1
                    cycle
                case ('--version', '-v')
                    show_version = .true.
                    deallocate (arg_str, stat=alloc_stat)
                    if (alloc_stat /= 0) then
                        call report_deallocation_failure('command argument', &
                                                         alloc_stat)
                    end if
                    i = i + 1
                    cycle
                case ('--infer')
                    operating_mode = OPERATING_MODE_INFER
                    deallocate (arg_str, stat=alloc_stat)
                    if (alloc_stat /= 0) then
                        call report_deallocation_failure('command argument', &
                                                         alloc_stat)
                    end if
                    i = i + 1
                    cycle
                case ('--std=lf')
                    operating_mode = OPERATING_MODE_STRICT
                    deallocate (arg_str, stat=alloc_stat)
                    if (alloc_stat /= 0) then
                        call report_deallocation_failure('command argument', &
                                                         alloc_stat)
                    end if
                    i = i + 1
                    cycle
                case ('--std')
                    if (i < num_args) then
                        call get_command_argument(i + 1, length=next_len)
                        allocate (character(len=next_len) :: next_arg)
                        call get_command_argument(i + 1, value=next_arg)
                        if (len_trim(next_arg) == 0) then
                            write (error_unit, '(A)') &
                                'Error: --std requires a value (expected lf)'
                            call exit_quiet(EXIT_FAILURE)
                        end if
                        if (next_arg(1:1) == '-') then
                            write (error_unit, '(A)') &
                                'Error: --std requires a value (expected lf)'
                            call exit_quiet(EXIT_FAILURE)
                        end if
                        select case (to_lower(trim(next_arg)))
                        case ('lf')
                            operating_mode = OPERATING_MODE_STRICT
                        case default
                            write (error_unit, '(A,A)') 'Error: Unknown --std value ', &
                                trim(next_arg)
                            call exit_quiet(EXIT_FAILURE)
                        end select
                        deallocate (next_arg)
                        deallocate (arg_str, stat=alloc_stat)
                        if (alloc_stat /= 0) then
                            call report_deallocation_failure('command argument', &
                                                             alloc_stat)
                        end if
                        i = i + 2
                        cycle
                    end if
                    write (error_unit, '(A)') &
                        'Error: --std requires a value (expected lf)'
                    call exit_quiet(EXIT_FAILURE)
                case default
                    block
                        logical :: rec, is_file
                        call parse_trace_option(trim(arg_str), rec, is_file, optval)
                        if (rec) then
                            if (is_file) then
                                if (len_trim(optval) == 0) then
                                    if (i < num_args) then
                                        call get_command_argument(i + 1, &
                                                                  length=next_len)
                                        allocate (character(len=next_len) :: next_arg)
                                        call get_command_argument(i + 1, &
                                                                  value=next_arg)
                                        if (len_trim(next_arg) == 0) then
                                            write (error_unit, '(A)') &
                                                'Error: --trace-file requires a path'
                                            call exit_quiet(EXIT_FAILURE)
                                        end if
                                        if (next_arg(1:1) == '-') then
                                            write (error_unit, '(A)') &
                                                'Error: --trace-file requires a path'
                                            call exit_quiet(EXIT_FAILURE)
                                        end if
                                        trace_file_opt = next_arg
                                        has_trace_file_override = .true.
                                        deallocate (next_arg)
                                        i = i + 1
                                    else
                                        write (error_unit, '(A)') &
                                            'Error: --trace-file requires a path'
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
                            deallocate (arg_str, stat=alloc_stat)
                            if (alloc_stat /= 0) then
                                call report_deallocation_failure('command argument', &
                                                                 alloc_stat)
                            end if
                            i = i + 1
                            cycle
                        end if
                    end block
                    if (len(arg_str) >= 1) then
                        if (arg_str(1:1) == '-') then
                            write (error_unit, '(A,A)') 'Error: Unknown option ', &
                                trim(arg_str)
                            write (error_unit, '(A)') ''
                            write (error_unit, '(A)') &
                                'Try ''fortfront --help'' for usage information.'
                            call exit_quiet(EXIT_FAILURE)
                        end if
                    end if
                end select
            end if

            ! Treat as filename (honoring end_of_options or non-option token)
            if (from_file) then
                write (error_unit, '(A)') 'Error: Multiple input files not supported.'
                write (error_unit, '(A)') &
                    'fortfront processes one file at a time or reads from stdin.'
                write (error_unit, '(A)') ''
                write (error_unit, '(A)') &
                    'Try ''fortfront --help'' for usage information.'
                call exit_quiet(EXIT_FAILURE)
            end if
            from_file = .true.
            filename = arg_str

            deallocate (arg_str, stat=alloc_stat)
            if (alloc_stat /= 0) then
                write (error_unit, '(A,I0,A)') &
                    'Memory deallocation failed for command argument (stat=', &
                    alloc_stat, ')'
                call exit_quiet(EXIT_FAILURE)
            end if
            i = i + 1
        end do
    end if

    ! Handle help option
    if (show_help) then
        write (output_unit, '(A)') &
            'fortfront - Lazy Fortran to Standard Fortran Transpiler'
        write (output_unit, '(A)') ''
        write (output_unit, '(A)') 'USAGE:'
        write (output_unit, '(A)') '    fortfront [OPTIONS] [--] [FILE]'
        write (output_unit, '(A)') ''
        write (output_unit, '(A)') 'ARGUMENTS:'
        write (output_unit, '(A)') &
            '    FILE    Input file (reads from stdin if not specified)'
        write (output_unit, '(A)') ''
        write (output_unit, '(A)') 'OPTIONS:'
        write (output_unit, '(A)') '    -h, --help     Show this help message'
        write (output_unit, '(A)') '    -v, --version  Show version information'
        write (output_unit, '(A)') '        --infer    Enable infer mode (default)'
        write (output_unit, '(A)') '        --std=lf   Enable strict mode'
        write (output_unit, '(A)') '        --std lf   Enable strict mode'
        write (output_unit, '(A)') &
            '        --trace[=on|off]   Enable/disable tracing (overrides env)'
        write (output_unit, '(A)') &
            '        --trace-file <path>  Trace output file (overrides env)'
        write (output_unit, '(A)') &
            '        --   End of options; treat following token as FILE'
        write (output_unit, '(A)') &
            '             even if it starts with -'
        write (output_unit, '(A)') ''
        write (output_unit, '(A)') 'EXAMPLES:'
        write (output_unit, '(A)') '    fortfront input.lf        # Transpile file'
        write (output_unit, '(A)') &
            '    cat input.lf | fortfront  # Transpile from stdin'
        write (output_unit, '(A)') '    echo "x = 5" | fortfront  # Transpile string'
        write (output_unit, '(A)') &
            '    fortfront -- -file.lf     # Filename begins with a hyphen'
        call trace_leave('cli:main')
        call exit_quiet(EXIT_SUCCESS)
    end if

    ! Handle version option
    if (show_version) then
        write (output_unit, '(A)') 'fortfront 0.1.0'
        write (output_unit, '(A)') 'Lazy Fortran to Standard Fortran Transpiler'
        write (output_unit, '(A)') 'https://github.com/lazy-fortran/fortfront'
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
    if (allocated(input_text)) then
        input_is_empty = (len_trim(input_text) == 0)
    else
        input_is_empty = .true.
    end if
    block
        character(len=64) :: tmp_msg
        integer :: byte_count

        if (allocated(input_text)) then
            byte_count = len(input_text)
        else
            byte_count = 0
        end if
        write (tmp_msg, '("CLI: read input done (bytes=",I0,")")') byte_count
        call cli_trace(tmp_msg)
    end block

    ! Trim to actual size to save memory
    ! input_text is already trimmed by reader

    ! Apply CLI trace overrides if provided
    if (has_trace_override) then
        if (.not. trace_enabled) then
            ! Explicit disable wins regardless of file override
            if (allocated(trace_file_path)) deallocate (trace_file_path)
        end if
    end if
    if (has_trace_file_override) then
        if (allocated(trace_file_path)) deallocate (trace_file_path)
        trace_file_path = trim(trace_file_opt)
    end if

    ! Transform lazy fortran to standard fortran
    call trace_enter('cli:transform')
    call cli_trace('CLI: transform begin')
    block
        type(transform_context_t) :: context
        call create_transform_context(from_file, filename, input_text, &
                                      operating_mode, context)
        call enforce_strict_mode_input_rules(operating_mode, input_text, &
                                             context%input_mode)
        call transform_with_context(input_text, output_text, error_msg, context)
    end block
    call trace_leave('cli:transform')
    call cli_trace('CLI: transform end')

    ! Handle errors FIRST: check for errors before writing output
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') trim(error_msg)
            has_fatal_error = index(error_msg, 'ERROR') > 0 .or. &
                              index(error_msg, 'FATAL') > 0 .or. &
                              index(error_msg, '[SYNTAX_ERROR]') > 0 .or. &
                              index(error_msg, '[VALIDATION') > 0 .or. &
                              index(error_msg, '[PARSER_') > 0 .or. &
                              index(error_msg, '[UNRECOGNIZED_INPUT]') > 0 .or. &
                              index(error_msg, '[INVALID_INPUT]') > 0 .or. &
                              index(error_msg, ' semantic error(s):') > 0
            if (has_fatal_error) then
                call exit_quiet(EXIT_FAILURE)
            end if
            ! INFO/WARNING messages are advisory only; continue with success
        end if
    end if

    ! Only write output if no errors occurred
    if (allocated(output_text)) then
        if (len(output_text) > 0) then
            write (output_unit, '(A)', advance='no') output_text
            ! Ensure a trailing newline for line-buffered environments
            ! (e.g., Windows pipes)
            if (output_text(len(output_text):len(output_text)) /= &
                new_line('A')) then
                write (output_unit, '(A)') ''
            end if
            flush (output_unit)
        end if
    end if

    ! If no output was generated and no error was reported, treat as failure
    ! for non-empty inputs. Empty standard Fortran inputs are allowed to
    ! produce no output to preserve strict round-trip semantics.
    if (.not. allocated(output_text)) then
        if (.not. input_is_empty) then
            write (error_unit, '(A)') 'No output generated'
            call exit_quiet(EXIT_FAILURE)
        end if
    else
        if (len(output_text) == 0) then
            if (.not. input_is_empty) then
                write (error_unit, '(A)') 'No output generated'
                call exit_quiet(EXIT_FAILURE)
            end if
        end if
    end if

    call trace_leave('cli:main')
    call trace_finalize()

contains

    function extract_module_name_from_source(source) result(module_name)
        character(len=*), intent(in) :: source
        character(len=:), allocatable :: module_name
        type(token_t), allocatable :: tokens(:)
        integer :: i

        allocate (character(len=0) :: module_name)

        ! Tokenize the source code
        call tokenize_core(source, tokens)

        ! Look for module start and extract name
        do i = 1, size(tokens) - 1
            if (tokens(i)%kind == TK_KEYWORD .and. tokens(i)%text == "module" .and. &
                tokens(i + 1)%kind == TK_IDENTIFIER) then
                module_name = tokens(i + 1)%text
                exit
            end if
        end do
    end function extract_module_name_from_source

    subroutine create_transform_context(from_file, filename, input_text, &
                                        operating_mode, context)
        logical, intent(in) :: from_file
        character(len=:), allocatable, intent(in) :: filename
        character(len=*), intent(in) :: input_text
        integer, intent(in) :: operating_mode
        type(transform_context_t), intent(out) :: context
        character(len=:), allocatable :: actual_module_name

        if (from_file .and. allocated(filename)) then
            call configure_context_from_file(filename, context)
            if (len_trim(input_text) > 0) then
                actual_module_name = extract_module_name_from_source(input_text)
                if (allocated(actual_module_name)) then
                    if (len_trim(actual_module_name) > 0) then
                        context%module_name = trim(actual_module_name)
                    end if
                    deallocate (actual_module_name)
                end if
            end if
        else
            call configure_context_from_stdin(input_text, context)
        end if
        context%operating_mode = operating_mode
    end subroutine create_transform_context

    subroutine enforce_strict_mode_input_rules(operating_mode, source, input_mode)
        integer, intent(in) :: operating_mode
        character(len=*), intent(in) :: source
        integer, intent(in) :: input_mode
        character(len=:), allocatable :: ignored_name
        logical :: is_standard

        if (operating_mode /= OPERATING_MODE_STRICT) return
        if (input_mode /= INPUT_MODE_LAZY) return

        is_standard = is_standard_fortran_input(source, ignored_name)
        if (allocated(ignored_name)) deallocate (ignored_name)
        if (.not. is_standard) then
            write (error_unit, '(A)') &
                'ERROR: Strict mode forbids bare statements without an explicit ' // &
                'program/module/procedure unit.'
            write (error_unit, '(A)') &
                'Suggestion: use --infer for top-level statements.'
            call exit_quiet(EXIT_FAILURE)
        end if
    end subroutine enforce_strict_mode_input_rules

    subroutine configure_context_from_file(filename, context)
        character(len=:), allocatable, intent(in) :: filename
        type(transform_context_t), intent(out) :: context
        character(len=:), allocatable :: basename, extension

        call split_filename(filename, basename, extension)

        ! INPUT MODE DETERMINED BY FILE EXTENSION ONLY
        ! .lf files = INPUT_MODE_LAZY (lazy Fortran with type inference)
        ! All other files (.f90, .f, etc.) = INPUT_MODE_STANDARD (standard Fortran)
        if (extension == '.lf') then
            context%input_mode = INPUT_MODE_LAZY
        else
            context%input_mode = INPUT_MODE_STANDARD
        end if

        context%source_name = basename
        context%module_name = basename

        context%program_name = 'main'
        context%has_filename = .true.
    end subroutine configure_context_from_file

    subroutine configure_context_from_stdin(input_text, context)
        character(len=*), intent(in) :: input_text
        type(transform_context_t), intent(out) :: context
        character(len=64) :: uuid_str
        integer :: clock_count, clock_rate, clock_max
        integer :: values(8)
        character(len=:), allocatable :: prog_name

        ! Detect whether stdin input is standard Fortran or lazy Fortran
        ! Standard Fortran has explicit program/module/subroutine/function structures
        if (is_standard_fortran_input(input_text, prog_name)) then
            context%input_mode = INPUT_MODE_STANDARD
        else
            context%input_mode = INPUT_MODE_LAZY
        end if

        call date_and_time(values=values)
        call system_clock(clock_count, clock_rate, clock_max)
        write (uuid_str, '("stdin_",I0,"_",I0,"_",I0,"_",I0,"_",I0)') &
            values(5), values(6), values(7), values(8), clock_count

        context%source_name = trim(uuid_str)
        if (allocated(prog_name)) then
            context%module_name = prog_name
        else
            context%module_name = trim(uuid_str)
        end if
        context%program_name = 'main'
        context%has_filename = .false.
    end subroutine configure_context_from_stdin

    function is_standard_fortran_input(source, prog_name) result(is_standard)
        character(len=*), intent(in) :: source
        character(len=:), allocatable, intent(out) :: prog_name
        logical :: is_standard
        type(token_t), allocatable :: tokens(:)

        ! Tokenize the source
        call tokenize_core(source, tokens)
        if (.not. allocated(tokens)) then
            is_standard = .false.
            return
        end if
        if (size(tokens) == 0) then
            is_standard = .false.
            return
        end if

        call detect_explicit_program_unit(tokens, is_standard, prog_name)
    end function is_standard_fortran_input

    subroutine split_filename(filename, basename, extension)
        character(len=:), allocatable, intent(in) :: filename
        character(len=:), allocatable, intent(out) :: basename
        character(len=:), allocatable, intent(out) :: extension
        integer :: slash_pos, dot_pos

        basename = trim(filename)
        slash_pos = max(index(basename, '/', back=.true.), &
                        index(basename, '\', back=.true.))
        if (slash_pos > 0) basename = basename(slash_pos + 1:)

        dot_pos = index(basename, '.', back=.true.)
        if (dot_pos > 1) then
            extension = basename(dot_pos:)
            basename = basename(1:dot_pos - 1)
        else
            extension = ''
        end if
    end subroutine split_filename

    subroutine cli_trace(message)
        character(len=*), intent(in) :: message
        integer :: unit_id, ios
        if (.not. trace_enabled) return
        if (.not. allocated(trace_file_path)) return
        open (newunit=unit_id, file=trace_file_path, status='unknown', &
              position='append', &
              action='write', iostat=ios)
        if (ios /= 0) return
        write (unit_id, '(A)') trim(message)
        flush (unit_id)
        close (unit_id)
    end subroutine cli_trace

    subroutine report_deallocation_failure(component, status_code)
        character(len=*), intent(in) :: component
        integer, intent(in) :: status_code

        write (error_unit, '(A)', advance='no') 'Memory deallocation failed for '
        write (error_unit, '(A)', advance='no') trim(component)
        write (error_unit, '(A,I0,A)') ' (stat=', status_code, ')'
        call exit_quiet(EXIT_FAILURE)
    end subroutine report_deallocation_failure

    subroutine read_all_stdin_or_file(from_file, filename, text, status)
        logical, intent(in) :: from_file
        character(len=*), intent(in), optional :: filename
        character(len=:), allocatable, intent(out) :: text
        integer, intent(out) :: status
        integer :: unit_num, ios

        status = 0
        if (from_file) then
            if (.not. present(filename)) then
                write (error_unit, '(A)') &
                    'Filename is required when reading from file'
                status = 1
                return
            end if
            block
                character(len=:), allocatable :: io_error

                call read_file_contents(trim(filename), text, io_error)
                if (message_has_error(io_error)) then
                    write (error_unit, '(A)') trim(io_error)
                    status = 2
                    return
                end if
            end block
            if (status == 0) then
                if (is_fixed_form_file(trim(filename))) then
                    call normalize_fixed_form_source_text(text)
                end if
            end if
        else
            call read_all_from_unit(input_unit, text, status)
        end if
    end subroutine read_all_stdin_or_file

    subroutine read_all_from_unit(unit_num, text, status)
        integer, intent(in) :: unit_num
        character(len=:), allocatable, intent(out) :: text
        integer, intent(out) :: status
        character(len=:), allocatable :: buffer, tmp_text
        integer :: total_size, capacity, ios, chunk_size, alloc_stat
        logical :: exit_inner_loop, reached_end

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
                read (unit_num, '(A)', advance='no', iostat=ios, &
                      size=chunk_size) buffer
                call process_read_result(ios, chunk_size, buffer, text, total_size, &
                                         capacity, status, exit_inner_loop, &
                                         reached_end)
                if (status /= 0) return
                if (exit_inner_loop) exit
            end do
            if (reached_end) exit
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

    subroutine append_when_data(chunk, chunk_size, text, total_size, capacity, status)
        character(len=*), intent(in) :: chunk
        integer, intent(in) :: chunk_size
        character(len=:), allocatable, intent(inout) :: text
        integer, intent(inout) :: total_size, capacity
        integer, intent(inout) :: status

        if (status /= 0) return
        if (chunk_size <= 0) return

        call append_chunk(chunk(1:chunk_size), text, total_size, capacity, status)
    end subroutine append_when_data

    subroutine process_read_result(ios, chunk_size, buffer, text, total_size, &
                                   capacity, status, exit_inner_loop, reached_end)
        integer, intent(in) :: ios
        integer, intent(in) :: chunk_size
        character(len=*), intent(in) :: buffer
        character(len=:), allocatable, intent(inout) :: text
        integer, intent(inout) :: total_size, capacity
        integer, intent(inout) :: status
        logical, intent(out) :: exit_inner_loop
        logical, intent(out) :: reached_end

        exit_inner_loop = .false.
        reached_end = .false.

        select case (ios)
        case (iostat_end)
            exit_inner_loop = .true.
            reached_end = .true.
            call append_when_data(buffer, chunk_size, text, total_size, &
                                  capacity, status)
        case (iostat_eor)
            exit_inner_loop = .true.
            call append_when_data(buffer, chunk_size, text, total_size, &
                                  capacity, status)
            if (status /= 0) return
            call append_newline(text, total_size, capacity, status)
        case (0)
            call append_when_data(buffer, chunk_size, text, total_size, &
                                  capacity, status)
        case default
            write (error_unit, '(A,I0,A)') 'Error reading input (iostat=', ios, ')'
            status = 3
            exit_inner_loop = .true.
            reached_end = .true.
        end select
    end subroutine process_read_result

    subroutine append_chunk(chunk, text, total_size, capacity, status)
        character(len=*), intent(in) :: chunk
        character(len=:), allocatable, intent(inout) :: text
        integer, intent(inout) :: total_size, capacity
        integer, intent(inout) :: status
        character(len=:), allocatable :: tmp
        integer :: needed, alloc_stat

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
                    'Failed to allocate memory for input expansion (', &
                    capacity, ' bytes)'
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
        character(len=:), allocatable, intent(inout) :: text
        integer, intent(inout) :: total_size, capacity
        integer, intent(inout) :: status
        character(len=:), allocatable :: tmp
        integer :: needed, alloc_stat

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
                    'Failed to allocate memory for newline expansion (', &
                    capacity, ' bytes)'
                status = 5
                return
            end if
            if (total_size > 0) tmp(1:total_size) = text(1:total_size)
            call move_alloc(tmp, text)
        end if
        text(total_size + 1:total_size + 1) = new_line('A')
        total_size = total_size + 1
    end subroutine append_newline

end program fortfront_cli
