module frontend_transformation
    use, intrinsic :: iso_fortran_env, only: error_unit
    ! fortfront - Transformation functions module
    ! Contains string-based transformation functionality

    use lexer_core, only: token_t, tokenize_core, TK_EOF, TK_KEYWORD, &
                           TK_COMMENT, TK_NEWLINE, TK_OPERATOR, TK_IDENTIFIER, &
                           TK_NUMBER, TK_STRING, TK_UNKNOWN
    use compiler_arena, only: compiler_arena_t, create_compiler_arena
    use ast_arena_modern, only: ast_arena_t
    use semantic_analyzer, only: semantic_context_t, create_semantic_context, &
                                   analyze_program, has_semantic_errors
    use standardizer, only: standardize_ast, set_standardizer_type_standardization, &
                           get_standardizer_type_standardization
    use codegen_arena_interface, only: generate_code_from_arena
    use codegen_basic_utils, only: add_line_continuations
    use codegen_core, only: initialize_codegen
    use codegen_type_utils, only: set_type_standardization, get_type_standardization
    use codegen_indent, only: set_indent_config, get_indent_config, &
                               set_line_length_config, get_line_length_config
    use input_validation, only: validate_basic_syntax, has_only_meaningless_tokens
    use ast_nodes_core, only: program_node
    use frontend_parsing, only: parse_tokens
    use frontend_core, only: lex_source, emit_fortran
    use debug_trace, only: trace_init, trace_enter, trace_leave

    implicit none
    private

    public :: transform_lazy_fortran_string, &
              transform_lazy_fortran_string_with_format, format_options_t

    ! Performance: reuse a single compiler arena across transformations
    ! to avoid repeated heavy allocations and deallocations.
    type(compiler_arena_t), save :: shared_arena
    logical, save :: shared_arena_initialized = .false.

    ! Profiling support (optional runtime instrumentation)
    ! Formatting options for code generation
    type :: format_options_t
        integer :: indent_size = 4
        logical :: use_tabs = .false.
        character(len=1) :: indent_char = ' '
        logical :: standardize_types = .true.  ! Whether to standardize type kinds
        integer :: line_length = 130  ! Maximum line length before adding continuations
    end type format_options_t

contains
    ! String-based transformation function for CLI usage
    subroutine transform_lazy_fortran_string(input, output, error_msg)
        character(len=*), intent(in) :: input
        character(len=:), allocatable, intent(out) :: output
        character(len=:), allocatable, intent(out) :: error_msg

        ! Local variables for 4-phase pipeline
        type(token_t), allocatable, target :: tokens(:)
        ! Use shared module-level arena for performance
        integer :: prog_index
        logical :: debug_transform
        character(len=8) :: debug_flag
        integer :: env_status

        allocate(character(len=0) :: error_msg)
        error_msg = ""


        call trace_init()

        call trace_enter('transform_lazy_fortran_string')
        ! Initialize the codegen system (idempotent)
        debug_transform = .false.
        call get_environment_variable('FORTFRONT_DEBUG_TRANSFORM', debug_flag, status=env_status)
        if (env_status == 0) debug_transform = .true.
        call initialize_codegen()

        ! Obtain the shared compiler arena and reset for a clean run
        ! PERFORMANCE FIX: Initialize in-place to avoid assignment operator overhead
        if (.not. shared_arena_initialized) then
            call shared_arena%init()
            shared_arena_initialized = .true.
        else
            call shared_arena%reset()
        end if

        ! Handle empty or whitespace-only input
        if (is_empty_or_whitespace_only(input)) then
            call create_minimal_program(output)
            call trace_leave('transform_lazy_fortran_string')
            return
        end if

        if (contains_binary_data(input)) then
            error_msg = '[INVALID_INPUT] Input appears to be binary data' // &
     &                new_line('A') // '  Source: <binary data omitted>' // &
     &                new_line('A') // '  Suggestion: Provide plain-text Fortran source'
            call create_minimal_program(output)
            call trace_leave('transform_lazy_fortran_string')
            return
        end if

        ! Phase 1: Lexical Analysis
        call trace_enter('phase:lexer')
        call run_lexical_analysis(input, tokens, shared_arena, error_msg)
        call trace_leave('phase:lexer')
        if (error_msg /= "") then
            call handle_lexical_error(input, error_msg, output, shared_arena)
            call trace_leave('transform_lazy_fortran_string')
            return
        end if

        ! Phase 1.5: Enhanced syntax validation with comprehensive error reporting (Issue #256)
        call trace_enter('phase:syntax')
        call validate_syntax_with_reporting(input, tokens, error_msg, output, shared_arena)
        call trace_leave('phase:syntax')
        if (error_msg /= "") then
            call trace_leave('transform_lazy_fortran_string')
            return
        end if

        ! Check for meaningful content
        if (not_meaningful_for_parsing(tokens)) then
            call create_minimal_program(output)
            call trace_leave('transform_lazy_fortran_string')
            return
        end if

        ! Phase 2: Parsing
        call trace_enter('phase:parser')
        call run_parsing_phase(tokens, shared_arena, prog_index, error_msg, output)
        if (debug_transform) then
            write(error_unit, '(A,I0)') 'DEBUG transform: prog_index after parsing = ', prog_index
            if (allocated(output)) then
                if (len(output) > 0) then
                    write(error_unit, '(A)') 'DEBUG transform: intermediate output:'
                    write(error_unit, '(A)') output
                end if
            end if
        end if
        call trace_leave('phase:parser')
        if (error_msg /= "") then
            call trace_leave('transform_lazy_fortran_string')
            return
        end if

        ! Phases 3-5: Semantic Analysis, Standardization, Code Generation
        call trace_enter('phase:final')
        call run_final_phases(shared_arena, prog_index, output, error_msg)
        if (debug_transform) then
            write(error_unit, '(A,I0)') 'DEBUG transform: prog_index after final = ', prog_index
            write(error_unit, '(A)') 'DEBUG transform: final output:'
            if (allocated(output)) then
                if (len(output) > 0) then
                    write(error_unit, '(A)') output
                else
                    write(error_unit, '(A)') '<empty output>'
                end if
            else
                write(error_unit, '(A)') '<output not allocated>'
            end if
            write(error_unit, '(A)') 'DEBUG transform: error_msg:'
            if (allocated(error_msg)) then
                write(error_unit, '(A)') trim(error_msg)
            else
                write(error_unit, '(A)') '<error_msg not allocated>'
            end if
        end if
        call trace_leave('phase:final')
        if (error_msg /= "") then
            call trace_leave('transform_lazy_fortran_string')
            return
        end if

        ! Ensure error_msg is empty on successful transformation
        error_msg = ""

        ! Preserve a contiguous leading block of comment lines from the input
        if (has_leading_comment(input)) then
            block
                character(len=:), allocatable :: lead
                lead = extract_leading_comment_block(input)
                if (allocated(lead)) then
                    if (len_trim(lead) > 0) then
                        if (len_trim(output) > 0) then
                            output = trim(lead) // new_line('A') // trim(output)
                        else
                            output = trim(lead)
                        end if
                    end if
                end if
            end block
        end if
        call trace_leave('transform_lazy_fortran_string')

        ! Reuse arena: no destroy, it will be reset at next call
    end subroutine transform_lazy_fortran_string

    ! String-based transformation function with formatting options
    subroutine transform_lazy_fortran_string_with_format(input, output, &
                                                         error_msg, format_opts)
        character(len=*), intent(in) :: input
        character(len=:), allocatable, intent(out) :: output
        character(len=:), allocatable, intent(out) :: error_msg
        type(format_options_t), intent(in) :: format_opts

        ! Save current indentation, line length, and type standardization configuration
        integer :: saved_size, saved_line_length
        character(len=1) :: saved_char
        logical :: saved_standardize_types, saved_standardizer_types
        
        call save_current_configuration(saved_size, saved_char, saved_line_length, &
                                      saved_standardize_types, saved_standardizer_types)

        ! Set new configuration
        call apply_format_options(format_opts)

        ! Call the regular transformation function
        call transform_lazy_fortran_string(input, output, error_msg)

        ! Restore original configuration
        call restore_configuration(saved_size, saved_char, saved_line_length, &
                                  saved_standardize_types, saved_standardizer_types)
    end subroutine transform_lazy_fortran_string_with_format

    ! Check if input is empty or whitespace only
    function is_empty_or_whitespace_only(input) result(is_empty)
        character(len=*), intent(in) :: input
        logical :: is_empty
        
        is_empty = (len_trim(input) == 0 .or. is_whitespace_only(input))
    end function is_empty_or_whitespace_only

    ! Create minimal program
    subroutine create_minimal_program(output)
        character(len=:), allocatable, intent(out) :: output
        
        output = "program main" // new_line('A') // &
                 "    implicit none" // new_line('A') // &
                 "end program main" // new_line('A')
    end subroutine create_minimal_program

    ! Run lexical analysis
    subroutine run_lexical_analysis(input, tokens, compiler_arena, error_msg)
        character(len=*), intent(in) :: input
        type(token_t), allocatable, intent(out) :: tokens(:)
        type(compiler_arena_t), intent(inout) :: compiler_arena
        character(len=:), allocatable, intent(inout) :: error_msg

        call compiler_arena%next_phase("lexer")
        call lex_source(input, tokens, error_msg)
    end subroutine run_lexical_analysis

    ! Handle lexical error
    subroutine handle_lexical_error(input, error_msg, output, compiler_arena)
        character(len=*), intent(in) :: input
        character(len=:), allocatable, intent(inout) :: error_msg
        character(len=:), allocatable, intent(out) :: output
        type(compiler_arena_t), intent(inout) :: compiler_arena

        ! CRITICAL FIX for Issue #1058: Generate valid Fortran output only
        ! Error messages go to error_msg (stderr), valid Fortran goes to output (stdout)
        output = "program main" // new_line('A') // &
                "    implicit none" // new_line('A') // &
                "    ! Original code could not be parsed" // new_line('A') // &
                "end program main" // new_line('A')
        ! error_msg already contains the error details for stderr
        ! Reuse shared arena: do not destroy here
    end subroutine handle_lexical_error

    ! Validate syntax with reporting
    subroutine validate_syntax_with_reporting(input, tokens, error_msg, output, compiler_arena)
        character(len=*), intent(in) :: input
        type(token_t), intent(in) :: tokens(:)
        character(len=:), allocatable, intent(inout) :: error_msg
        character(len=:), allocatable, intent(out) :: output
        type(compiler_arena_t), intent(inout) :: compiler_arena

        call validate_basic_syntax(input, tokens, error_msg)
        if (error_msg /= "") then
            ! CRITICAL FIX for Issue #1058: Generate valid Fortran output only
            ! Error messages go to error_msg (stderr), valid Fortran goes to output (stdout)
            output = "program main" // new_line('A') // &
                    "    implicit none" // new_line('A') // &
                    "    ! COMPILATION FAILED" // new_line('A') // &
                    "    ! Original code could not be parsed" // new_line('A') // &
                    "end program main" // new_line('A')
            ! error_msg already contains the error details for stderr
            ! Reuse shared arena: do not destroy here
        end if
    end subroutine validate_syntax_with_reporting

    ! Check if not meaningful for parsing
    function not_meaningful_for_parsing(tokens) result(not_meaningful)
        type(token_t), intent(in) :: tokens(:)
        logical :: not_meaningful
        integer :: meaningful_tokens, i

        ! Check if validation passed but we have no meaningful content to parse
        ! This handles cases where input is only comments, whitespace, or empty
        meaningful_tokens = 0
        do i = 1, size(tokens)
            if (tokens(i)%kind == TK_EOF .or. tokens(i)%kind == TK_NEWLINE .or. &
                tokens(i)%kind == TK_COMMENT) cycle
            meaningful_tokens = meaningful_tokens + 1
        end do

        not_meaningful = (meaningful_tokens == 0 .or. size(tokens) == 0 .or. &
                         has_only_meaningless_tokens(tokens))
    end function not_meaningful_for_parsing

    ! Run parsing phase
    subroutine run_parsing_phase(tokens, compiler_arena, prog_index, error_msg, output)
        type(token_t), intent(in) :: tokens(:)
        type(compiler_arena_t), intent(inout) :: compiler_arena
        integer, intent(out) :: prog_index
        character(len=:), allocatable, intent(inout) :: error_msg
        character(len=:), allocatable, intent(out) :: output

        ! Phase 2: Parsing with enhanced error recovery
        call compiler_arena%next_phase("parser")
        call parse_tokens(tokens, compiler_arena%ast, prog_index, error_msg)
        
        ! Enhanced error handling - don't stop at first parsing issue
        if (error_msg /= "" .and. index(error_msg, "Cannot open") == 0) then
            ! Try to continue parsing with partial results if we have a valid program
            if (prog_index > 0 .and. prog_index <= compiler_arena%ast%size) then
                ! We have a partial parse - continue with what we have
                ! Log the parsing warning but don't fail completely
                write(error_unit, '(A,A)') "Warning: Parsing issues detected but continuing: ", error_msg
                error_msg = ""  ! Clear error to continue processing
            else
                call handle_parsing_error(compiler_arena, prog_index, error_msg, output)
                return
            end if
        end if

        ! Debug: check if we got a valid program index
        if (prog_index <= 0) then
            call handle_invalid_program_index(error_msg, output, compiler_arena)
        end if
    end subroutine run_parsing_phase

    ! Handle parsing error
    subroutine handle_parsing_error(compiler_arena, prog_index, error_msg, output)
        type(compiler_arena_t), intent(inout) :: compiler_arena
        integer, intent(in) :: prog_index
        character(len=:), allocatable, intent(inout) :: error_msg
        character(len=:), allocatable, intent(out) :: output

        ! CRITICAL FIX for Issue #1058: Generate valid Fortran output only
        ! Error messages go to error_msg (stderr), valid Fortran goes to output (stdout)
        if (prog_index > 0) then
            call emit_fortran(compiler_arena%ast, prog_index, output)
            ! Don't append error messages to output - they go to stderr via error_msg
        else
            call create_parsing_error_program(error_msg, output)
        end if
        ! Reuse shared arena: do not destroy here
    end subroutine handle_parsing_error

    ! Create parsing error program
    subroutine create_parsing_error_program(error_msg, output)
        character(len=*), intent(in) :: error_msg
        character(len=:), allocatable, intent(out) :: output

        ! CRITICAL FIX for Issue #1058: Generate valid Fortran output only
        ! Error messages go to error_msg (stderr), valid Fortran goes to output (stdout)
            output = "program main" // new_line('A') // &
                    "    implicit none" // new_line('A') // &
                    "    ! COMPILATION FAILED" // new_line('A') // &
                    "    ! Original code could not be parsed" // new_line('A') // &
                    "end program main" // new_line('A')
        ! error_msg parameter already contains the error details for stderr
    end subroutine create_parsing_error_program

    ! Handle invalid program index
    subroutine handle_invalid_program_index(error_msg, output, compiler_arena)
        character(len=:), allocatable, intent(inout) :: error_msg
        character(len=:), allocatable, intent(out) :: output
        type(compiler_arena_t), intent(inout) :: compiler_arena

        error_msg = "Parsing succeeded but no valid program unit was created"
        ! CRITICAL FIX for Issue #1058: Generate valid Fortran output only
        ! Error messages go to error_msg (stderr), valid Fortran goes to output (stdout)
        output = "program main" // new_line('A') // &
                "    implicit none" // new_line('A') // &
                "    ! COMPILATION FAILED" // new_line('A') // &
                "    ! Original code could not be structured as a program" // new_line('A') // &
                "end program main" // new_line('A')
        ! error_msg already contains the error details for stderr
        ! Reuse shared arena: do not destroy here
    end subroutine handle_invalid_program_index

    ! Run final phases (semantic, standardization, codegen)
    subroutine run_final_phases(compiler_arena, prog_index, output, error_msg)
        type(compiler_arena_t), intent(inout) :: compiler_arena
        integer, intent(inout) :: prog_index
        character(len=:), allocatable, intent(out) :: output
        character(len=:), allocatable, intent(inout) :: error_msg

        ! Phase 3: Semantic Analysis
        call run_semantic_analysis_phase(compiler_arena, prog_index, error_msg)
        if (allocated(error_msg) .and. len(error_msg) > 0) then
            ! CRITICAL FIX for Issue #1120: Generate output even with semantic errors
            ! Continue to code generation to provide useful output to user
            call run_code_generation_phase(compiler_arena, prog_index, output)
            ! If code generation fails, provide minimal program
            if (.not. allocated(output) .or. len(output) == 0) then
                call create_minimal_program(output)
            end if
            return  ! Error message already set, output generated
        end if


        ! Phase 4: Standardization
        call run_standardization_phase(compiler_arena, prog_index)

        ! Phase 5: Code Generation
        call run_code_generation_phase(compiler_arena, prog_index, output)
    end subroutine run_final_phases

    ! Run semantic analysis phase
    subroutine run_semantic_analysis_phase(compiler_arena, prog_index, error_msg)
        type(compiler_arena_t), intent(inout) :: compiler_arena
        integer, intent(in) :: prog_index
        character(len=:), allocatable, intent(out) :: error_msg

        call compiler_arena%next_phase("semantic")
        block
            type(semantic_context_t), allocatable :: ctx
            allocate(ctx)
            call create_semantic_context(ctx)
            
            ! Keep pre-standardization semantics permissive in string transform path
            ctx%strict_mode = .false.
            ctx%respect_implicit_none = .false.
            
            call trace_enter('semantic:analyze_program')
            call analyze_program(ctx, compiler_arena%ast, prog_index)
            call trace_leave('semantic:analyze_program')
            
            ! Check for semantic errors and provide detailed error messages
            if (has_semantic_errors(ctx)) then
                error_msg = get_detailed_semantic_errors(ctx)
                return
            end if
        end block
        
        error_msg = ""
    end subroutine run_semantic_analysis_phase

    ! Helper function to get detailed semantic error messages
    function get_detailed_semantic_errors(ctx) result(error_msg)
        type(semantic_context_t), intent(in) :: ctx
        character(len=:), allocatable :: error_msg
        integer :: i, total_errors
        character(len=256) :: buffer
        
        total_errors = ctx%errors%count
        if (total_errors == 0) then
            error_msg = "No semantic errors found"
            return
        end if
        
        ! Build comprehensive error message
        write(buffer, '(A,I0,A)') "Found ", total_errors, " semantic error(s):"
        error_msg = trim(buffer)
        
        ! Add first few error messages for details
        do i = 1, min(3, total_errors)  ! Limit to first 3 errors to avoid overflow
            if (i <= size(ctx%errors%errors)) then
                if (allocated(ctx%errors%errors(i)%error_message)) then
                    error_msg = error_msg // new_line('a') // "  - " // ctx%errors%errors(i)%error_message
                    if (allocated(ctx%errors%errors(i)%suggestion)) then
                        error_msg = error_msg // new_line('a') // "    Suggestion: " // ctx%errors%errors(i)%suggestion
                    end if
                end if
            end if
        end do
        
        ! Add summary if there are more errors
        if (total_errors > 3) then
            write(buffer, '(A,I0,A)') "  ... and ", (total_errors - 3), " more error(s)"
            error_msg = error_msg // new_line('a') // trim(buffer)
        end if
    end function get_detailed_semantic_errors

    ! Run standardization phase
    subroutine run_standardization_phase(compiler_arena, prog_index)
        type(compiler_arena_t), intent(inout) :: compiler_arena
        integer, intent(inout) :: prog_index

        call compiler_arena%next_phase("standardization")
        ! Skip standardization for multi-unit containers
        if (should_skip_standardization(compiler_arena, prog_index)) then
            return
        end if

        call standardize_ast(compiler_arena%ast, prog_index)
    end subroutine run_standardization_phase

    ! Check if should skip standardization
    function should_skip_standardization(compiler_arena, prog_index) result(skip_standardization)
        type(compiler_arena_t), intent(in) :: compiler_arena
        integer, intent(in) :: prog_index
        logical :: skip_standardization

        skip_standardization = .false.
        if (prog_index > 0 .and. prog_index <= compiler_arena%ast%size) then
            if (allocated(compiler_arena%ast%entries(prog_index)%node)) then
                select type (node => compiler_arena%ast%entries(prog_index)%node)
                type is (program_node)
                    if (node%name == "__MULTI_UNIT__") then
                        skip_standardization = .true.
                    end if
                end select
            end if
        end if
    end function should_skip_standardization

    ! Run code generation phase
    subroutine run_code_generation_phase(compiler_arena, prog_index, output)
        type(compiler_arena_t), intent(inout) :: compiler_arena
        integer, intent(in) :: prog_index
        character(len=:), allocatable, intent(out) :: output

        call compiler_arena%next_phase("codegen")
        call maybe_dump_program_overview(compiler_arena%ast, prog_index)
        output = generate_code_from_arena(compiler_arena%ast, prog_index)
        output = add_line_continuations(output)
    end subroutine run_code_generation_phase

    subroutine maybe_dump_program_overview(arena, prog_index)
        use, intrinsic :: iso_fortran_env, only: error_unit
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: prog_index
        character(len=8) :: flag
        integer :: status
        integer :: i

        call get_environment_variable('FORTFRONT_DEBUG_DUMP_AST', flag, status=status)
        if (status /= 0) return
        if (len_trim(flag) == 0) return

        write(error_unit, '(A)') 'DEBUG AST: program overview'
        write(error_unit, '(A,I0)') '  root index: ', prog_index
        do i = 1, min(arena%size, size(arena%entries))
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (node => arena%entries(i)%node)
            type is (program_node)
                if (allocated(node%body_indices)) then
                    write(error_unit, '(A,I0,2X,A,2X,I0)') &
                        '  program idx', i, trim(node%name), size(node%body_indices)
                else
                    write(error_unit, '(A,I0,2X,A,2X,A)') &
                        '  program idx', i, trim(node%name), 'no body'
                end if
            end select
        end do
    end subroutine maybe_dump_program_overview

    ! Save current configuration
    subroutine save_current_configuration(saved_size, saved_char, saved_line_length, &
                                        saved_standardize_types, saved_standardizer_types)
        integer, intent(out) :: saved_size, saved_line_length
        character(len=1), intent(out) :: saved_char
        logical, intent(out) :: saved_standardize_types, saved_standardizer_types

        call get_indent_config(saved_size, saved_char)
        call get_line_length_config(saved_line_length)
        call get_type_standardization(saved_standardize_types)
        call get_standardizer_type_standardization(saved_standardizer_types)
    end subroutine save_current_configuration

    ! Apply format options
    subroutine apply_format_options(format_opts)
        type(format_options_t), intent(in) :: format_opts

        call set_indent_config(format_opts%indent_size, format_opts%indent_char)
        call set_line_length_config(format_opts%line_length)
        call set_type_standardization(format_opts%standardize_types)
        call set_standardizer_type_standardization(format_opts%standardize_types)
    end subroutine apply_format_options

    ! Restore configuration
    subroutine restore_configuration(saved_size, saved_char, saved_line_length, &
                                   saved_standardize_types, saved_standardizer_types)
        integer, intent(in) :: saved_size, saved_line_length
        character(len=1), intent(in) :: saved_char
        logical, intent(in) :: saved_standardize_types, saved_standardizer_types

        call set_indent_config(saved_size, saved_char)
        call set_line_length_config(saved_line_length)
        call set_type_standardization(saved_standardize_types)
        call set_standardizer_type_standardization(saved_standardizer_types)
    end subroutine restore_configuration

    ! Detect binary data early; allow UTF-8/high-bit text
    pure logical function contains_binary_data(text) result(has_binary)
        character(len=*), intent(in) :: text
        integer :: i, code, limit

        has_binary = .false.
        limit = min(len(text), 4096)
        if (limit <= 0) return

        do i = 1, limit
            code = iachar(text(i:i))
            if (code == 0) then
                has_binary = .true.
                return
            end if
            ! Reject control characters except TAB(9), LF(10), CR(13)
            if (code < 32 .and. code /= 9 .and. code /= 10 .and. code /= 13) then
                has_binary = .true.
                return
            end if
            ! High-bit (>=128) allowed to support UTF-8 and extended text
        end do
    end function contains_binary_data

    ! Check if input contains only whitespace characters (spaces, tabs, newlines)
    function is_whitespace_only(input) result(is_whitespace)
        character(len=*), intent(in) :: input
        logical :: is_whitespace
        integer :: i

        is_whitespace = .true.
        do i = 1, len(input)
            if (input(i:i) /= ' ' .and. input(i:i) /= char(9) .and. &  ! space and tab
                input(i:i) /= new_line('A')) then                      ! newline
                is_whitespace = .false.
                exit
            end if
        end do
    end function is_whitespace_only

    pure logical function has_leading_comment(src)
        character(len=*), intent(in) :: src
        integer :: n, k
        n = len(src)
        k = 1
        do while (k <= n)
            if (src(k:k) == ' ' .or. iachar(src(k:k)) == 9 .or. &
                src(k:k) == new_line('A')) then
                k = k + 1
            else
                exit
            end if
        end do
        has_leading_comment = (k <= n .and. src(k:k) == '!')
    end function has_leading_comment

    ! Extract a contiguous leading block of comment lines from the raw input.
    ! Leading comment lines start with optional whitespace followed by '!'.
    pure function extract_leading_comment_block(src) result(block_text)
        character(len=*), intent(in) :: src
        character(len=:), allocatable :: block_text
        integer :: n, i, j
        logical :: saw_comment

        block_text = ""
        saw_comment = .false.
        n = len(src)
        i = 1

        ! Quick check: if the first non-whitespace character is not '!',
        ! there is no leading comment block to preserve.
        do while (i <= n)
            if (src(i:i) == ' ' .or. iachar(src(i:i)) == 9 .or. &
                src(i:i) == new_line('A')) then
                i = i + 1
            else
                exit
            end if
        end do
        if (i > n) then
            deallocate(block_text)
            return
        end if
        if (src(i:i) /= '!') then
            deallocate(block_text)
            return
        end if

        ! Reset to start scanning from the beginning to capture all leading comments
        i = 1

        do while (i <= n)
            ! Find start of line (skip nothing; i already at start)
            j = i
            ! Skip leading spaces/tabs
            do while (j <= n)
                if (src(j:j) == ' ' .or. iachar(src(j:j)) == 9) then
                    j = j + 1
                else
                    exit
                end if
            end do

            if (j > n) exit

            select case (src(j:j))
            case ('!')
                ! Comment line: collect until newline
                saw_comment = .true.
                if (len(block_text) > 0) block_text = block_text // new_line('A')
                do while (i <= n .and. src(i:i) /= new_line('A'))
                    block_text = block_text // src(i:i)
                    i = i + 1
                end do
                ! Trim trailing spaces from collected line
                block_text = trim(block_text)
                ! Consume newline if present
                if (i <= n .and. src(i:i) == new_line('A')) i = i + 1
            case (char(10))  ! newline encountered at line start
                if (saw_comment) then
                    if (len(block_text) > 0) block_text = block_text // new_line('A')
                end if
                i = i + 1
            case default
                exit  ! Non-comment, non-blank line: stop
            end select
        end do

        if (len(block_text) == 0) then
            deallocate(block_text)
        end if
    end function extract_leading_comment_block

end module frontend_transformation
