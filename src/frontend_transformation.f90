module frontend_transformation
    use string_utils_mod, only: to_lower
    use, intrinsic :: iso_fortran_env, only: error_unit
    ! fortfront - Transformation functions module
    ! Contains string-based transformation functionality

    use lexer_core, only: token_t, tokenize_core, normalize_line_endings, &
                          TK_EOF, TK_KEYWORD, TK_COMMENT, TK_NEWLINE, &
                          TK_OPERATOR, TK_IDENTIFIER, TK_NUMBER, TK_STRING, &
                          TK_UNKNOWN
    use compiler_arena, only: compiler_arena_t, create_compiler_arena
    use ast_arena_modern, only: ast_arena_t
    use semantic_analyzer, only: semantic_context_t, create_semantic_context, &
                                 analyze_program, has_semantic_errors
    use type_system_unified, only: reset_type_system
    use standardizer, only: standardize_ast, set_standardizer_type_standardization, &
                            get_standardizer_type_standardization, &
                            mark_pointer_targets
    use codegen_arena_interface, only: generate_code_from_arena
    use ast_monomorphization, only: transform_monomorphization
    use call_graph_signatures_mod, only: signatures_map_t, type_signature_t, &
                                         add_signature, create_signatures_map
    use codegen_basic_utils, only: add_line_continuations
    use codegen_core, only: initialize_codegen
    use codegen_type_utils, only: set_type_standardization, get_type_standardization
    use codegen_indent, only: set_indent_config, get_indent_config, &
                              set_line_length_config, get_line_length_config
    use input_validation, only: validate_basic_syntax, has_only_meaningless_tokens
    use ast_nodes_core, only: program_node, assignment_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use ast_nodes_misc, only: contains_node, use_statement_node
    use ast_nodes_data, only: declaration_node, module_node, &
                              mixed_construct_container_node
    use frontend_parsing, only: parse_tokens
    use frontend_core, only: lex_source, emit_fortran
    use debug_trace, only: trace_init, trace_enter, trace_leave, trace_is_enabled
    use procedure_classification, only: should_hoist_procedure
    use semantic_input_mode, only: INPUT_MODE_LAZY, INPUT_MODE_STANDARD

    implicit none
    private

    public :: transform_lazy_fortran_string, &
              transform_lazy_fortran_string_with_format, &
              transform_with_context, &
              format_options_t, transform_context_t, &
              INPUT_MODE_LAZY, INPUT_MODE_STANDARD, &
              detect_input_mode_from_content

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

    ! Context for transformation (source name, wrapping strategy)
    type :: transform_context_t
        character(len=:), allocatable :: source_name
        ! filename without extension or "stdin"
        character(len=:), allocatable :: module_name  ! for wrapping functions
        character(len=:), allocatable :: program_name  ! for wrapping main code
        logical :: has_filename = .false.  ! true if from file, false if stdin
        integer :: input_mode = INPUT_MODE_LAZY
        ! INPUT_MODE_LAZY or INPUT_MODE_STANDARD
    end type transform_context_t

contains
    ! String-based transformation function for CLI usage
    subroutine transform_lazy_fortran_string(input, output, error_msg, &
                                             enable_ast_wrapping)
        character(len=*), intent(in) :: input
        character(len=:), allocatable, intent(out) :: output
        character(len=:), allocatable, intent(out) :: error_msg
        logical, intent(in), optional :: enable_ast_wrapping

        ! Local variables for 4-phase pipeline
        type(token_t), allocatable, target :: tokens(:)
        ! Use shared module-level arena for performance
        integer :: prog_index
        character(len=:), allocatable :: source
        logical :: apply_ast_wrapping

        allocate (character(len=0) :: error_msg)
        error_msg = ""

        apply_ast_wrapping = .false.
        if (present(enable_ast_wrapping)) then
            apply_ast_wrapping = enable_ast_wrapping
        end if

        call trace_init()

        source = normalize_line_endings(input)

        call trace_enter('transform_lazy_fortran_string')
        ! Initialize the codegen system (idempotent)
        call initialize_codegen()

        ! Reset type system arena to prevent type accumulation across transformations
        ! CRITICAL: This prevents circular type references and slowdowns when running
        ! multiple transformations in sequence (e.g., during test suite execution)
        ! IMPORTANT: Must be called BEFORE resetting the AST arena to avoid dangling
        ! pointers in mono_type_t instances stored in AST nodes
        call reset_type_system()

        ! Obtain the shared compiler arena and reset for a clean run
        ! PERFORMANCE FIX: Initialize in-place to avoid assignment operator overhead
        if (.not. shared_arena_initialized) then
            call shared_arena%init()
            shared_arena_initialized = .true.
        else
            call shared_arena%reset()
        end if

        ! Handle empty or whitespace-only input
        if (is_empty_or_whitespace_only(source)) then
            call create_minimal_program(output)
            call trace_leave('transform_lazy_fortran_string')
            return
        end if

        if (contains_binary_data(source)) then
            error_msg = '[INVALID_INPUT] Input appears to be binary data' // &
     &                new_line('A') // '  Source: <binary data omitted>' // &
     &                new_line('A') // '  Suggestion: Provide plain-text Fortran source'
            call create_minimal_program(output)
            call trace_leave('transform_lazy_fortran_string')
            return
        end if

        ! Phase 1: Lexical Analysis
        call trace_enter('phase:lexer')
        call run_lexical_analysis(source, tokens, shared_arena, error_msg)
        call trace_leave('phase:lexer')
        if (error_msg /= "") then
            call handle_lexical_error(source, error_msg, output, shared_arena)
            call trace_leave('transform_lazy_fortran_string')
            return
        end if

        ! Detect standard Fortran inputs we should pass through unchanged
        if (is_probably_standard_fortran(tokens)) then
            output = ensure_trailing_newline(source)
            call trace_leave('transform_lazy_fortran_string')
            return
        end if

 ! Phase 1.5: Enhanced syntax validation with comprehensive error reporting (Issue #256)
        call trace_enter('phase:syntax')
        call validate_syntax_with_reporting(source, tokens, error_msg, output, &
            & shared_arena)
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
        call trace_leave('phase:parser')
        if (error_msg /= "") then
            call trace_leave('transform_lazy_fortran_string')
            return
        end if

        ! Phases 3-5: Semantic Analysis, Standardization, Code Generation
        call trace_enter('phase:final')
        call run_final_phases(shared_arena, prog_index, output, error_msg, &
                              apply_ast_wrapping)
        call trace_leave('phase:final')
        if (error_msg /= "") then
            call trace_leave('transform_lazy_fortran_string')
            return
        end if

        ! Ensure error_msg is empty on successful transformation
        error_msg = ""

        ! Preserve a contiguous leading block of comment lines from the input
        if (has_leading_comment(source)) then
            block
                character(len=:), allocatable :: lead
                lead = extract_leading_comment_block(source)
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
                                        saved_standardize_types, &
                                        saved_standardizer_types)

        ! Set new configuration
        call apply_format_options(format_opts)

        ! Call the regular transformation function
        call transform_lazy_fortran_string(input, output, error_msg)

        ! Restore original configuration
        call restore_configuration(saved_size, saved_char, saved_line_length, &
                                   saved_standardize_types, saved_standardizer_types)
    end subroutine transform_lazy_fortran_string_with_format

    ! Context-aware transformation (wraps functions in modules, respects source names)
    ! AST-based transformation with context-aware module wrapping
    subroutine transform_with_context(input, output, error_msg, context)
        character(len=*), intent(in) :: input
        character(len=:), allocatable, intent(out) :: output
        character(len=:), allocatable, intent(out) :: error_msg
        type(transform_context_t), intent(in) :: context

        ! For standard Fortran mode, use simple transformation
        if (context%input_mode == INPUT_MODE_STANDARD) then
            call transform_lazy_fortran_string(input, output, error_msg, &
                                               enable_ast_wrapping=.false.)
            return
        end if

        ! For lazy Fortran mode, use AST-based wrapping
        call transform_lazy_with_ast_wrapping(input, output, error_msg, context)
    end subroutine transform_with_context

    ! Transform lazy Fortran with AST-based module wrapping
    subroutine transform_lazy_with_ast_wrapping(input, output, error_msg, context)
        character(len=*), intent(in) :: input
        character(len=:), allocatable, intent(out) :: output
        character(len=:), allocatable, intent(out) :: error_msg
        type(transform_context_t), intent(in) :: context
        type(token_t), allocatable, target :: tokens(:)
        integer :: prog_index
        logical :: has_functions, has_subroutines, has_main_code
        type(signatures_map_t) :: signatures
        character(len=:), allocatable :: source

        allocate (character(len=0) :: error_msg)
        error_msg = ""

        call trace_init()
        call trace_enter('transform_lazy_with_ast_wrapping')
        call initialize_codegen()

        source = normalize_line_endings(input)

        ! Reset type system arena to prevent type accumulation across transformations
        ! IMPORTANT: Must be called BEFORE resetting the AST arena to avoid dangling
        ! pointers in mono_type_t instances stored in AST nodes
        call reset_type_system()

        ! Initialize or reset shared arena
        if (.not. shared_arena_initialized) then
            call shared_arena%init()
            shared_arena_initialized = .true.
        else
            call shared_arena%reset()
        end if

        ! Handle empty input
        if (is_empty_or_whitespace_only(source)) then
            call create_minimal_program(output)
            call trace_leave('transform_lazy_with_ast_wrapping')
            return
        end if

        ! Run transformation pipeline up to AST construction
        call run_lexical_analysis(source, tokens, shared_arena, error_msg)
        if (error_msg /= "") then
            call handle_lexical_error(source, error_msg, output, shared_arena)
            call trace_leave('transform_lazy_with_ast_wrapping')
            return
        end if

        if (is_probably_standard_fortran(tokens)) then
            output = ensure_trailing_newline(source)
            call trace_leave('transform_lazy_with_ast_wrapping')
            return
        end if

        call validate_syntax_with_reporting(source, tokens, error_msg, output, &
                                            shared_arena)
        if (error_msg /= "") then
            call trace_leave('transform_lazy_with_ast_wrapping')
            return
        end if

        if (not_meaningful_for_parsing(tokens)) then
            call create_minimal_program(output)
            call trace_leave('transform_lazy_with_ast_wrapping')
            return
        end if

        call run_parsing_phase(tokens, shared_arena, prog_index, error_msg, output)
        if (error_msg /= "") then
            call trace_leave('transform_lazy_with_ast_wrapping')
            return
        end if

        ! Run semantic analysis and standardization (but not code generation yet)
        call run_semantic_analysis_phase(shared_arena, prog_index, error_msg, &
                                         signatures)
        if (allocated(error_msg) .and. len(error_msg) > 0) then
            call run_code_generation_phase(shared_arena, prog_index, output)
            call trace_leave('transform_lazy_with_ast_wrapping')
            return
        end if

        ! Run monomorphization (AST transformation)
        call run_monomorphization_phase(shared_arena, prog_index, signatures)

        call run_standardization_phase(shared_arena, prog_index, .true.)

        ! AST-BASED WRAPPING: Analyze and modify AST directly
        call analyze_ast_content(shared_arena%ast, prog_index, has_functions, &
                                 has_subroutines, has_main_code)

        ! Check if there's already a module in the AST - if so, no wrapping needed
        if (has_existing_module_in_ast(shared_arena%ast)) then
            ! Don't wrap - preserve existing module structure
        else if ((has_functions .or. has_subroutines) .and. has_main_code) then
            call promote_functions_to_internal_program(shared_arena%ast, prog_index)
        else if ((has_functions .or. has_subroutines) .and. .not. has_main_code) then
            call wrap_ast_in_module_only(shared_arena%ast, prog_index, context)
        end if
        ! If only main code or nothing to wrap, leave AST as-is

        ! Generate code from (possibly wrapped) AST
        call run_code_generation_phase(shared_arena, prog_index, output)

        ! Preserve leading comments
        if (has_leading_comment(source)) then
            block
                character(len=:), allocatable :: lead
                lead = extract_leading_comment_block(source)
                if (allocated(lead) .and. len_trim(lead) > 0) then
                    output = trim(lead) // new_line('A') // trim(output)
                end if
            end block
        end if

        call trace_leave('transform_lazy_with_ast_wrapping')
    end subroutine transform_lazy_with_ast_wrapping

    pure function ensure_trailing_newline(text) result(with_newline)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: with_newline
        integer :: text_len

        text_len = len(text)
        if (text_len == 0) then
            with_newline = new_line('A')
        else if (text(text_len:text_len) == new_line('A')) then
            with_newline = text
        else
            with_newline = text // new_line('A')
        end if
    end function ensure_trailing_newline

    pure logical function is_probably_standard_fortran(tokens) result(is_standard)
        type(token_t), intent(in) :: tokens(:)
        integer :: i
        logical :: has_implicit_none
        logical :: references_tooling_api
        character(len=:), allocatable :: lower_text
        character(len=:), allocatable :: next_text

        has_implicit_none = .false.
        references_tooling_api = .false.

        do i = 1, size(tokens)
            if (.not. allocated(tokens(i)%text)) cycle
            lower_text = to_lower(tokens(i)%text)
            if (tokens(i)%kind == TK_KEYWORD) then
                if (lower_text == 'implicit') then
                    if (i < size(tokens)) then
                        if (tokens(i + 1)%kind == TK_KEYWORD) then
                            if (allocated(tokens(i + 1)%text)) then
                                next_text = to_lower(tokens(i + 1)%text)
                                if (next_text == 'none') then
                                    has_implicit_none = .true.
                                end if
                            end if
                        end if
                    end if
                end if
            else if (tokens(i)%kind == TK_IDENTIFIER) then
                select case (lower_text)
                case ('tooling_load_ast_from_string', 'tooling_parse_options_t', &
                      'transform_lazy_fortran_string', &
                      'transform_lazy_fortran_string_with_format', &
                      'frontend_transformation')
                    references_tooling_api = .true.
                end select
            end if
        end do

        is_standard = has_implicit_none .and. references_tooling_api
    end function is_probably_standard_fortran

    pure function detect_input_mode_from_content(input) result(mode)
        character(len=*), intent(in) :: input
        integer :: mode
        logical :: has_implicit_none, has_program, has_module, has_subroutine
        logical :: has_function_keyword, has_end_function
        integer :: i, line_end
        character(len=:), allocatable :: line, trimmed, lowered

        has_implicit_none = .false.
        has_program = .false.
        has_module = .false.
        has_subroutine = .false.
        has_function_keyword = .false.
        has_end_function = .false.

        i = 1
        do while (i <= len(input))
            ! Extract line inline (can't use extract_line since it's not pure)
            line_end = index(input(i:), new_line('A'))
            if (line_end == 0) then
                line = input(i:)
                i = len(input) + 1
            else
                line = input(i:i + line_end - 2)
                i = i + line_end
            end if

            trimmed = trim(adjustl(line))
            if (len(trimmed) == 0) cycle
            if (trimmed(1:1) == '!') cycle

            lowered = to_lower(trimmed)

            if (index(lowered, 'implicit none') == 1 .or. &
                index(lowered, 'implicit  none') == 1) then
                has_implicit_none = .true.
            end if

            if (index(lowered, 'program ') == 1) then
                has_program = .true.
            end if

            if (index(lowered, 'module ') == 1 .and. &
                index(lowered, 'module procedure') == 0) then
                has_module = .true.
            end if

            if (index(lowered, 'subroutine ') == 1) then
                has_subroutine = .true.
            end if

            if (index(lowered, 'function ') == 1) then
                has_function_keyword = .true.
            end if

            if (index(lowered, 'end function') > 0) then
                has_end_function = .true.
            end if
        end do

        if (has_implicit_none .or. has_program .or. has_module) then
            mode = INPUT_MODE_STANDARD
        else if (has_subroutine .or. (has_function_keyword .and. &
                                      has_end_function)) then
            mode = INPUT_MODE_STANDARD
        else
            mode = INPUT_MODE_LAZY
        end if
    end function detect_input_mode_from_content

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
    subroutine validate_syntax_with_reporting(input, tokens, error_msg, output, &
        & compiler_arena)
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

        ! Local buffer for parse_tokens (character(len=*) requires fixed-length)
        character(len=500) :: parse_error_buffer

        ! Phase 2: Parsing with enhanced error recovery
        call compiler_arena%next_phase("parser")
        call parse_tokens(tokens, compiler_arena%ast, prog_index, parse_error_buffer)

        ! Copy buffer to allocatable error_msg
        if (len_trim(parse_error_buffer) > 0) then
            error_msg = trim(parse_error_buffer)
        end if

        ! Enhanced error handling - propagate errors properly
        if (error_msg /= "" .and. index(error_msg, "Cannot open") == 0) then
            ! Don't clear error_msg - we need to propagate it to the caller
            ! Even if prog_index > 0 (partial parse), the error must be reported
            call handle_parsing_error(compiler_arena, prog_index, &
                                      error_msg, output)
            return
        end if

        ! Check if we got a valid program index
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
                 "    ! Original code could not be structured as a program" // &
                 new_line('A') // &
                 "end program main" // new_line('A')
        ! error_msg already contains the error details for stderr
        ! Reuse shared arena: do not destroy here
    end subroutine handle_invalid_program_index

    ! Run final phases (semantic, standardization, codegen)
    subroutine run_final_phases(compiler_arena, prog_index, output, error_msg, &
                                enable_ast_wrapping)
        type(compiler_arena_t), intent(inout) :: compiler_arena
        integer, intent(inout) :: prog_index
        character(len=:), allocatable, intent(out) :: output
        character(len=:), allocatable, intent(inout) :: error_msg
        type(signatures_map_t) :: signatures
        logical, intent(in) :: enable_ast_wrapping
        logical :: has_functions, has_subroutines, has_main_code
        logical :: force_internal_wrapping
        type(transform_context_t) :: context

        ! Phase 3: Semantic Analysis
        call run_semantic_analysis_phase(compiler_arena, prog_index, error_msg, &
                                         signatures)
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

        ! Phase 3.5: Standardization (normalize structure before specialization)
        call run_standardization_phase(compiler_arena, prog_index, .true.)

        ! Phase 4: Monomorphization (AST transformation)
        call run_monomorphization_phase(compiler_arena, prog_index, signatures)

        call analyze_ast_content(compiler_arena%ast, prog_index, has_functions, &
                                 has_subroutines, has_main_code)

        force_internal_wrapping = requires_lazy_internalization( &
                                compiler_arena%ast, prog_index)

        ! Initialize default context for wrapping
        context%source_name = "main"
        context%module_name = "main_module"
        context%program_name = "main"
        context%has_filename = .false.
        context%input_mode = INPUT_MODE_LAZY

        if (.not. has_existing_module_in_ast(compiler_arena%ast)) then
            if ((has_functions .or. has_subroutines) .and. has_main_code) then
                if (enable_ast_wrapping .or. force_internal_wrapping) then
                    call promote_functions_to_internal_program(compiler_arena%ast, &
                                                               prog_index)
                end if
            else if (enable_ast_wrapping .and. (has_functions .or. &
                     has_subroutines) .and. .not. has_main_code) then
                call wrap_ast_in_module_only(compiler_arena%ast, prog_index, context)
            end if
        end if

        ! Phase 5: Code Generation
        call run_code_generation_phase(compiler_arena, prog_index, output)
    end subroutine run_final_phases

    ! Run semantic analysis phase
    subroutine run_semantic_analysis_phase(compiler_arena, prog_index, error_msg, &
                                           signatures)
        type(compiler_arena_t), intent(inout) :: compiler_arena
        integer, intent(in) :: prog_index
        character(len=:), allocatable, intent(out) :: error_msg
        type(signatures_map_t), intent(out) :: signatures

        call compiler_arena%next_phase("semantic")
        block
            type(semantic_context_t) :: ctx
            logical :: handled

            handled = .false.
            call create_semantic_context(ctx)

            ! Start in LAZY mode, but allow automatic detection of implicit none
            ! which will switch to STANDARD mode for better performance
            ctx%input_mode = INPUT_MODE_LAZY

            if (prog_index > 0 .and. prog_index <= compiler_arena%ast%size) then
                if (allocated(compiler_arena%ast%entries(prog_index)%node)) then
                    select type (root_node => &
                                 compiler_arena%ast%entries(prog_index)%node)
                    type is (mixed_construct_container_node)
                        call analyze_container_semantics(compiler_arena%ast, &
                                                         root_node, signatures, &
                                                         error_msg)
                        if (len(error_msg) > 0) return
                        handled = .true.
                    class default
                        call trace_enter('semantic:analyze_program')
                        call analyze_program(ctx, compiler_arena%ast, prog_index)
                        call trace_leave('semantic:analyze_program')
                        signatures = ctx%signatures
                        if (has_semantic_errors(ctx)) then
                            error_msg = get_detailed_semantic_errors(ctx)
                            return
                        end if
                        handled = .true.
                    end select
                end if
            end if

            if (.not. handled) then
                signatures = create_signatures_map()
            end if
        end block

        error_msg = ""
    end subroutine run_semantic_analysis_phase

    subroutine analyze_container_semantics(arena, container, signatures, error_msg)
        type(ast_arena_t), intent(inout) :: arena
        type(mixed_construct_container_node), intent(in) :: container
        type(signatures_map_t), intent(out) :: signatures
        character(len=:), allocatable, intent(inout) :: error_msg
        type(signatures_map_t) :: combined
        type(semantic_context_t) :: local_ctx
        integer :: i, child_idx
        character(len=:), allocatable :: local_error
        logical :: have_error

        combined = create_signatures_map()
        error_msg = ""
        have_error = .false.

        if (allocated(container%implicit_declaration_indices)) then
            do i = 1, size(container%implicit_declaration_indices)
                call analyze_program_by_index(container%implicit_declaration_indices(i))
            end do
        end if
        if (allocated(container%explicit_program_indices)) then
            do i = 1, size(container%explicit_program_indices)
                call analyze_program_by_index(container%explicit_program_indices(i))
            end do
        end if

        signatures = combined
        if (.not. have_error) error_msg = ""
    contains
        subroutine analyze_program_by_index(node_idx)
            integer, intent(in) :: node_idx

            if (node_idx < 1 .or. node_idx > arena%size) return
            if (.not. allocated(arena%entries(node_idx)%node)) return

            write (error_unit, '(A,1X,I0)') 'DEBUG semantic container child', &
                node_idx
            if (allocated(arena%entries(node_idx)%node_type)) then
                write (error_unit, '(A,1X,A)') 'DEBUG node_type', &
                    trim(arena%entries(node_idx)%node_type)
            end if

            call create_semantic_context(local_ctx)
            local_ctx%input_mode = INPUT_MODE_LAZY

            call trace_enter('semantic:analyze_program')
            call analyze_program(local_ctx, arena, node_idx)
            call trace_leave('semantic:analyze_program')

            write (error_unit, '(A,1X,I0)') 'DEBUG local signatures count', &
                local_ctx%signatures%proc_count

            call merge_signature_maps(combined, local_ctx%signatures)

            if (has_semantic_errors(local_ctx)) then
                local_error = get_detailed_semantic_errors(local_ctx)
                if (len_trim(local_error) > 0) then
                    if (have_error) then
                        error_msg = error_msg // new_line('A') // trim(local_error)
                    else
                        error_msg = local_error
                    end if
                    have_error = .true.
                end if
            end if
        end subroutine analyze_program_by_index
    end subroutine analyze_container_semantics

    subroutine merge_signature_maps(target, source)
        type(signatures_map_t), intent(inout) :: target
        type(signatures_map_t), intent(in) :: source
        integer :: i, j

        if (source%proc_count <= 0) return

        do i = 1, source%proc_count
            if (.not. allocated(source%proc_sigs(i)%procedure_name)) cycle
            if (source%proc_sigs(i)%sig_count <= 0) cycle
            do j = 1, source%proc_sigs(i)%sig_count
                call add_signature_from_entry(target, &
                                             trim(source%proc_sigs(i)%procedure_name), &
                                              source%proc_sigs(i)%signatures(j))
            end do
        end do
    end subroutine merge_signature_maps

    subroutine add_signature_from_entry(target, name, sig)
        type(signatures_map_t), intent(inout) :: target
        character(len=*), intent(in) :: name
        type(type_signature_t), intent(in) :: sig
        integer, allocatable :: kinds(:)
        logical :: has_param_types
        logical :: has_return_type

        if (allocated(sig%param_kinds)) then
            allocate (kinds(size(sig%param_kinds)))
            if (size(sig%param_kinds) > 0) kinds = sig%param_kinds
        else
            allocate (kinds(0))
        end if

        has_param_types = allocated(sig%param_type_strings)
        has_return_type = allocated(sig%return_type_string)

        if (has_param_types) then
            if (has_return_type) then
                call add_signature(target, name, kinds, sig%return_kind, &
                                   sig%call_site_node, sig%line, sig%column, &
                                   sig%param_type_strings, sig%return_type_string)
            else
                call add_signature(target, name, kinds, sig%return_kind, &
                                   sig%call_site_node, sig%line, sig%column, &
                                   param_type_strings=sig%param_type_strings)
            end if
        else
            if (has_return_type) then
                call add_signature(target, name, kinds, sig%return_kind, &
                                   sig%call_site_node, sig%line, sig%column, &
                                   return_type_string=sig%return_type_string)
            else
                call add_signature(target, name, kinds, sig%return_kind, &
                                   sig%call_site_node, sig%line, sig%column)
            end if
        end if
    end subroutine add_signature_from_entry

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
        write (buffer, '(A,I0,A)') "Found ", total_errors, " semantic error(s):"
        error_msg = trim(buffer)

        ! Add first few error messages for details
        do i = 1, min(3, total_errors)  ! Limit to first 3 errors to avoid overflow
            if (i <= size(ctx%errors%errors)) then
                if (allocated(ctx%errors%errors(i)%error_message)) then
                    error_msg = error_msg // new_line('a') // "  - " // &
                        & ctx%errors%errors(i)%error_message
                    if (allocated(ctx%errors%errors(i)%suggestion)) then
                        error_msg = error_msg // new_line('a') // &
                            "    Suggestion: " // &
                            & ctx%errors%errors(i)%suggestion
                    end if
                end if
            end if
        end do

        ! Add summary if there are more errors
        if (total_errors > 3) then
            write (buffer, '(A,I0,A)') "  ... and ", (total_errors - 3), &
                " more error(s)"
            error_msg = error_msg // new_line('a') // trim(buffer)
        end if
    end function get_detailed_semantic_errors

    ! Run standardization phase
    subroutine run_standardization_phase(compiler_arena, prog_index, &
                                         enable_multi_unit_normalization)
        type(compiler_arena_t), intent(inout) :: compiler_arena
        integer, intent(inout) :: prog_index
        logical, intent(in) :: enable_multi_unit_normalization

        call compiler_arena%next_phase("standardization")
        if (enable_multi_unit_normalization) then
            call normalize_multi_unit_container(compiler_arena%ast, prog_index)
        end if
        ! Skip standardization for multi-unit containers
        if (should_skip_standardization(compiler_arena, prog_index)) then
            call mark_pointer_targets(compiler_arena%ast)
            return
        end if

        call standardize_ast(compiler_arena%ast, prog_index)
        call mark_pointer_targets(compiler_arena%ast)
    end subroutine run_standardization_phase

    ! Check if should skip standardization
    function should_skip_standardization(compiler_arena, prog_index) &
        & result(skip_standardization)
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

    subroutine collect_procedures_and_target(arena, root_prog, all_procedures, &
                                             target_prog_idx)
        type(ast_arena_t), intent(in) :: arena
        class(program_node), intent(in) :: root_prog
        integer, allocatable, intent(out) :: all_procedures(:)
        integer, intent(out) :: target_prog_idx
        integer :: i
        integer :: first_main_idx

        allocate (all_procedures(0))
        target_prog_idx = 0
        first_main_idx = 0

        if (.not. allocated(root_prog%body_indices)) return

        do i = 1, size(root_prog%body_indices)
            if (root_prog%body_indices(i) <= 0 .or. &
                root_prog%body_indices(i) > arena%size) cycle
            if (.not. allocated(arena%entries(root_prog%body_indices(i))%node)) cycle

            select type (child => arena%entries(root_prog%body_indices(i))%node)
            type is (program_node)
                if (trim(child%name) /= "__MULTI_UNIT__") then
                    if (trim(child%name) /= "" .and. child%name /= "main" .and. &
                        child%name /= "MAIN") then
                        target_prog_idx = root_prog%body_indices(i)
                    else if (first_main_idx == 0) then
                        first_main_idx = root_prog%body_indices(i)
                    end if
                end if
            type is (function_def_node)
                all_procedures = [all_procedures, root_prog%body_indices(i)]
            type is (subroutine_def_node)
                all_procedures = [all_procedures, root_prog%body_indices(i)]
            end select
        end do

        if (target_prog_idx == 0) target_prog_idx = first_main_idx
    end subroutine collect_procedures_and_target

    subroutine filter_hoistable_procedures(arena, all_procedures, target_prog_idx, &
                                           procedures)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: all_procedures(:)
        integer, intent(in) :: target_prog_idx
        integer, allocatable, intent(out) :: procedures(:)
        integer :: i

        allocate (procedures(0))
        do i = 1, size(all_procedures)
            if (should_hoist_procedure(arena, all_procedures(i), target_prog_idx)) then
                procedures = [procedures, all_procedures(i)]
            end if
        end do
    end subroutine filter_hoistable_procedures

    subroutine remove_procedures_from_body(root_prog, procedures)
        class(program_node), intent(inout) :: root_prog
        integer, intent(in) :: procedures(:)
        integer, allocatable :: new_body(:)
        integer :: i

        allocate (new_body(0))
        if (allocated(root_prog%body_indices)) then
            do i = 1, size(root_prog%body_indices)
                if (any(root_prog%body_indices(i) == procedures)) cycle
                new_body = [new_body, root_prog%body_indices(i)]
            end do
        end if
        root_prog%body_indices = new_body
    end subroutine remove_procedures_from_body

    function find_contains_position(arena, target) result(contains_pos)
        type(ast_arena_t), intent(in) :: arena
        class(program_node), intent(in) :: target
        integer :: contains_pos
        integer :: i

        contains_pos = 0
        if (.not. allocated(target%body_indices)) return

        do i = 1, size(target%body_indices)
            if (target%body_indices(i) <= 0 .or. &
                target%body_indices(i) > arena%size) cycle
            if (.not. allocated(arena%entries(target%body_indices(i))%node)) cycle

            select type (stmt => arena%entries(target%body_indices(i))%node)
            type is (contains_node)
                contains_pos = i
                exit
            end select
        end do
    end function find_contains_position

    subroutine ensure_contains_exists(arena, target, target_prog_idx, contains_pos)
        type(ast_arena_t), intent(inout) :: arena
        class(program_node), intent(inout) :: target
        integer, intent(in) :: target_prog_idx
        integer, intent(inout) :: contains_pos
        type(contains_node) :: contains_stmt
        integer :: contains_idx

        if (contains_pos > 0) return

        contains_stmt%line = 1
        contains_stmt%column = 1
        call arena%push(contains_stmt, "contains", target_prog_idx)
        contains_idx = arena%size

        if (.not. allocated(target%body_indices)) then
            allocate (target%body_indices(1))
            target%body_indices(1) = contains_idx
        else
            target%body_indices = [target%body_indices, contains_idx]
        end if
        contains_pos = size(target%body_indices)
    end subroutine ensure_contains_exists

    subroutine insert_procedures_after_contains(target, procedures, contains_pos)
        class(program_node), intent(inout) :: target
        integer, intent(in) :: procedures(:)
        integer, intent(in) :: contains_pos
        integer, allocatable :: original(:)
        integer :: orig_size, insert_size

        if (allocated(target%body_indices)) then
            original = target%body_indices
            deallocate (target%body_indices)
        else
            allocate (original(0))
        end if

        orig_size = size(original)
        insert_size = size(procedures)
        allocate (target%body_indices(orig_size + insert_size))

        if (contains_pos >= 1) then
            target%body_indices(1:contains_pos) = original(1:contains_pos)
        end if
        target%body_indices(contains_pos + 1:contains_pos + insert_size) = procedures
        if (contains_pos < orig_size) then
            target%body_indices(contains_pos + insert_size + 1:) = &
                original(contains_pos + 1:orig_size)
        end if
    end subroutine insert_procedures_after_contains

    subroutine clean_external_declarations(arena, target, procedures)
        type(ast_arena_t), intent(in) :: arena
        class(program_node), intent(inout) :: target
        integer, intent(in) :: procedures(:)
        integer :: i, j
        integer, allocatable :: compressed(:)
        logical :: declares_function

        if (.not. allocated(target%body_indices)) return

        do i = 1, size(target%body_indices)
            if (target%body_indices(i) <= 0 .or. &
                target%body_indices(i) > arena%size) cycle
            if (.not. allocated(arena%entries(target%body_indices(i))%node)) cycle

            select type (stmt => arena%entries(target%body_indices(i))%node)
            type is (declaration_node)
                declares_function = stmt%is_external
                if (stmt%is_multi_declaration .and. allocated(stmt%var_names)) then
                    do j = 1, size(stmt%var_names)
                        if (is_procedure_name(trim(stmt%var_names(j)), arena, &
                                              procedures)) then
                            declares_function = .true.
                            exit
                        end if
                    end do
                else
                    if (is_procedure_name(trim(stmt%var_name), arena, procedures)) then
                        declares_function = .true.
                    end if
                end if
                if (declares_function) target%body_indices(i) = 0
            end select
        end do

        allocate (compressed(0))
        do i = 1, size(target%body_indices)
            if (target%body_indices(i) /= 0) then
                compressed = [compressed, target%body_indices(i)]
            end if
        end do
        target%body_indices = compressed
    end subroutine clean_external_declarations

    subroutine merge_additional_main_programs(arena, root_prog, target_prog_idx)
        type(ast_arena_t), intent(inout) :: arena
        class(program_node), intent(inout) :: root_prog
        integer, intent(in) :: target_prog_idx
        integer, allocatable :: new_body(:)
        integer :: i, child_idx
        class(program_node), pointer :: target_prog => null()

        if (.not. allocated(root_prog%body_indices)) return
        if (target_prog_idx <= 0 .or. target_prog_idx > arena%size) return
        if (.not. allocated(arena%entries(target_prog_idx)%node)) return
        select type (target => arena%entries(target_prog_idx)%node)
        type is (program_node)
            target_prog => target
        class default
            return
        end select
        if (.not. associated(target_prog)) return

        allocate (new_body(0))
        do i = 1, size(root_prog%body_indices)
            child_idx = root_prog%body_indices(i)
            if (child_idx == target_prog_idx) then
                new_body = [new_body, child_idx]
                cycle
            end if
            if (child_idx <= 0 .or. child_idx > arena%size) then
                new_body = [new_body, child_idx]
                cycle
            end if
            if (.not. allocated(arena%entries(child_idx)%node)) then
                new_body = [new_body, child_idx]
                cycle
            end if
            select type (child => arena%entries(child_idx)%node)
            type is (program_node)
                if (trim(child%name) == "main" .or. &
                    trim(child%name) == "__IMPLICIT_MAIN__") then
                    call append_program_body_to_target(arena, target_prog_idx, &
                                                       child_idx)
                    cycle
                end if
            end select
            new_body = [new_body, child_idx]
        end do
        root_prog%body_indices = new_body
    end subroutine merge_additional_main_programs

    subroutine append_program_body_to_target(arena, target_prog_idx, &
                                             source_prog_idx)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: target_prog_idx
        integer, intent(in) :: source_prog_idx
        integer :: i, child_idx

        if (source_prog_idx <= 0 .or. source_prog_idx > arena%size) return
        if (.not. allocated(arena%entries(source_prog_idx)%node)) return
        if (.not. allocated(arena%entries(target_prog_idx)%node)) return

        select type (target => arena%entries(target_prog_idx)%node)
        type is (program_node)
            select type (source => arena%entries(source_prog_idx)%node)
            type is (program_node)
                if (.not. allocated(source%body_indices)) return
                if (.not. allocated(target%body_indices)) then
                    target%body_indices = source%body_indices
                else
                    target%body_indices = [target%body_indices, &
                                           source%body_indices]
                end if
                do i = 1, size(source%body_indices)
                    child_idx = source%body_indices(i)
                    if (child_idx <= 0 .or. child_idx > arena%size) cycle
                    arena%entries(child_idx)%parent_index = target_prog_idx
                end do
                deallocate (source%body_indices)
            end select
        end select
    end subroutine append_program_body_to_target

    function collect_target_procedures(arena, target_prog_idx) result(proc_indices)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: target_prog_idx
        integer, allocatable :: proc_indices(:)
        integer :: i, child_idx

        allocate (proc_indices(0))
        if (target_prog_idx <= 0 .or. target_prog_idx > arena%size) return
        if (.not. allocated(arena%entries(target_prog_idx)%node)) return

        select type (target => arena%entries(target_prog_idx)%node)
        type is (program_node)
            if (.not. allocated(target%body_indices)) return
            do i = 1, size(target%body_indices)
                child_idx = target%body_indices(i)
                if (child_idx <= 0 .or. child_idx > arena%size) cycle
                if (.not. allocated(arena%entries(child_idx)%node)) cycle
                select type (child => arena%entries(child_idx)%node)
                type is (function_def_node)
                    proc_indices = [proc_indices, child_idx]
                type is (subroutine_def_node)
                    proc_indices = [proc_indices, child_idx]
                end select
            end do
        end select
    end function collect_target_procedures

    subroutine remove_target_procedures_from_body(arena, target_prog_idx, &
                                                  procedures)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: target_prog_idx
        integer, intent(in) :: procedures(:)
        integer, allocatable :: new_body(:)
        integer :: i

        if (size(procedures) == 0) return
        if (target_prog_idx <= 0 .or. target_prog_idx > arena%size) return
        if (.not. allocated(arena%entries(target_prog_idx)%node)) return

        select type (target => arena%entries(target_prog_idx)%node)
        type is (program_node)
            if (.not. allocated(target%body_indices)) return
            allocate (new_body(0))
            do i = 1, size(target%body_indices)
                if (any(target%body_indices(i) == procedures)) cycle
                new_body = [new_body, target%body_indices(i)]
            end do
            target%body_indices = new_body
        end select
    end subroutine remove_target_procedures_from_body

    subroutine normalize_multi_unit_container(arena, root_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(inout) :: root_index
        integer :: i, target_prog_idx, contains_pos
        integer, allocatable :: procedures(:), all_procedures(:)
        integer, allocatable :: embedded_procs(:), lifted_procs(:)
        class(program_node), pointer :: root_prog => null()
        integer :: first_main_idx

        if (root_index <= 0 .or. root_index > arena%size) return
        if (.not. allocated(arena%entries(root_index)%node)) return

        select type (root => arena%entries(root_index)%node)
        type is (program_node)
            if (trim(root%name) /= "__MULTI_UNIT__") return
            root_prog => root
        class default
            return
        end select
        if (.not. associated(root_prog)) return

        call collect_procedures_and_target(arena, root_prog, all_procedures, &
                                           target_prog_idx)

        if (target_prog_idx == 0) return
        if (size(all_procedures) == 0) return

        if (target_prog_idx <= 0 .or. target_prog_idx > arena%size) return
        if (.not. allocated(arena%entries(target_prog_idx)%node)) return
        select type (target_prog => arena%entries(target_prog_idx)%node)
        type is (program_node)
            if (trim(target_prog%name) /= "main" .and. &
                trim(target_prog%name) /= "__IMPLICIT_MAIN__") return
        class default
            return
        end select

        call merge_additional_main_programs(arena, root_prog, target_prog_idx)

        embedded_procs = collect_target_procedures(arena, target_prog_idx)
        call filter_hoistable_procedures(arena, all_procedures, target_prog_idx, &
                                         lifted_procs)

        if (size(embedded_procs) == 0 .and. size(lifted_procs) == 0) return

        call remove_target_procedures_from_body(arena, target_prog_idx, &
                                                embedded_procs)
        call remove_procedures_from_body(root_prog, lifted_procs)

        procedures = embedded_procs
        if (size(lifted_procs) > 0) then
            procedures = [procedures, lifted_procs]
        end if

        if (.not. allocated(arena%entries(target_prog_idx)%node)) return
        select type (target => arena%entries(target_prog_idx)%node)
        type is (program_node)
            contains_pos = find_contains_position(arena, target)
            call ensure_contains_exists(arena, target, target_prog_idx, contains_pos)
            call insert_procedures_after_contains(target, procedures, contains_pos)

            do i = 1, size(procedures)
                if (procedures(i) > 0 .and. procedures(i) <= arena%size) then
                    arena%entries(procedures(i))%parent_index = target_prog_idx
                end if
            end do

            call clean_external_declarations(arena, target, procedures)
        end select
    end subroutine normalize_multi_unit_container

    logical function is_procedure_name(name, arena, proc_indices) result(match)
        character(len=*), intent(in) :: name
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: proc_indices(:)
        integer :: k

        match = .false.
        do k = 1, size(proc_indices)
            if (proc_indices(k) <= 0 .or. proc_indices(k) > arena%size) cycle
            if (.not. allocated(arena%entries(proc_indices(k))%node)) cycle
            select type (proc_node => arena%entries(proc_indices(k))%node)
            type is (function_def_node)
                if (trim(proc_node%name) == trim(name)) then
                    match = .true.
                    return
                end if
            type is (subroutine_def_node)
                if (trim(proc_node%name) == trim(name)) then
                    match = .true.
                    return
                end if
            end select
        end do
    end function is_procedure_name

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
        use ast_nodes_procedure, only: function_def_node, subroutine_def_node
        use ast_nodes_data, only: declaration_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: prog_index
        character(len=8) :: flag
        integer :: status
        integer :: i, j
        character(len=:), allocatable :: child_type

        call get_environment_variable('FORTFRONT_DEBUG_DUMP_AST', flag, status=status)
        if (status /= 0) return
        if (len_trim(flag) == 0) return

        write (error_unit, '(A)') 'DEBUG AST: program overview'
        write (error_unit, '(A,I0)') '  root index: ', prog_index
        do i = 1, min(arena%size, size(arena%entries))
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (node => arena%entries(i)%node)
            type is (program_node)
                if (allocated(node%body_indices)) then
                    write (error_unit, '(A,I0,2X,A,2X,I0)') &
                        '  program idx', i, trim(node%name), size(node%body_indices)
                    do j = 1, size(node%body_indices)
                        if (node%body_indices(j) <= 0) cycle
                        if (node%body_indices(j) > arena%size) cycle
                        if (allocated(arena%entries(node%body_indices(j))%node_type)) then
                            child_type = trim(arena%entries(node%body_indices(j))%node_type)
                        else
                            child_type = '<unknown>'
                        end if
                        write (error_unit, '(A,1X,I0,2X,A)') '    body', &
                            node%body_indices(j), child_type
                    end do
                else
                    write (error_unit, '(A,I0,2X,A,2X,A)') &
                        '  program idx', i, trim(node%name), 'no body'
                end if
            type is (function_def_node)
                write (error_unit, '(A,I0,2X,A)') '  function idx', i, trim(node%name)
            type is (subroutine_def_node)
                write (error_unit, '(A,I0,2X,A)') '  subroutine idx', i, &
                    trim(node%name)
            type is (declaration_node)
                write (error_unit, '(A,I0,2X,A,2X,A)') '  decl idx', i, &
                    trim(node%type_name), trim(node%var_name)
            end select
        end do
    end subroutine maybe_dump_program_overview

    ! Save current configuration
    subroutine save_current_configuration(saved_size, saved_char, saved_line_length, &
                                          saved_standardize_types, &
                                              & saved_standardizer_types)
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
                input(i:i) /= new_line('A')) then  ! newline
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
            deallocate (block_text)
            return
        end if
        if (src(i:i) /= '!') then
            deallocate (block_text)
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
            deallocate (block_text)
        end if
    end function extract_leading_comment_block

    ! ========================================================================
    ! AST-BASED WRAPPING FUNCTIONS (Clean, proper compiler architecture)
    ! ========================================================================

    recursive subroutine mark_procedure_subtree(arena, node_index, membership)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        logical, intent(inout) :: membership(:)
        integer :: child_idx, child_count

        if (node_index <= 0) return
        if (node_index > size(membership)) return
        if (membership(node_index)) return

        membership(node_index) = .true.

        if (.not. allocated(arena%entries(node_index)%child_indices)) return
        child_count = size(arena%entries(node_index)%child_indices)
        if (child_count == 0) return
        do child_idx = 1, child_count
            call mark_procedure_subtree(arena, &
                                   arena%entries(node_index)%child_indices(child_idx), &
                                        membership)
        end do
    end subroutine mark_procedure_subtree

    subroutine build_procedure_membership(arena, membership)
        type(ast_arena_t), intent(in) :: arena
        logical, allocatable, intent(out) :: membership(:)
        integer :: i, j

        if (arena%size <= 0) then
            allocate (membership(0))
            return
        end if

        allocate (membership(arena%size))
        membership = .false.

        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (proc => arena%entries(i)%node)
            type is (function_def_node)
                if (allocated(proc%body_indices)) then
                    do j = 1, size(proc%body_indices)
                        call mark_procedure_subtree(arena, proc%body_indices(j), &
                                                    membership)
                    end do
                end if
            type is (subroutine_def_node)
                if (allocated(proc%body_indices)) then
                    do j = 1, size(proc%body_indices)
                        call mark_procedure_subtree(arena, proc%body_indices(j), &
                                                    membership)
                    end do
                end if
            end select
        end do
    end subroutine build_procedure_membership

    ! Analyze AST content directly (no string manipulation)

    subroutine analyze_ast_content(arena, root_index, has_functions, &
                                   has_subroutines, has_main_code)
        use ast_nodes_io, only: print_statement_node
        use ast_nodes_control, only: if_node, do_loop_node
        use ast_nodes_procedure, only: subroutine_call_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        logical, intent(out) :: has_functions, has_subroutines, has_main_code
        integer :: i, j
        logical, allocatable :: in_procedure(:)

        has_functions = .false.
        has_subroutines = .false.
        has_main_code = .false.

        ! Check if root is already a module - if so, don't wrap
        if (root_index > 0 .and. root_index <= arena%size) then
            if (allocated(arena%entries(root_index)%node)) then
                select type (root => arena%entries(root_index)%node)
                type is (module_node)
                    ! Already a module, no wrapping needed
                    return
                type is (program_node)
                    ! Check if this is a multi-unit container
                    if (root%name == "__MULTI_UNIT__" .and. &
                        allocated(root%body_indices)) then
                        ! Scan child units for functions, subroutines, and main code
                        do j = 1, size(root%body_indices)
                            call analyze_single_unit(arena, root%body_indices(j), &
                                                     has_functions, has_subroutines, &
                                                     has_main_code)
                        end do
                        return
                    end if
                end select
            end if
        end if

        ! For non-multi-unit roots, scan all nodes in arena
        call build_procedure_membership(arena, in_procedure)

        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle

            select type (node => arena%entries(i)%node)
            type is (function_def_node)
                has_functions = .true.
            type is (subroutine_def_node)
                has_subroutines = .true.
            type is (assignment_node)
                ! Assignment outside of procedures = main code
                if (.not. in_procedure(i)) then
                    has_main_code = .true.
                end if
            type is (print_statement_node)
                ! Print statement outside of procedures = main code
                if (.not. in_procedure(i)) then
                    has_main_code = .true.
                end if
            type is (if_node)
                ! Control flow outside of procedures = main code
                if (.not. in_procedure(i)) then
                    has_main_code = .true.
                end if
            type is (do_loop_node)
                ! Loop outside of procedures = main code
                if (.not. in_procedure(i)) then
                    has_main_code = .true.
                end if
            type is (subroutine_call_node)
                ! Subroutine call outside of procedures = main code
                if (.not. in_procedure(i)) then
                    has_main_code = .true.
                end if
            end select
        end do
    end subroutine analyze_ast_content

    ! Analyze a single unit (program, function, or subroutine) for content
    subroutine analyze_single_unit(arena, unit_index, has_functions, &
                                   has_subroutines, has_main_code)
        use ast_nodes_io, only: print_statement_node
        use ast_nodes_control, only: if_node, do_loop_node
        use ast_nodes_procedure, only: subroutine_call_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: unit_index
        logical, intent(inout) :: has_functions, has_subroutines, has_main_code
        integer :: i

        if (unit_index <= 0 .or. unit_index > arena%size) return
        if (.not. allocated(arena%entries(unit_index)%node)) return

        select type (unit => arena%entries(unit_index)%node)
        type is (function_def_node)
            has_functions = .true.
        type is (subroutine_def_node)
            has_subroutines = .true.
        type is (program_node)
            ! Scan program body for executable statements
            if (allocated(unit%body_indices)) then
                do i = 1, size(unit%body_indices)
                    if (unit%body_indices(i) <= 0 .or. &
                        unit%body_indices(i) > arena%size) cycle
                    if (.not. allocated(arena%entries(unit%body_indices(i))%node)) &
                        cycle

                    select type (stmt => arena%entries(unit%body_indices(i))%node)
                    type is (assignment_node)
                        has_main_code = .true.
                    type is (print_statement_node)
                        has_main_code = .true.
                    type is (if_node)
                        has_main_code = .true.
                    type is (do_loop_node)
                        has_main_code = .true.
                    type is (subroutine_call_node)
                        has_main_code = .true.
                    type is (function_def_node)
                        has_functions = .true.
                    type is (subroutine_def_node)
                        has_subroutines = .true.
                    end select
                end do
            end if
        end select
    end subroutine analyze_single_unit

    subroutine promote_functions_to_internal_program(arena, root_index)
        use ast_nodes_core, only: program_node
        use ast_nodes_procedure, only: function_def_node, subroutine_def_node, &
                                        subroutine_call_node
        use ast_nodes_misc, only: contains_node, implicit_statement_node, &
                                  end_statement_node, comment_node, &
                                  directive_node, blank_line_node
        use ast_nodes_io, only: print_statement_node
        use ast_nodes_control, only: if_node, do_loop_node
        use ast_nodes_data, only: mixed_construct_container_node, declaration_node
        use standardizer_program, only: insert_contains_statement
        use ast_factory, only: push_implicit_statement
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(inout) :: root_index
        integer :: i, child_index, main_prog_index, candidate_prog_index
        integer :: idx, body_size, n_proc, contains_pos, pos
        integer, allocatable :: proc_indices(:)
        integer, allocatable :: new_body(:)
        integer, allocatable :: main_stmts(:)
        integer, allocatable :: filtered_body(:)
        logical :: has_contains, child_is_main_candidate, has_exec, has_procs
        integer :: contains_index, implicit_none_index, prog_index

        if (root_index <= 0 .or. root_index > arena%size) return
        if (.not. allocated(arena%entries(root_index)%node)) return

        allocate (proc_indices(0))
        allocate (main_stmts(0))
        main_prog_index = 0
        candidate_prog_index = 0

        select type (root => arena%entries(root_index)%node)
        type is (mixed_construct_container_node)
! Handle mixed constructs: implicit declarations (main code) + explicit programs (functions)
            if (allocated(root%implicit_declaration_indices)) then
                main_stmts = root%implicit_declaration_indices
            end if
            if (allocated(root%explicit_program_indices)) then
                do i = 1, size(root%explicit_program_indices)
                    child_index = root%explicit_program_indices(i)
                    if (child_index <= 0 .or. child_index > arena%size) cycle
                    if (.not. allocated(arena%entries(child_index)%node)) cycle
                    select type (child => arena%entries(child_index)%node)
                    type is (function_def_node)
                        proc_indices = [proc_indices, child_index]
                    type is (subroutine_def_node)
                        proc_indices = [proc_indices, child_index]
                    end select
                end do
            end if

            ! Create a program with main code and internal procedures
            ! Handle case where we have procedures (with or without main code)
            if (size(proc_indices) > 0) then
     ! Create program structure: implicit none + main statements + contains + procedures
                implicit_none_index = push_implicit_statement(arena, .true., &
                                                              line=1, column=1, &
                                                              parent_index=0)

           ! Build program body: implicit none + main statements + contains + procedures
                body_size = 1 + size(main_stmts) + 1 + size(proc_indices)
                allocate (new_body(body_size))
                new_body(1) = implicit_none_index
                do i = 1, size(main_stmts)
                    new_body(1 + i) = main_stmts(i)
                end do

                ! Add contains statement
                block
                    type(contains_node) :: contains_stmt
                    contains_stmt%line = 1
                    contains_stmt%column = 1
                    call arena%push(contains_stmt, "contains", 0)
                    contains_index = arena%size
                end block
                new_body(1 + size(main_stmts) + 1) = contains_index

                ! Add procedures
                do i = 1, size(proc_indices)
                    new_body(1 + size(main_stmts) + 1 + i) = proc_indices(i)
                end do

                ! Create program node
                block
                    type(program_node) :: prog
                    prog%name = "main"
                    prog%body_indices = new_body
                    prog%line = 1
                    prog%column = 1
                    call arena%push(prog, "program", 0)
                    prog_index = arena%size
                end block

                ! Update parent indices
                arena%entries(implicit_none_index)%parent_index = prog_index
                do i = 1, size(main_stmts)
                    arena%entries(main_stmts(i))%parent_index = prog_index
                end do
                arena%entries(contains_index)%parent_index = prog_index
                do i = 1, size(proc_indices)
                    arena%entries(proc_indices(i))%parent_index = prog_index
                end do

                root_index = prog_index
                return
            end if

        type is (program_node)
            if (trim(root%name) /= "__MULTI_UNIT__") return
            if (.not. allocated(root%body_indices)) return

            do i = 1, size(root%body_indices)
                child_index = root%body_indices(i)
                if (child_index <= 0 .or. child_index > arena%size) cycle
                if (.not. allocated(arena%entries(child_index)%node)) cycle

                select type (child => arena%entries(child_index)%node)
                type is (program_node)
                    child_is_main_candidate = .false.
                    has_exec = program_has_executable_statements(arena, &
                                                                child_index)
                    has_procs = program_contains_procedures(arena, child_index)
                    if (main_prog_index == 0) then
                        if (trim(child%name) /= "__MULTI_UNIT__") then
                            if (has_exec .and. .not. has_procs) then
                                main_prog_index = child_index
                                child_is_main_candidate = .true.
                            else if (has_exec .and. candidate_prog_index == 0) then
                                candidate_prog_index = child_index
                            end if
                        end if
                    else if (child_index == main_prog_index) then
                        child_is_main_candidate = .true.
                    end if
                    call collect_program_procedures(child_index)
                    if (.not. child_is_main_candidate) then
                        if (.not. has_procs) then
                            call append_program_statements(child_index)
                        end if
                    else
                        cycle
                    end if
                type is (function_def_node)
                    proc_indices = [proc_indices, child_index]
                type is (subroutine_def_node)
                    proc_indices = [proc_indices, child_index]
                class default
                    ! Non-procedure, non-program statements (bare statements)
                    ! Always collect them - we'll merge with program node if one exists
                    main_stmts = [main_stmts, child_index]
                end select
            end do

            if (main_prog_index == 0 .and. candidate_prog_index > 0) then
                main_prog_index = candidate_prog_index
            end if
        class default
            return
        end select

        ! If we have procedures but no program node, create one from bare statements
        if (main_prog_index == 0 .and. size(proc_indices) > 0) then
            ! Create program structure with bare statements + contains + procedures
            implicit_none_index = push_implicit_statement(arena, .true., &
                                                          line=1, column=1, &
                                                          parent_index=0)

            body_size = 1 + size(main_stmts) + 1 + size(proc_indices)
            allocate (new_body(body_size))
            new_body(1) = implicit_none_index
            do i = 1, size(main_stmts)
                new_body(1 + i) = main_stmts(i)
            end do

            ! Add contains statement
            block
                type(contains_node) :: contains_stmt
                contains_stmt%line = 1
                contains_stmt%column = 1
                call arena%push(contains_stmt, "contains", 0)
                contains_index = arena%size
            end block
            new_body(1 + size(main_stmts) + 1) = contains_index

            ! Add procedures
            do i = 1, size(proc_indices)
                new_body(1 + size(main_stmts) + 1 + i) = proc_indices(i)
            end do

            ! Create program node
            block
                type(program_node) :: prog
                prog%name = "main"
                prog%body_indices = new_body
                prog%line = 1
                prog%column = 1
                call arena%push(prog, "program", 0)
                main_prog_index = arena%size
            end block

            ! Update parent indices
            arena%entries(implicit_none_index)%parent_index = main_prog_index
            do i = 1, size(main_stmts)
                arena%entries(main_stmts(i))%parent_index = main_prog_index
            end do
            arena%entries(contains_index)%parent_index = main_prog_index
            do i = 1, size(proc_indices)
                arena%entries(proc_indices(i))%parent_index = main_prog_index
            end do

            ! Replace the __MULTI_UNIT__ children with just the new program
            select type (root_prog => arena%entries(root_index)%node)
            type is (program_node)
                deallocate (root_prog%body_indices)
                allocate (root_prog%body_indices(1))
                root_prog%body_indices(1) = main_prog_index
                arena%entries(main_prog_index)%parent_index = root_index
            end select

            return
        end if

        if (main_prog_index == 0) return
        if (size(proc_indices) == 0) return

        select type (main_prog => arena%entries(main_prog_index)%node)
        type is (program_node)
            if (.not. allocated(main_prog%body_indices)) then
                allocate (main_prog%body_indices(0))
            end if

            has_contains = .false.
            do i = 1, size(main_prog%body_indices)
                idx = main_prog%body_indices(i)
                if (idx <= 0 .or. idx > arena%size) cycle
                if (.not. allocated(arena%entries(idx)%node)) cycle
                select type (body_node => arena%entries(idx)%node)
                type is (contains_node)
                    has_contains = .true.
                end select
            end do

            if (.not. has_contains) then
                call insert_contains_statement(arena, main_prog, main_prog_index, &
                                               size(main_prog%body_indices) + 1)
            end if

            ! Merge: existing program body + bare statements + procedures
            if (allocated(main_prog%body_indices)) then
                allocate (filtered_body(0))
                do i = 1, size(main_prog%body_indices)
                    idx = main_prog%body_indices(i)
                    if (idx <= 0 .or. idx > arena%size) cycle
                    if (.not. allocated(arena%entries(idx)%node)) cycle
                    select type (body_node => arena%entries(idx)%node)
                    type is (function_def_node)
                        cycle
                    type is (subroutine_def_node)
                        cycle
                    class default
                        filtered_body = [filtered_body, idx]
                    end select
                end do
            else
                allocate (filtered_body(0))
            end if

            body_size = size(filtered_body)
            n_proc = size(proc_indices)
            allocate (new_body(body_size + size(main_stmts) + n_proc))
            pos = 0
            contains_pos = 0
            do i = 1, body_size
                idx = filtered_body(i)
                if (idx <= 0 .or. idx > arena%size) cycle
                if (.not. allocated(arena%entries(idx)%node)) cycle
                select type (body_node => arena%entries(idx)%node)
                type is (contains_node)
                    contains_pos = i
                    exit
                end select
            end do
            if (contains_pos > 0) then
                if (contains_pos > 1) then
                    new_body(1:contains_pos - 1) = filtered_body(1:contains_pos - 1)
                    pos = contains_pos - 1
                end if
                if (size(main_stmts) > 0) then
                    new_body(pos + 1:pos + size(main_stmts)) = main_stmts
                    pos = pos + size(main_stmts)
                end if
                new_body(pos + 1) = filtered_body(contains_pos)
                pos = pos + 1
                if (contains_pos < body_size) then
                    new_body(pos + 1:pos + (body_size - contains_pos)) = &
                        filtered_body(contains_pos + 1:body_size)
                    pos = pos + (body_size - contains_pos)
                end if
            else
                if (body_size > 0) then
                    new_body(1:body_size) = filtered_body
                    pos = body_size
                end if
                if (size(main_stmts) > 0) then
                    new_body(pos + 1:pos + size(main_stmts)) = main_stmts
                    pos = pos + size(main_stmts)
                end if
            end if
            if (n_proc > 0) then
                new_body(pos + 1:pos + n_proc) = proc_indices
            end if
            main_prog%body_indices = new_body

            ! Update parent indices for the newly added bare statements
            do i = 1, size(main_stmts)
                arena%entries(main_stmts(i))%parent_index = main_prog_index
            end do
        end select

        do i = 1, size(proc_indices)
            arena%entries(proc_indices(i))%parent_index = main_prog_index
        end do

        root_index = main_prog_index

    contains

        subroutine collect_program_procedures(program_idx)
            integer, intent(in) :: program_idx
            integer :: j, stmt_idx

            if (program_idx <= 0 .or. program_idx > arena%size) return
            if (.not. allocated(arena%entries(program_idx)%node)) return

            select type (prog => arena%entries(program_idx)%node)
            type is (program_node)
                if (.not. allocated(prog%body_indices)) return
                do j = 1, size(prog%body_indices)
                    stmt_idx = prog%body_indices(j)
                    if (stmt_idx <= 0 .or. stmt_idx > arena%size) cycle
                    if (.not. allocated(arena%entries(stmt_idx)%node)) cycle
                    select type (stmt => arena%entries(stmt_idx)%node)
                    type is (function_def_node)
                        proc_indices = [proc_indices, stmt_idx]
                    type is (subroutine_def_node)
                        proc_indices = [proc_indices, stmt_idx]
                    end select
                end do
            end select
        end subroutine collect_program_procedures

        subroutine append_program_statements(program_idx)
            integer, intent(in) :: program_idx
            integer :: j, stmt_idx

            if (program_idx <= 0 .or. program_idx > arena%size) return
            if (.not. allocated(arena%entries(program_idx)%node)) return

            select type (prog => arena%entries(program_idx)%node)
            type is (program_node)
                if (.not. allocated(prog%body_indices)) return
                do j = 1, size(prog%body_indices)
                    stmt_idx = prog%body_indices(j)
                    if (stmt_idx <= 0 .or. stmt_idx > arena%size) cycle
                    if (.not. allocated(arena%entries(stmt_idx)%node)) cycle
                    select type (stmt => arena%entries(stmt_idx)%node)
                    type is (function_def_node)
                        cycle
                    type is (subroutine_def_node)
                        cycle
                    type is (implicit_statement_node)
                        cycle
                    type is (contains_node)
                        cycle
                    type is (end_statement_node)
                        cycle
                    class default
                        main_stmts = [main_stmts, stmt_idx]
                    end select
                end do
            end select
        end subroutine append_program_statements

        logical function program_has_executable_statements(arena, program_idx) &
            result(has_exec)
            type(ast_arena_t), intent(in) :: arena
            integer, intent(in) :: program_idx
            integer :: j, stmt_idx

            has_exec = .false.
            if (program_idx <= 0 .or. program_idx > arena%size) return
            if (.not. allocated(arena%entries(program_idx)%node)) return

            select type (prog => arena%entries(program_idx)%node)
            type is (program_node)
                if (.not. allocated(prog%body_indices)) return
                do j = 1, size(prog%body_indices)
                    stmt_idx = prog%body_indices(j)
                    if (stmt_idx <= 0 .or. stmt_idx > arena%size) cycle
                    if (.not. allocated(arena%entries(stmt_idx)%node)) cycle
                    select type (stmt => arena%entries(stmt_idx)%node)
                    type is (function_def_node)
                        cycle
                    type is (subroutine_def_node)
                        cycle
                    type is (implicit_statement_node)
                        cycle
                    type is (contains_node)
                        exit
                    type is (end_statement_node)
                        cycle
                    type is (comment_node)
                        cycle
                    type is (directive_node)
                        cycle
                    type is (blank_line_node)
                        cycle
                    type is (declaration_node)
                        cycle
                    class default
                        has_exec = .true.
                        return
                    end select
                end do
            end select
        end function program_has_executable_statements

        logical function program_contains_procedures(arena, program_idx) &
            result(has_procs)
            type(ast_arena_t), intent(in) :: arena
            integer, intent(in) :: program_idx
            integer :: j, stmt_idx

            has_procs = .false.
            if (program_idx <= 0 .or. program_idx > arena%size) return
            if (.not. allocated(arena%entries(program_idx)%node)) return

            select type (prog => arena%entries(program_idx)%node)
            type is (program_node)
                if (.not. allocated(prog%body_indices)) return
                do j = 1, size(prog%body_indices)
                    stmt_idx = prog%body_indices(j)
                    if (stmt_idx <= 0 .or. stmt_idx > arena%size) cycle
                    if (.not. allocated(arena%entries(stmt_idx)%node)) cycle
                    select type (stmt => arena%entries(stmt_idx)%node)
                    type is (function_def_node)
                        has_procs = .true.
                        return
                    type is (subroutine_def_node)
                        has_procs = .true.
                        return
                    end select
                end do
            end select
        end function program_contains_procedures
    end subroutine promote_functions_to_internal_program

    ! Check if AST already contains a module node
    function has_existing_module_in_ast(arena) result(has_module)
        type(ast_arena_t), intent(in) :: arena
        logical :: has_module
        integer :: i

        has_module = .false.

        ! Scan all nodes in arena for a module node
        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (node => arena%entries(i)%node)
            type is (module_node)
                has_module = .true.
                exit
            end select
        end do
    end function has_existing_module_in_ast

    logical function requires_lazy_internalization(arena, prog_index) &
        result(needs_wrapping)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: prog_index
        integer :: i, idx

        needs_wrapping = .false.
        if (prog_index <= 0 .or. prog_index > arena%size) return
        if (.not. allocated(arena%entries(prog_index)%node)) return

        select type (root => arena%entries(prog_index)%node)
        type is (program_node)
            if (trim(root%name) == "__MULTI_UNIT__") then
                if (.not. allocated(root%body_indices)) return
                needs_wrapping = .true.
                do i = 1, size(root%body_indices)
                    idx = root%body_indices(i)
                    if (idx <= 0 .or. idx > arena%size) cycle
                    if (.not. allocated(arena%entries(idx)%node)) cycle
                    select type (child => arena%entries(idx)%node)
                    type is (program_node)
                        if (.not. is_implicit_program_name(child%name)) then
                            needs_wrapping = .false.
                            return
                        end if
                    end select
                end do
            else
                needs_wrapping = is_implicit_program_name(root%name)
            end if
        class default
            needs_wrapping = .false.
        end select
    end function requires_lazy_internalization

    logical function is_implicit_program_name(name) result(is_implicit)
        character(len=*), intent(in) :: name

        select case (trim(name))
        case ("main", "__IMPLICIT_MAIN__")
            is_implicit = .true.
        case default
            is_implicit = .false.
        end select
    end function is_implicit_program_name

    subroutine collect_procedure_indices(arena, proc_indices)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(out) :: proc_indices(:)
        integer :: i

        allocate (proc_indices(0))
        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (node => arena%entries(i)%node)
            type is (function_def_node)
                proc_indices = [proc_indices, i]
            type is (subroutine_def_node)
                proc_indices = [proc_indices, i]
            end select
        end do
    end subroutine collect_procedure_indices

    subroutine create_module_with_procedures(arena, context, proc_indices, mod_index)
        type(ast_arena_t), intent(inout) :: arena
        type(transform_context_t), intent(in) :: context
        integer, intent(in) :: proc_indices(:)
        integer, intent(out) :: mod_index
        type(module_node) :: mod
        character(len=:), allocatable :: original_module_name
        integer :: i

        ! Check if there's already a module in the AST and preserve its name
        original_module_name = ""
        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (node => arena%entries(i)%node)
            type is (module_node)
                if (len(original_module_name) == 0) then
                    original_module_name = node%name
                    exit
                end if
            end select
        end do

        if (len(original_module_name) > 0) then
            mod%name = original_module_name
        else
            mod%name = context%module_name
        end if

        mod%has_contains = .true.
        mod%procedure_indices = proc_indices
        mod%line = 1
        mod%column = 1
        call arena%push(mod, "module", 0)
        mod_index = arena%size
    end subroutine create_module_with_procedures

! Wrap procedures in a module (AST manipulation, no strings!)
    subroutine wrap_ast_in_module_only(arena, root_index, context)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(inout) :: root_index
        type(transform_context_t), intent(in) :: context
        type(module_node) :: mod
        integer :: i, mod_index
        integer, allocatable :: proc_indices(:)

        call collect_procedure_indices(arena, proc_indices)

        if (size(proc_indices) == 0) return

        call create_module_with_procedures(arena, context, proc_indices, mod_index)

        ! Update parent indices of procedures
        do i = 1, size(proc_indices)
            arena%entries(proc_indices(i))%parent_index = mod_index
        end do

        ! Set new root
        root_index = mod_index
    end subroutine wrap_ast_in_module_only

    ! Wrap procedures in module and main code in program (AST manipulation)
    subroutine wrap_ast_in_module_and_program(arena, root_index, context)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(inout) :: root_index
        type(transform_context_t), intent(in) :: context
        type(module_node) :: mod
        type(program_node), pointer :: prog_ptr
        type(use_statement_node) :: use_stmt
        integer :: i, mod_index, use_index
        integer, allocatable :: proc_indices(:), new_body(:)

        call collect_procedure_indices(arena, proc_indices)

        if (size(proc_indices) == 0) return

        call create_module_with_procedures(arena, context, proc_indices, mod_index)

        ! Update procedure parent indices
        do i = 1, size(proc_indices)
            arena%entries(proc_indices(i))%parent_index = mod_index
        end do

        ! Remove procedures from program body and add use statement
        if (root_index > 0 .and. root_index <= arena%size) then
            if (allocated(arena%entries(root_index)%node)) then
                select type (prog => arena%entries(root_index)%node)
                type is (program_node)
                    ! Create use statement
                    use_stmt%module_name = context%module_name
                    use_stmt%line = 1
                    use_stmt%column = 1
                    call arena%push(use_stmt, "use", root_index)
                    use_index = arena%size

                    ! Remove procedure indices from program body
                    if (allocated(prog%body_indices)) then
                        allocate (new_body(0))
                        do i = 1, size(prog%body_indices)
                            if (.not. any(prog%body_indices(i) == proc_indices)) then
                                new_body = [new_body, prog%body_indices(i)]
                            end if
                        end do
                        ! Prepend use statement
                        prog%body_indices = [use_index, new_body]
                    end if
                end select
            end if
        end if

        ! Note: We don't change root_index because the program is still the root
        ! Code generation will emit both the module and the program
    end subroutine wrap_ast_in_module_and_program

    ! Run monomorphization phase (AST transformation)
    subroutine run_monomorphization_phase(compiler_arena, prog_index, signatures)
        type(compiler_arena_t), intent(inout) :: compiler_arena
        integer, intent(inout) :: prog_index
        type(signatures_map_t), intent(in) :: signatures
        logical :: trace_active

        call trace_init()
        call compiler_arena%next_phase("monomorphization")

        trace_active = trace_is_enabled()

        if (trace_active) then
            if (prog_index > 0 .and. prog_index <= compiler_arena%ast%size) then
                if (allocated(compiler_arena%ast%entries(prog_index)%node_type)) then
                    write (error_unit, '(A,1X,A)') 'DEBUG run_mono node_type', &
                        trim(compiler_arena%ast%entries(prog_index)%node_type)
                else
                    write (error_unit, '(A)') 'DEBUG run_mono node_type <not set>'
                end if
                if (allocated(compiler_arena%ast%entries(prog_index)%node)) then
                    select type (root_node => &
                                 compiler_arena%ast%entries(prog_index)%node)
                    type is (program_node)
                        write (error_unit, '(A,1X,A)') 'DEBUG run_mono root=program', &
                            trim(root_node%name)
                    type is (module_node)
                        write (error_unit, '(A,1X,A)') 'DEBUG run_mono root=module', &
                            trim(root_node%name)
                    class default
                        write (error_unit, '(A,1X,I0)') &
                            'DEBUG run_mono root=other index', prog_index
                    end select
                else
                    write (error_unit, '(A,1X,I0)') &
                        'DEBUG run_mono root node not allocated', prog_index
                end if
            else
                write (error_unit, '(A,1X,I0)') &
                    'DEBUG run_mono invalid prog_index', prog_index
            end if
        end if

        ! Transform AST to add monomorphized variants
        call transform_monomorphization(compiler_arena%ast, prog_index, signatures)
    end subroutine run_monomorphization_phase

end module frontend_transformation
