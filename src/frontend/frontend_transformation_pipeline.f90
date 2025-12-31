module frontend_transformation_pipeline
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
                            mark_pointer_targets, set_standardizer_input_mode
    use codegen_arena_interface, only: generate_code_from_arena
    use ast_monomorphization, only: transform_monomorphization
    use call_graph_signatures_mod, only: signatures_map_t, type_signature_t, &
                                         add_signature, create_signatures_map
    use codegen_basic_utils, only: add_line_continuations
    use codegen_core, only: initialize_codegen
    use codegen_type_utils, only: set_type_standardization, &
                                  get_type_standardization
    use codegen_indent, only: set_indent_config, get_indent_config, &
                              set_line_length_config, get_line_length_config
    use input_validation, only: validate_basic_syntax, has_only_meaningless_tokens
    use ast_nodes_core, only: program_node, assignment_node, &
                              identifier_node, call_or_subscript_node, &
                              component_access_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use ast_nodes_misc, only: contains_node, use_statement_node
    use ast_nodes_data, only: declaration_node, module_node, &
                              mixed_construct_container_node
    use frontend_parsing, only: parse_tokens
    use frontend_core, only: lex_source, emit_fortran
    use debug_trace, only: trace_init, trace_enter, trace_leave, trace_is_enabled
    use procedure_classification, only: should_hoist_procedure
    use semantic_input_mode, only: INPUT_MODE_LAZY, INPUT_MODE_STANDARD
    use semantic_operating_mode, only: OPERATING_MODE_INFER, OPERATING_MODE_STRICT
    use frontend_transformation_common, only: format_options_t, &
                                              transform_context_t, shared_arena, &
                                              shared_arena_initialized
    use frontend_transformation_structure, only: collect_procedures_and_target, &
                                                 filter_hoistable_procedures, &
                                                 remove_procedures_from_body, &
                                                 ensure_contains_exists, &
                                                 insert_procedures_after_contains, &
                                                 clean_external_declarations, &
                                                 merge_additional_main_programs, &
                                                 append_program_body_to_target, &
                                                 remove_target_procedures_from_body, &
                                                 normalize_multi_unit_container, &
                                                 collect_procedure_indices, &
                                                 create_module_with_procedures, &
                                                 wrap_ast_in_module_only, &
                                                 wrap_ast_in_module_and_program, &
                                                 run_code_generation_phase, &
                                                 is_whitespace_only, &
                                                 has_leading_comment, &
                                                 extract_leading_comment_block, &
                                                 contains_binary_data, &
                                                 save_current_configuration, &
                                                 restore_configuration, &
                                                 apply_format_options, &
                                                 decode_bom_if_needed
    use frontend_transformation_analysis, only: build_procedure_membership, &
                                                analyze_ast_content, &
                                                analyze_single_unit, &
                                                collect_host_assignment_names, &
                                                collect_program_assignment_names, &
                                                collect_procedure_assignment_names, &
                                                collect_assignment_from_node, &
                                                record_identifier_name, &
                                                append_unique_name, &
                                                promote_functions_to_internal_program, &
                                                requires_lazy_internalization, &
                                                has_existing_module_in_ast
    use frontend_location_validation, only: validate_ast_locations
    use frontend_transformation_semantics, only: analyze_container_semantics, &
                                                 merge_signature_maps, &
                                                 add_signature_from_entry, &
                                                 get_detailed_semantic_errors
    use frontend_diagnostics, only: make_diagnostic, format_diagnostic, &
                                    DIAG_BINARY_DATA, DIAG_NO_PROGRAM_UNIT, &
                                    DIAG_SYNTAX_ERROR, DIAGNOSTIC_ERROR
    use fortfront_types, only: diagnostic_t, source_range_t
    use frontend_pass_manager, only: pass_manager_t, pass_context_t, &
                                     pass_config_t, create_pass_manager, &
                                     PASS_SEMANTIC, PASS_STANDARDIZATION, &
                                     PASS_MONOMORPHIZATION, PASS_CODEGEN
    use frontend_final_passes, only: semantic_pass, standardization_pass, &
                                     monomorphization_pass, codegen_pass

    implicit none
    private

    public :: transform_lazy_fortran_string, &
              transform_lazy_fortran_string_with_format, &
              transform_with_context, &
              INPUT_MODE_LAZY, INPUT_MODE_STANDARD, &
              pass_config_t, create_pass_manager

contains
    ! String-based transformation function for CLI usage
    subroutine transform_lazy_fortran_string(input, output, error_msg, &
                                             enable_ast_wrapping, operating_mode)
        character(len=*), intent(in) :: input
        character(len=:), allocatable, intent(out) :: output
        character(len=:), allocatable, intent(out) :: error_msg
        logical, intent(in), optional :: enable_ast_wrapping
        integer, intent(in), optional :: operating_mode

        ! Local variables for 4-phase pipeline
        type(token_t), allocatable, target :: tokens(:)
        ! Use shared module-level arena for performance
        integer :: prog_index
        character(len=:), allocatable :: source
        logical :: apply_ast_wrapping
        integer :: local_operating_mode

        allocate (character(len=0) :: error_msg)
        error_msg = ""

        apply_ast_wrapping = .false.
        if (present(enable_ast_wrapping)) then
            apply_ast_wrapping = enable_ast_wrapping
        end if
        local_operating_mode = OPERATING_MODE_INFER
        if (present(operating_mode)) local_operating_mode = operating_mode

        call trace_init()

        call decode_bom_if_needed(input, source)
        source = normalize_line_endings(source)

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
            if (present(enable_ast_wrapping)) then
                ! Standard Fortran mode (context-aware call): do not fabricate
                ! a trivial program wrapper for empty input. This keeps
                ! standard Fortran round-trip semantics strict.
                output = ""
            else
                ! Lazy Fortran / library API: preserve legacy behaviour and
                ! generate a minimal program for empty input.
                call create_minimal_program(output)
            end if
            call trace_leave('transform_lazy_fortran_string')
            return
        end if

        if (contains_binary_data(source)) then
            block
                type(diagnostic_t) :: diag
                diag = make_diagnostic(DIAG_BINARY_DATA, DIAGNOSTIC_ERROR, &
                                       "Input appears to be binary data"// &
                                       new_line('A')// &
                                       "  Source: <binary data omitted>"// &
                                       new_line('A')// &
                                       "  Suggestion: Provide plain-text "// &
                                       "Fortran source")
                error_msg = format_diagnostic(diag)
            end block
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

        ! Phase 1.5: Enhanced syntax validation with detailed reporting (Issue #256)
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
            if (present(enable_ast_wrapping)) then
                ! Standard Fortran mode: inputs that tokenize to comments,
                ! whitespace or EOF should not gain a program wrapper.
                output = ""
            else
                call create_minimal_program(output)
            end if
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

        ! Optional: Validate AST locations after parsing
        call validate_locations_if_enabled(shared_arena%ast, 'post-parse')

        ! Phases 3-5: Semantic Analysis, Standardization, Code Generation
        call trace_enter('phase:final')
        call run_final_phases(shared_arena, prog_index, output, error_msg, &
                              apply_ast_wrapping, local_operating_mode)
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
        type(token_t), allocatable :: tokens(:)

        ! Set input mode in standardizer to distinguish standard vs lazy Fortran
        call set_standardizer_input_mode(context%input_mode)

        if (context%operating_mode == OPERATING_MODE_STRICT) then
            call tokenize_core(input, tokens)
            if (.not. allocated(tokens)) then
                block
                    type(diagnostic_t) :: diag
                    diag = make_diagnostic(DIAG_SYNTAX_ERROR, DIAGNOSTIC_ERROR, &
                                           "Failed to tokenize input for strict "// &
                                           "mode checks")
                    error_msg = format_diagnostic(diag)
                end block
                call create_minimal_program(output)
                return
            end if

            if (.not. tokens_have_explicit_program_unit(tokens)) then
                block
                    type(diagnostic_t) :: diag
                    diag = make_diagnostic(DIAG_SYNTAX_ERROR, DIAGNOSTIC_ERROR, &
                                           "Strict mode forbids bare statements "// &
                                           "without an explicit "// &
                                           "program/module/procedure unit"// &
                                           new_line('A')// &
                                           "Suggestion: use infer mode for "// &
                                           "top-level statements")
                    error_msg = format_diagnostic(diag)
                end block
                call create_minimal_program(output)
                return
            end if
        end if

        ! For standard Fortran mode, use simple transformation
        if (context%input_mode == INPUT_MODE_STANDARD) then
            call transform_lazy_fortran_string(input, output, error_msg, &
                                               enable_ast_wrapping=.false., &
                                               operating_mode=context%operating_mode)
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

        call decode_bom_if_needed(input, source)
        source = normalize_line_endings(source)

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

        call validate_syntax_with_reporting(source, tokens, error_msg, output, &
                                            shared_arena)
        if (error_msg /= "") then
            call trace_leave('transform_lazy_with_ast_wrapping')
            return
        end if

        if (context%operating_mode == OPERATING_MODE_STRICT) then
            if (.not. tokens_have_explicit_program_unit(tokens)) then
                block
                    type(diagnostic_t) :: diag
                    diag = make_diagnostic(DIAG_SYNTAX_ERROR, DIAGNOSTIC_ERROR, &
                                           "Strict mode forbids bare statements "// &
                                           "without an explicit "// &
                                           "program/module/procedure unit"// &
                                           new_line('A')// &
                                           "Suggestion: use infer mode for "// &
                                           "top-level statements")
                    error_msg = format_diagnostic(diag)
                end block
                call create_minimal_program(output)
                call trace_leave('transform_lazy_with_ast_wrapping')
                return
            end if
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
        call run_semantic_analysis_phase(shared_arena, prog_index, &
                                         context%operating_mode, error_msg, &
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

    logical function tokens_have_explicit_program_unit(tokens) result(has_unit)
        type(token_t), intent(in) :: tokens(:)
        integer :: i, next_idx
        character(len=:), allocatable :: lowered

        has_unit = .false.
        do i = 1, size(tokens)
            if (tokens(i)%kind /= TK_KEYWORD .and. &
                tokens(i)%kind /= TK_IDENTIFIER) cycle
            if (.not. allocated(tokens(i)%text)) cycle
            lowered = to_lower(trim(tokens(i)%text))
            select case (lowered)
            case ("program", "subroutine", "function", "submodule")
                has_unit = .true.
                return
            case ("block")
                next_idx = find_next_identifier_token(tokens, i)
                if (next_idx > 0) then
                    if (.not. allocated(tokens(next_idx)%text)) cycle
                    if (to_lower(trim(tokens(next_idx)%text)) == "data") then
                        has_unit = .true.
                        return
                    end if
                end if
            case ("module")
                next_idx = find_next_identifier_token(tokens, i)
                if (next_idx > 0) then
                    if (.not. allocated(tokens(next_idx)%text)) cycle
                    if (to_lower(trim(tokens(next_idx)%text)) /= "procedure") then
                        has_unit = .true.
                        return
                    end if
                end if
            end select
        end do
    end function tokens_have_explicit_program_unit

    integer function find_next_identifier_token(tokens, start_pos) result(idx)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        integer :: i

        idx = 0
        do i = start_pos + 1, size(tokens)
            select case (tokens(i)%kind)
            case (TK_IDENTIFIER)
                idx = i
                return
            case (TK_KEYWORD)
                idx = i
                return
            case default
                cycle
            end select
        end do
    end function find_next_identifier_token

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
            ! Keep stderr for diagnostics and stdout for usable Fortran output
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
        type(diagnostic_t) :: diag

        diag = make_diagnostic(DIAG_NO_PROGRAM_UNIT, DIAGNOSTIC_ERROR, &
                               "Parsing succeeded but no valid program unit "// &
                               "was created")
        error_msg = format_diagnostic(diag)
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

    ! Run final phases (semantic, standardization, codegen) using pass manager
    subroutine run_final_phases(compiler_arena, prog_index, output, error_msg, &
                                enable_ast_wrapping, operating_mode)
        type(compiler_arena_t), target, intent(inout) :: compiler_arena
        integer, intent(inout) :: prog_index
        character(len=:), allocatable, intent(out) :: output
        character(len=:), allocatable, intent(inout) :: error_msg
        logical, intent(in) :: enable_ast_wrapping
        integer, intent(in) :: operating_mode
        type(pass_manager_t) :: manager
        type(pass_context_t) :: pass_ctx

        ! Create and configure pass manager with default pipeline
        manager = create_pass_manager()

        ! Register passes in order
        call manager%add_pass(PASS_SEMANTIC, "Semantic Analysis", &
                              "phase:semantic", .true., semantic_pass)

        call manager%add_pass(PASS_STANDARDIZATION, "Standardization", &
                              "phase:standardization", .true., &
                              standardization_pass)

        call manager%add_pass(PASS_MONOMORPHIZATION, "Monomorphization", &
                              "phase:monomorphization", .true., &
                              monomorphization_pass)

        call manager%add_pass(PASS_CODEGEN, "Code Generation", &
                              "phase:codegen", .true., codegen_pass)

        ! Initialize pass context
        pass_ctx%compiler_arena => compiler_arena
        pass_ctx%prog_index = prog_index
        allocate (character(len=0) :: pass_ctx%error_msg)
        pass_ctx%operating_mode = operating_mode
        pass_ctx%enable_ast_wrapping = enable_ast_wrapping
        pass_ctx%has_functions = .false.
        pass_ctx%has_subroutines = .false.
        pass_ctx%has_main_code = .false.

        ! Run the pipeline
        call manager%run(pass_ctx)

        ! Extract results
        if (allocated(pass_ctx%output)) then
            output = pass_ctx%output
        else
            call create_minimal_program(output)
        end if

        if (allocated(pass_ctx%error_msg)) then
            error_msg = pass_ctx%error_msg
        else
            allocate (character(len=0) :: error_msg)
        end if

        prog_index = pass_ctx%prog_index

        ! Optional: Validate AST locations after standardization
        call validate_locations_if_enabled(compiler_arena%ast, 'post-standardize')

        ! Clean up
        call manager%clear()
    end subroutine run_final_phases

    ! Run semantic analysis phase
    subroutine run_semantic_analysis_phase(compiler_arena, prog_index, &
                                           operating_mode, error_msg, signatures)
        type(compiler_arena_t), intent(inout) :: compiler_arena
        integer, intent(in) :: prog_index
        integer, intent(in) :: operating_mode
        character(len=:), allocatable, intent(out) :: error_msg
        type(signatures_map_t), intent(out) :: signatures

        call compiler_arena%next_phase("semantic")
        block
            type(semantic_context_t) :: ctx
            logical :: handled

            handled = .false.
            call create_semantic_context(ctx)
            ctx%operating_mode = operating_mode

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

    ! Helper: Run location validation if enabled via environment variable
    subroutine validate_locations_if_enabled(arena, phase_name)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: phase_name
        character(len=256) :: env_value
        character(len=:), allocatable :: normalized
        integer :: status, violations
        logical :: strict_mode_enabled

        call get_environment_variable('FORTFRONT_VALIDATE_LOCATIONS', &
                                      env_value, status=status)
        if (status /= 0) return
        if (len_trim(env_value) == 0) return

        normalized = to_lower(trim(adjustl(env_value)))
        strict_mode_enabled = normalized == 'strict'

        ! Run validation and report violations
        call validate_ast_locations(arena, strict_mode=strict_mode_enabled, &
                                    violations_count=violations)
        if (violations > 0) then
            write (error_unit, '(A,A,A,I0,A)') &
                'Location validation (', trim(phase_name), '): ', &
                violations, ' violations detected'
        end if
    end subroutine validate_locations_if_enabled

end module frontend_transformation_pipeline
