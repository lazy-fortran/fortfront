    ! Run the arena-backed lexer/parser/semantic/codegen pipeline.
    ! Owns arena lifetime: initializes it, and destroys it on every exit path.
    ! pipeline_ok is .true. only when codegen completed without an error_msg.
    subroutine run_arena_pipeline(source, has_wrapping_flag, &
                                  apply_ast_wrapping, local_operating_mode, &
                                  apply_default_format, output, error_msg, &
                                  pipeline_ok)
        character(len=*), intent(in) :: source
        logical, intent(in) :: has_wrapping_flag
        logical, intent(in) :: apply_ast_wrapping
        integer, intent(in) :: local_operating_mode
        logical, intent(in) :: apply_default_format
        character(len=:), allocatable, intent(inout) :: output
        character(len=:), allocatable, intent(inout) :: error_msg
        logical, intent(out) :: pipeline_ok
        type(token_t), allocatable, target :: tokens(:)
        type(compiler_arena_t) :: arena
        integer :: prog_index

        pipeline_ok = .false.
        call arena%init()

        ! Phase 1: Lexical Analysis
        call trace_enter('phase:lexer')
        call run_lexical_analysis(source, tokens, arena, error_msg)
        call trace_leave('phase:lexer')
        if (error_msg /= "") then
            call handle_lexical_error(source, error_msg, output, arena)
            call arena%destroy()
            return
        end if

        ! Phase 1.5: Enhanced syntax validation with detailed reporting (Issue #256)
        call trace_enter('phase:syntax')
        call validate_syntax_with_reporting(source, tokens, error_msg, output, &
            & arena)
        call trace_leave('phase:syntax')
        if (error_msg /= "") then
            call arena%destroy()
            return
        end if

        ! Check for meaningful content
        if (not_meaningful_for_parsing(tokens)) then
            call emit_meaningless_content_output(has_wrapping_flag, output)
            call arena%destroy()
            return
        end if

        ! Phase 2: Parsing
        call trace_enter('phase:parser')
        call run_parsing_phase(tokens, arena, prog_index, error_msg, output)
        call trace_leave('phase:parser')
        if (error_msg /= "") then
            call arena%destroy()
            return
        end if

        ! Optional: Validate AST locations after parsing
        call validate_locations_if_enabled(arena%ast, 'post-parse')

        ! Phases 3-5: Semantic Analysis, Standardization, Code Generation
        call trace_enter('phase:final')
        call run_final_phases_with_format(arena, prog_index, output, error_msg, &
                                          apply_ast_wrapping, &
                                          local_operating_mode, apply_default_format)
        call trace_leave('phase:final')
        call arena%destroy()
        if (error_msg /= "") return

        pipeline_ok = .true.
    end subroutine run_arena_pipeline

    ! Resolve the optional transformation flags into concrete local settings.
    subroutine resolve_transformation_options(enable_ast_wrapping, operating_mode, &
                                              preserve_format_config, &
                                              apply_ast_wrapping, &
                                              local_operating_mode, &
                                              apply_default_format)
        logical, intent(in), optional :: enable_ast_wrapping
        integer, intent(in), optional :: operating_mode
        logical, intent(in), optional :: preserve_format_config
        logical, intent(out) :: apply_ast_wrapping
        integer, intent(out) :: local_operating_mode
        logical, intent(out) :: apply_default_format

        apply_ast_wrapping = .false.
        if (present(enable_ast_wrapping)) then
            apply_ast_wrapping = enable_ast_wrapping
        end if
        local_operating_mode = OPERATING_MODE_INFER
        if (present(operating_mode)) local_operating_mode = operating_mode
        apply_default_format = .not. present(enable_ast_wrapping)
        if (present(preserve_format_config)) then
            if (preserve_format_config) apply_default_format = .false.
        end if
    end subroutine resolve_transformation_options

    ! Handle empty/whitespace-only or binary input before arena allocation.
    ! Returns .true. when the caller should stop and return immediately.
    function handle_preparse_early_exit(source, has_wrapping_flag, output, &
                                        error_msg) result(handled)
        character(len=*), intent(in) :: source
        logical, intent(in) :: has_wrapping_flag
        character(len=:), allocatable, intent(inout) :: output
        character(len=:), allocatable, intent(inout) :: error_msg
        logical :: handled

        handled = .false.

        ! Handle empty or whitespace-only input
        if (is_empty_or_whitespace_only(source)) then
            if (has_wrapping_flag) then
                ! Standard Fortran mode (context-aware call): do not fabricate
                ! a trivial program wrapper for empty input. This keeps
                ! standard Fortran round-trip semantics strict.
                output = ""
            else
                ! Lazy Fortran / library API: preserve legacy behaviour and
                ! generate a minimal program for empty input.
                call create_minimal_program(output)
            end if
            handled = .true.
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
            handled = .true.
        end if
    end function handle_preparse_early_exit

    ! Produce output for token streams with no meaningful content to parse.
    subroutine emit_meaningless_content_output(has_wrapping_flag, output)
        logical, intent(in) :: has_wrapping_flag
        character(len=:), allocatable, intent(inout) :: output

        if (has_wrapping_flag) then
            ! Standard Fortran mode: inputs that tokenize to comments,
            ! whitespace or EOF should not gain a program wrapper.
            output = ""
        else
            call create_minimal_program(output)
        end if
    end subroutine emit_meaningless_content_output

    ! Run the final phases, optionally bracketed by default-format config save/restore.
    subroutine run_final_phases_with_format(arena, prog_index, output, error_msg, &
                                            apply_ast_wrapping, &
                                            local_operating_mode, apply_default_format)
        type(compiler_arena_t), intent(inout) :: arena
        integer, intent(inout) :: prog_index
        character(len=:), allocatable, intent(out) :: output
        character(len=:), allocatable, intent(inout) :: error_msg
        logical, intent(in) :: apply_ast_wrapping
        integer, intent(in) :: local_operating_mode
        logical, intent(in) :: apply_default_format
        integer :: saved_size, saved_line_length
        character(len=1) :: saved_char
        logical :: saved_standardize_types, saved_standardizer_types

        if (apply_default_format) then
            call save_current_configuration(saved_size, saved_char, &
                                            saved_line_length, &
                                            saved_standardize_types, &
                                            saved_standardizer_types)
            call apply_format_options(format_options_t())
        end if
        call run_final_phases(arena, prog_index, output, error_msg, &
                              apply_ast_wrapping, local_operating_mode)
        if (apply_default_format) then
            call restore_configuration(saved_size, saved_char, saved_line_length, &
                                       saved_standardize_types, &
                                       saved_standardizer_types)
        end if
    end subroutine run_final_phases_with_format

    ! Prepend a preserved contiguous leading comment block to the output.
    subroutine prepend_leading_comment_block(source, output)
        character(len=*), intent(in) :: source
        character(len=:), allocatable, intent(inout) :: output
        character(len=:), allocatable :: lead

        if (.not. has_leading_comment(source)) return
        lead = extract_leading_comment_block(source)
        if (.not. allocated(lead)) return
        if (len_trim(lead) <= 0) return
        if (len_trim(output) > 0) then
            output = trim(lead) // new_line('A') // trim(output)
        else
            output = trim(lead)
        end if
    end subroutine prepend_leading_comment_block
