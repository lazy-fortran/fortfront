module frontend_core
    ! fortfront - Core frontend API module
    ! Main entry points for compilation pipeline

    use iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, tokenize_core, TK_EOF, TK_KEYWORD, &
                           TK_COMMENT, TK_NEWLINE, TK_OPERATOR, TK_IDENTIFIER, &
                           TK_NUMBER, TK_STRING, TK_UNKNOWN
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_dispatcher_module, only: parse_statement_dispatcher, &
                                           get_additional_indices, clear_additional_indices
    use parser_control_flow_module, only: parse_do_loop, parse_do_while, &
                                          parse_select_case
    ! Migrated from ast_core: use explicit imports for better dependency management
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node
    use compiler_arena, only: compiler_arena_t, create_compiler_arena, destroy_compiler_arena
    use ast_nodes_misc, only: comment_node
    use ast_base, only: LITERAL_STRING
    use ast_factory, only: push_program, push_literal
    use semantic_analyzer, only: semantic_context_t, create_semantic_context, &
                                   analyze_program, has_semantic_errors
    use standardizer, only: standardize_ast, set_standardizer_type_standardization, &
                           get_standardizer_type_standardization
    use codegen_arena_interface, only: generate_code_from_arena
    use codegen_basic_utils, only: add_line_continuations
    use codegen_type_utils, only: set_type_standardization, get_type_standardization
    use codegen_core, only: generate_code_polymorphic, initialize_codegen
    use codegen_indent, only: set_indent_config, get_indent_config, &
                               set_line_length_config, get_line_length_config
    use input_validation, only: validate_basic_syntax, check_missing_then_statements, &
                                check_incomplete_statements, check_for_fortran_content, &
                                check_missing_end_constructs, contains_invalid_patterns, &
                                has_only_meaningless_tokens, format_enhanced_error, &
                                format_syntax_error, split_into_lines
    use path_validation, only: validate_input_path, validate_output_path, path_validation_result_t
    use frontend_parsing, only: parse_tokens, parse_tokens_safe, parse_result_with_index_t
    use frontend_utilities, only: write_output_file, int_to_str
    use slow_path_config, only: initialize_slow_path_from_env, set_slow_path_enabled, &
                                is_slow_path_enabled
    use slow_path_analyzers, only: clear_slow_path_results, &
                                   run_slow_path_analyzers

    implicit none
    private

    public :: lex_source, analyze_semantics, emit_fortran
    public :: compile_source, compilation_options_t
    public :: lex_file
    public :: parse_tokens_safe, parse_result_with_index_t

    ! Simplified compilation options - no backend selection
    type :: compilation_options_t
        logical :: debug_tokens = .false.
        logical :: debug_ast = .false.
        logical :: debug_semantic = .false.
        logical :: debug_standardize = .false.
        logical :: debug_codegen = .false.
        logical :: slow_path_override = .false.
        logical :: slow_path_enabled = .false.
        character(len=:), allocatable :: output_file
    contains
        procedure :: deep_copy => compilation_options_deep_copy
        procedure :: assign => compilation_options_assign
        generic :: assignment(=) => assign
    end type compilation_options_t

contains

    ! Main entry point - clean 4-phase compilation pipeline
    subroutine compile_source(input_file, options, error_msg)
        character(len=*), intent(in) :: input_file
        type(compilation_options_t), intent(in) :: options
        character(len=*), intent(out) :: error_msg

        ! Local variables
        type(token_t), allocatable, target :: tokens(:)
        type(compiler_arena_t) :: compiler_arena
        integer :: prog_index
        character(len=:), allocatable :: code, source
        integer :: unit, iostat
        type(path_validation_result_t) :: validation_result

        ! Log compilation start with proper logging
        write(error_unit, '(A)') "INFO [frontend_core]: Starting compilation of " // input_file

        error_msg = ""

        call initialize_slow_path_from_env()
        call clear_slow_path_results()
        if (options%slow_path_override) then
            call set_slow_path_enabled(options%slow_path_enabled)
        end if
        
        ! Validate input file path for security
        validation_result = validate_input_path(input_file)
        if (.not. validation_result%is_valid()) then
            error_msg = "Input path validation failed: " // validation_result%get_message()
            return
        end if

        ! Read source file
        call read_source_file(input_file, source, error_msg)
        if (error_msg /= "") return

        ! Initialize unified compiler arena for all phases
        compiler_arena = create_compiler_arena()

        ! Phase 1: Lexical Analysis
        call compiler_arena%next_phase("lexer")
        call lex_file(source, tokens, error_msg)
        if (error_msg /= "") then
            call destroy_compiler_arena(compiler_arena)
            return
        end if

        ! Phase 2: Parsing
        call compiler_arena%next_phase("parser")
        call parse_tokens(tokens, compiler_arena%ast, prog_index, error_msg)
        if (error_msg /= "") then
            call destroy_compiler_arena(compiler_arena)
            return
        end if

        ! Phase 3: Semantic Analysis (only for lazy fortran)
        call compiler_arena%next_phase("semantic")
        ! Use the version with INTENT checking
        call run_semantic_analysis(compiler_arena%ast, prog_index, error_msg)
        if (error_msg /= "") then
            call destroy_compiler_arena(compiler_arena)
            return
        end if

        ! Phase 4: Standardization (transform dialect to standard Fortran)
        call compiler_arena%next_phase("standardization")
        call standardize_ast(compiler_arena%ast, prog_index)

        ! Phase 5: Standard Fortran Code Generation
        call compiler_arena%next_phase("codegen")
        call initialize_codegen()
        code = generate_code_from_arena(compiler_arena%ast, prog_index)

        call write_compiled_output(options, code, error_msg)
        if (error_msg /= "") then
            call destroy_compiler_arena(compiler_arena)
            return
        end if

        call destroy_compiler_arena(compiler_arena)

    end subroutine compile_source

    ! JSON-based compile entry points moved to module `frontend_json`.

    ! Phase 1: Lexical Analysis
    subroutine lex_file(source, tokens, error_msg)
        character(len=*), intent(in) :: source
        type(token_t), allocatable, intent(out) :: tokens(:)
        character(len=*), intent(out) :: error_msg
        
        error_msg = ""
        call tokenize_core(source, tokens)
    end subroutine lex_file

    ! Simple interface functions for clean pipeline usage
    subroutine lex_source(source_code, tokens, error_msg)
        character(len=*), intent(in) :: source_code
        type(token_t), allocatable, intent(out) :: tokens(:)
        character(len=:), allocatable, intent(out) :: error_msg
        
        call tokenize_core(source_code, tokens)
        if (.not. allocated(tokens)) then
            allocate(character(len=22) :: error_msg)
            error_msg = "Failed to tokenize source"
        else
            allocate(character(len=0) :: error_msg)
            error_msg = ""
        end if
    end subroutine lex_source

    subroutine analyze_semantics(arena, prog_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: prog_index
        character(len=256) :: error_msg

        call run_semantic_analysis(arena, prog_index, error_msg)
        ! Note: For backward compatibility, this version doesn't propagate errors
    end subroutine analyze_semantics

    subroutine emit_fortran(arena, prog_index, fortran_code)
        type(ast_arena_t), intent(in) :: arena  ! Made intent(in) to prevent corruption
        integer, intent(in) :: prog_index  ! Made intent(in) to prevent modification
        character(len=:), allocatable, intent(out) :: fortran_code

        ! Initialize the codegen system
        call initialize_codegen()

        ! CRITICAL FIX: Do NOT call standardize_ast here - it causes double standardization
        ! and memory corruption when called in error paths. Standardization happens once
        ! in the main transform pipeline only.
        fortran_code = generate_code_from_arena(arena, prog_index)
    end subroutine emit_fortran

    ! Private helper subroutines to break down large functions

    subroutine read_source_file(input_file, source, error_msg)
        character(len=*), intent(in) :: input_file
        character(len=:), allocatable, intent(out) :: source
        character(len=*), intent(out) :: error_msg
        integer :: unit, iostat

        ! Read source file
        open (newunit=unit, file=input_file, status='old', action='read', iostat=iostat)
        if (iostat /= 0) then
            error_msg = "Cannot open input file: "//input_file
            return
        end if

        block
            character(len=:), allocatable :: line
            allocate (character(len=0) :: source)
            allocate (character(len=1000) :: line)  ! Allocatable - safe from stack overflow

            do
                read (unit, '(A)', iostat=iostat) line
                if (iostat /= 0) exit
                source = source//trim(line)//new_line('a')
            end do
        end block
        close (unit)
        error_msg = ""
    end subroutine read_source_file

    subroutine run_semantic_analysis(arena, prog_index, error_msg)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: prog_index
        character(len=*), intent(out) :: error_msg

        block
            type(semantic_context_t), allocatable :: ctx
            allocate(ctx)
            call create_semantic_context(ctx)
            
            ! Use permissive mode here; strictness is decided in semantic analyzer
            ! based on presence of 'implicit none' within the program unit.
            ctx%strict_mode = .false.
            
            call analyze_program(ctx, arena, prog_index)
            
            ! Check for semantic errors and provide detailed error messages
            call clear_slow_path_results()
            if (has_semantic_errors(ctx)) then
                error_msg = get_detailed_semantic_errors(ctx)
                return
            end if

            if (is_slow_path_enabled()) then
                call run_slow_path_analyzers(arena, prog_index)
            end if
        end block
        
        error_msg = ""
    end subroutine run_semantic_analysis

    ! Helper function to get detailed semantic error messages
    function get_detailed_semantic_errors(ctx) result(error_msg)
        type(semantic_context_t), intent(in) :: ctx
        character(len=:), allocatable :: error_msg
        integer :: i, total_errors
        character(len=128) :: temp_msg
        
        total_errors = ctx%errors%count
        if (total_errors == 0) then
            error_msg = "No semantic errors found"
            return
        end if
        
        ! Build comprehensive error message
        temp_msg = ""
        write(temp_msg, '(A,I0,A)') "Found ", total_errors, " semantic error(s):"
        error_msg = trim(temp_msg)
        
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
            write(temp_msg, '(A,I0,A)') "  ... and ", (total_errors - 3), " more error(s)"
            error_msg = error_msg // new_line('a') // trim(temp_msg)
        end if
    end function get_detailed_semantic_errors

    subroutine run_compilation_pipeline_from_phase2(tokens, compiler_arena, prog_index, &
                                                    error_msg)
        type(token_t), intent(in) :: tokens(:)
        type(compiler_arena_t), intent(inout) :: compiler_arena
        integer, intent(inout) :: prog_index
        character(len=*), intent(out) :: error_msg

        ! Phase 2: Parsing
        call compiler_arena%next_phase("parser")
        call parse_tokens(tokens, compiler_arena%ast, prog_index, error_msg)
        if (error_msg /= "") return

        ! Phase 3: Semantic Analysis (only for lazy fortran)
        call compiler_arena%next_phase("semantic")
        call run_semantic_analysis(compiler_arena%ast, prog_index, error_msg)
        if (error_msg /= "") return

        ! Phase 4: Standardization (transform dialect to standard Fortran)
        call compiler_arena%next_phase("standardization")
        call standardize_ast(compiler_arena%ast, prog_index)
    end subroutine run_compilation_pipeline_from_phase2

    subroutine run_compilation_pipeline_from_phase3(compiler_arena, prog_index)
        type(compiler_arena_t), intent(inout) :: compiler_arena
        integer, intent(inout) :: prog_index
        character(len=256) :: error_msg

        ! Phase 3: Semantic Analysis
        call compiler_arena%next_phase("semantic")
        call run_semantic_analysis(compiler_arena%ast, prog_index, error_msg)
        ! Note: For internal use, we continue even if semantic errors occur
        ! The calling routine should check for errors separately

        ! Phase 4: Standardization
        call compiler_arena%next_phase("standardization")
        call standardize_ast(compiler_arena%ast, prog_index)
    end subroutine run_compilation_pipeline_from_phase3

    subroutine write_compiled_output(options, code, error_msg)
        type(compilation_options_t), intent(in) :: options
        character(len=*), intent(in) :: code
        character(len=*), intent(out) :: error_msg

        if (allocated(options%output_file) .and. len_trim(options%output_file) > 0) then
            call write_output_file(options%output_file, code, error_msg)
        else
            ! Write to stdout - commented out as fortfront.f90 handles printing
            ! print '(a)', code
            error_msg = ""
        end if
    end subroutine write_compiled_output

    ! Phase 4: Code Generation
    subroutine generate_fortran_code(arena, prog_index, code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: prog_index
        character(len=:), allocatable, intent(out) :: code

        ! CRITICAL FIX: Initialize codegen system before generating code
        call initialize_codegen()
        code = generate_code_from_arena(arena, prog_index)
    end subroutine generate_fortran_code

    ! Deep copy procedures for compilation_options_t
    function compilation_options_deep_copy(this) result(copy)
        class(compilation_options_t), intent(in) :: this
        type(compilation_options_t) :: copy

        copy%debug_tokens = this%debug_tokens
        copy%debug_ast = this%debug_ast
        copy%debug_semantic = this%debug_semantic
        copy%debug_standardize = this%debug_standardize
        copy%debug_codegen = this%debug_codegen
        copy%slow_path_override = this%slow_path_override
        copy%slow_path_enabled = this%slow_path_enabled

        if (allocated(this%output_file)) then
            copy%output_file = this%output_file
        end if
    end function compilation_options_deep_copy

    subroutine compilation_options_assign(lhs, rhs)
        class(compilation_options_t), intent(out) :: lhs
        type(compilation_options_t), intent(in) :: rhs

        lhs%debug_tokens = rhs%debug_tokens
        lhs%debug_ast = rhs%debug_ast
        lhs%debug_semantic = rhs%debug_semantic
        lhs%debug_standardize = rhs%debug_standardize
        lhs%debug_codegen = rhs%debug_codegen
        lhs%slow_path_override = rhs%slow_path_override
        lhs%slow_path_enabled = rhs%slow_path_enabled

        if (allocated(rhs%output_file)) then
            lhs%output_file = rhs%output_file
        end if
    end subroutine compilation_options_assign

end module frontend_core
