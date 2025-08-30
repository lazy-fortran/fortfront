module frontend_core
    ! fortfront - Core frontend API module
    ! Main entry points for compilation pipeline

    use lexer_core, only: token_t, tokenize_core, TK_EOF, TK_KEYWORD, &
                           TK_COMMENT, TK_NEWLINE, TK_OPERATOR, TK_IDENTIFIER, &
                           TK_NUMBER, TK_STRING, TK_UNKNOWN
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_core, only: parse_expression, parse_function_definition
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
    use codegen_type_utils, only: set_type_standardization, get_type_standardization
    use codegen_core, only: generate_code_polymorphic, initialize_codegen
    use codegen_indent, only: set_indent_config, get_indent_config, &
                               set_line_length_config, get_line_length_config
    use json_reader, only: json_read_tokens_from_file, json_read_ast_from_file, &
                            json_read_semantic_from_file
    use input_validation, only: validate_basic_syntax, check_missing_then_statements, &
                                check_incomplete_statements, check_for_fortran_content, &
                                check_missing_end_constructs, contains_invalid_patterns, &
                                has_only_meaningless_tokens, format_enhanced_error, &
                                format_syntax_error, split_into_lines
    use path_validation, only: validate_input_path, validate_output_path, path_validation_result_t
    use frontend_parsing, only: parse_tokens, parse_tokens_safe, parse_result_with_index_t
    use frontend_utilities, only: write_output_file, int_to_str

    implicit none
    private

    public :: lex_source, analyze_semantics, emit_fortran
    public :: compile_source, compilation_options_t
    public :: compile_from_tokens_json, compile_from_ast_json, &
              compile_from_semantic_json
    public :: lex_file
    public :: parse_tokens_safe, parse_result_with_index_t

    ! Simplified compilation options - no backend selection
    type :: compilation_options_t
        logical :: debug_tokens = .false.
        logical :: debug_ast = .false.
        logical :: debug_semantic = .false.
        logical :: debug_standardize = .false.
        logical :: debug_codegen = .false.
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
        type(token_t), allocatable :: tokens(:)
        type(compiler_arena_t) :: compiler_arena
        integer :: prog_index
        character(len=:), allocatable :: code, source
        integer :: unit, iostat
        type(path_validation_result_t) :: validation_result

        ! Log compilation start - TODO: add proper logging
        ! Debug: compile_source called with input_file

        error_msg = ""
        
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
        if (error_msg /= "") return
        ! Debug tokens output disabled - implement later if needed

        ! Phase 2: Parsing
        call compiler_arena%next_phase("parser")
        call parse_tokens(tokens, compiler_arena%ast, prog_index, error_msg)
        if (error_msg /= "") return
        ! Debug AST output disabled - implement later if needed

        ! Phase 3: Semantic Analysis (only for lazy fortran)
        call compiler_arena%next_phase("semantic")
        ! Use the version with INTENT checking
        call run_semantic_analysis(compiler_arena%ast, prog_index, error_msg)
        if (error_msg /= "") return
        ! Debug semantic output disabled - implement later if needed

        ! Phase 4: Standardization (transform dialect to standard Fortran)
        call compiler_arena%next_phase("standardization")
        call standardize_ast(compiler_arena%ast, prog_index)
        ! Debug standardize output disabled - implement later if needed

        ! Phase 5: Standard Fortran Code Generation
        call compiler_arena%next_phase("codegen")
        code = generate_code_from_arena(compiler_arena%ast, prog_index)

        ! Debug codegen output disabled - implement later if needed

        ! Write output
        call write_compiled_output(options, code, error_msg)

        ! Cleanup unified compiler arena
        call destroy_compiler_arena(compiler_arena)

    end subroutine compile_source

    ! Compile from tokens JSON (skip phase 1)
    subroutine compile_from_tokens_json(tokens_json_file, options, error_msg)
        character(len=*), intent(in) :: tokens_json_file
        type(compilation_options_t), intent(in) :: options
        character(len=*), intent(out) :: error_msg

        type(token_t), allocatable :: tokens(:)
        type(compiler_arena_t) :: compiler_arena
        integer :: prog_index
        character(len=:), allocatable :: code
        type(path_validation_result_t) :: validation_result

        error_msg = ""
        
        ! Validate tokens JSON file path for security
        validation_result = validate_input_path(tokens_json_file)
        if (.not. validation_result%is_valid()) then
            error_msg = "Tokens JSON path validation failed: " // validation_result%get_message()
            return
        end if

        ! Initialize unified compiler arena 
        compiler_arena = create_compiler_arena()

        ! Read tokens from JSON
        tokens = json_read_tokens_from_file(tokens_json_file)

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

        ! Phase 5: Code Generation
        call compiler_arena%next_phase("codegen")
        call generate_fortran_code(compiler_arena%ast, prog_index, code)

        ! Write output (only if not in compile mode - backend handles file creation)
        call write_compiled_output(options, code, error_msg)

        ! Cleanup unified compiler arena
        call destroy_compiler_arena(compiler_arena)

    end subroutine compile_from_tokens_json

    ! Compile from AST JSON (skip phases 1-2)
    subroutine compile_from_ast_json(ast_json_file, options, error_msg)
        character(len=*), intent(in) :: ast_json_file
        type(compilation_options_t), intent(in) :: options
        character(len=*), intent(out) :: error_msg

        type(compiler_arena_t) :: compiler_arena
        integer :: prog_index
        character(len=:), allocatable :: code
        type(path_validation_result_t) :: validation_result

        error_msg = ""
        
        ! Validate AST JSON file path for security
        validation_result = validate_input_path(ast_json_file)
        if (.not. validation_result%is_valid()) then
            error_msg = "AST JSON path validation failed: " // validation_result%get_message()
            return
        end if

        ! Initialize unified compiler arena
        compiler_arena = create_compiler_arena()

        ! Read AST from JSON - simplified for now
        prog_index = push_literal(compiler_arena%ast, "! JSON loading not implemented", LITERAL_STRING, 1, 1)

        ! Phase 3: Semantic Analysis
        call compiler_arena%next_phase("semantic")
        call run_semantic_analysis(compiler_arena%ast, prog_index, error_msg)
        if (error_msg /= "") return

        ! Phase 4: Standardization
        call compiler_arena%next_phase("standardization")
        call standardize_ast(compiler_arena%ast, prog_index)

        ! Phase 5: Code Generation
        call compiler_arena%next_phase("codegen")
        call generate_fortran_code(compiler_arena%ast, prog_index, code)

        ! Write output (only if not in compile mode - backend handles file creation)
        call write_compiled_output(options, code, error_msg)

        ! Cleanup unified compiler arena
        call destroy_compiler_arena(compiler_arena)

    end subroutine compile_from_ast_json

    ! Compile from semantic JSON (skip phases 1-3) - ANNOTATED AST TO CODEGEN
    subroutine compile_from_semantic_json(semantic_json_file, options, error_msg)
        character(len=*), intent(in) :: semantic_json_file
        type(compilation_options_t), intent(in) :: options
        character(len=*), intent(out) :: error_msg

        type(compiler_arena_t) :: compiler_arena
        integer :: prog_index
        character(len=:), allocatable :: code
        type(path_validation_result_t) :: validation_result

        error_msg = ""
        
        ! Validate semantic JSON file path for security
        validation_result = validate_input_path(semantic_json_file)
        if (.not. validation_result%is_valid()) then
            error_msg = "Semantic JSON path validation failed: " // validation_result%get_message()
            return
        end if

        ! Initialize unified compiler arena
        compiler_arena = create_compiler_arena()

        ! Read annotated AST and semantic context from JSON - simplified
        prog_index = push_literal(compiler_arena%ast, "! Semantic JSON loading not implemented", &
                                 LITERAL_STRING, 1, 1)

        ! Phase 4: Code Generation (direct from annotated AST)
        call compiler_arena%next_phase("codegen")
        call generate_fortran_code(compiler_arena%ast, prog_index, code)

        ! Write output (only if not in compile mode - backend handles file creation)
        call write_compiled_output(options, code, error_msg)

        ! Cleanup unified compiler arena
        call destroy_compiler_arena(compiler_arena)

    end subroutine compile_from_semantic_json

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
            type(semantic_context_t) :: ctx
            ctx = create_semantic_context()
            
            ! CRITICAL FIX: Enable strict mode to detect undefined variables
            ! This addresses Linux CI failure in Issue #495 test
            ctx%strict_mode = .true.
            
            call analyze_program(ctx, arena, prog_index)
            
            ! Check for semantic errors and return error message if found
            if (has_semantic_errors(ctx)) then
                error_msg = "Semantic errors detected: undefined variables or type mismatches"
                return
            end if
        end block
        
        error_msg = ""
    end subroutine run_semantic_analysis

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
            ! Write to stdout
            print '(a)', code
            error_msg = ""
        end if
    end subroutine write_compiled_output

    ! Phase 4: Code Generation
    subroutine generate_fortran_code(arena, prog_index, code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: prog_index
        character(len=:), allocatable, intent(out) :: code

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

        if (allocated(rhs%output_file)) then
            lhs%output_file = rhs%output_file
        end if
    end subroutine compilation_options_assign

end module frontend_core