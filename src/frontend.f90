module frontend
    ! fortfront - Core analysis frontend
    ! Simple, clean interface: Lexer → Parser → Semantic → Standard Fortran codegen

    use lexer_core, only: token_t, tokenize_core, TK_EOF, TK_KEYWORD, &
                           TK_COMMENT, TK_NEWLINE, TK_OPERATOR, TK_IDENTIFIER, &
                           TK_NUMBER, TK_STRING, TK_UNKNOWN
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_core, only: parse_expression, parse_function_definition
    use parser_dispatcher_module, only: parse_statement_dispatcher
    use parser_control_flow_module, only: parse_do_loop, parse_do_while, &
                                          parse_select_case
    ! Migrated from ast_core: use explicit imports for better dependency management
    use ast_arena, only: ast_arena_t, init_ast_arena
    use ast_nodes_core, only: program_node
    use ast_nodes_misc, only: comment_node
    use ast_base, only: LITERAL_STRING
    use ast_factory, only: push_program, push_literal
    use semantic_analyzer, only: semantic_context_t, create_semantic_context, &
                                   analyze_program
    use semantic_analyzer_with_checks, only: analyze_program_with_checks
    use semantic_pipeline_integration, only: analyze_semantics_with_pipeline
    use standardizer, only: standardize_ast, standardize_ast_with_semantics, &
                           set_standardizer_type_standardization, &
                           get_standardizer_type_standardization
    use codegen_core, only: generate_code_from_arena, generate_code_polymorphic, &
                           set_type_standardization, get_type_standardization
    use codegen_indent, only: set_indent_config, get_indent_config, &
                               set_line_length_config, get_line_length_config
    use json_reader, only: json_read_tokens_from_file, json_read_ast_from_file, &
                            json_read_semantic_from_file
    use stdlib_logger, only: global_logger
    use input_validation, only: validate_basic_syntax, check_missing_then_statements, &
                                check_incomplete_statements, check_for_fortran_content, &
                                check_missing_end_constructs, contains_invalid_patterns, &
                                has_only_meaningless_tokens, format_enhanced_error, &
                                format_syntax_error, split_into_lines
    use path_validation, only: validate_input_path, validate_output_path, path_validation_result_t

    implicit none
    private

    public :: lex_source, parse_tokens, analyze_semantics, emit_fortran
    public :: compile_source, compilation_options_t
    public :: compile_from_tokens_json, compile_from_ast_json, &
              compile_from_semantic_json
    public :: transform_lazy_fortran_string, &
              transform_lazy_fortran_string_with_format, format_options_t
    ! Debug functions for unit testing
    public :: find_program_unit_boundary, is_function_start, is_end_function, &
              parse_program_unit
    public :: is_do_loop_start, is_do_while_start, is_select_case_start, &
              is_end_do, is_end_select
    public :: is_if_then_start, is_end_if
    public :: lex_file

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

    ! Formatting options for code generation
    type :: format_options_t
        integer :: indent_size = 4
        logical :: use_tabs = .false.
        character(len=1) :: indent_char = ' '
        logical :: standardize_types = .true.  ! Whether to standardize type kinds
        integer :: line_length = 130  ! Maximum line length before adding continuations
    end type format_options_t

contains

    ! Create a container for multiple top-level program units
    function create_multi_unit_container(arena, unit_indices) result(container_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: unit_indices(:)
        integer :: container_index

        ! For now, use a program node with a special flag to indicate multiple units
        ! The code generator will handle this specially
        container_index = push_program(arena, "__MULTI_UNIT__", unit_indices, 1, 1)
    end function create_multi_unit_container

    ! Main entry point - clean 4-phase compilation pipeline
    subroutine compile_source(input_file, options, error_msg)
        character(len=*), intent(in) :: input_file
        type(compilation_options_t), intent(in) :: options
        character(len=*), intent(out) :: error_msg

        ! Local variables
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        character(len=:), allocatable :: code, source
        integer :: unit, iostat
        type(path_validation_result_t) :: validation_result

        ! Log compilation start
        call global_logger%log_debug("compile_source called with: "//trim(input_file))

        error_msg = ""
        
        ! Validate input file path for security
        validation_result = validate_input_path(input_file)
        if (.not. validation_result%is_valid()) then
            error_msg = "Input path validation failed: " // validation_result%get_message()
            return
        end if

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

        ! Phase 1: Lexical Analysis
        call lex_file(source, tokens, error_msg)
        if (error_msg /= "") return
        ! Debug tokens output disabled - implement later if needed

        ! Phase 2: Parsing
        call init_ast_arena(arena)
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (error_msg /= "") return
        ! Debug AST output disabled - implement later if needed

        ! Phase 3: Semantic Analysis with Variable Declaration Generation
        ! Use the new semantic pipeline with variable declaration analyzer  
        block
            type(semantic_context_t) :: semantic_ctx
            call analyze_semantics_with_pipeline(arena, prog_index, semantic_ctx)
            ! Debug semantic output disabled - implement later if needed

            ! Phase 4: Standardization (transform dialect to standard Fortran)
            ! Use enhanced standardizer with semantic context for precise type inference
            call standardize_ast_with_semantics(arena, prog_index, semantic_ctx)
        end block
        ! Debug standardize output disabled - implement later if needed

        ! Phase 5: Standard Fortran Code Generation
        code = generate_code_from_arena(arena, prog_index)

        ! Debug codegen output disabled - implement later if needed

        ! Write output
        if (allocated(options%output_file) .and. len_trim(options%output_file) > 0) then
            call write_output_file(options%output_file, code, error_msg)
        else
            ! Write to stdout
            print '(a)', code
        end if

    end subroutine compile_source

    ! Compile from tokens JSON (skip phase 1)
    subroutine compile_from_tokens_json(tokens_json_file, options, error_msg)
        character(len=*), intent(in) :: tokens_json_file
        type(compilation_options_t), intent(in) :: options
        character(len=*), intent(out) :: error_msg

        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
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

        ! Read tokens from JSON
        tokens = json_read_tokens_from_file(tokens_json_file)

        ! Phase 2: Parsing
        call init_ast_arena(arena)
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (error_msg /= "") return

        ! Phase 3: Semantic Analysis with Variable Declaration Generation
        ! Use the new semantic pipeline with variable declaration analyzer
        block
            type(semantic_context_t) :: semantic_ctx
            call analyze_semantics_with_pipeline(arena, prog_index, semantic_ctx)

            ! Phase 4: Standardization (transform dialect to standard Fortran)
            call standardize_ast_with_semantics(arena, prog_index, semantic_ctx)
        end block

        ! Phase 5: Code Generation
        call generate_fortran_code(arena, prog_index, code)

        ! Write output (only if not in compile mode - backend handles file creation)
        if (allocated(options%output_file) .and. allocated(options%output_file)) then
            call write_output_file(options%output_file, code, error_msg)
        end if

    end subroutine compile_from_tokens_json

    ! Compile from AST JSON (skip phases 1-2)
    subroutine compile_from_ast_json(ast_json_file, options, error_msg)
        character(len=*), intent(in) :: ast_json_file
        type(compilation_options_t), intent(in) :: options
        character(len=*), intent(out) :: error_msg

        type(ast_arena_t) :: arena
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

        ! Read AST from JSON - simplified for now
        call init_ast_arena(arena)
prog_index = push_literal(arena, "! JSON loading not implemented", LITERAL_STRING, 1, 1)

        ! Phase 3: Semantic Analysis
        ! Note: This path doesn't use INTENT checking since it's for testing
        block
            type(semantic_context_t) :: sem_ctx
            sem_ctx = create_semantic_context()
            call analyze_program(sem_ctx, arena, prog_index)

            ! Phase 4: Standardization
            call standardize_ast_with_semantics(arena, prog_index, sem_ctx)
        end block

        ! Phase 5: Code Generation
        call generate_fortran_code(arena, prog_index, code)

        ! Write output (only if not in compile mode - backend handles file creation)
        if (allocated(options%output_file) .and. allocated(options%output_file)) then
            call write_output_file(options%output_file, code, error_msg)
        end if

    end subroutine compile_from_ast_json

    ! Compile from semantic JSON (skip phases 1-3) - ANNOTATED AST TO CODEGEN
    subroutine compile_from_semantic_json(semantic_json_file, options, error_msg)
        character(len=*), intent(in) :: semantic_json_file
        type(compilation_options_t), intent(in) :: options
        character(len=*), intent(out) :: error_msg

        type(ast_arena_t) :: arena
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

        ! Read annotated AST and semantic context from JSON - simplified
        call init_ast_arena(arena)
        prog_index = push_literal(arena, "! Semantic JSON loading not implemented", &
                                 LITERAL_STRING, 1, 1)

        ! Phase 4: Code Generation (direct from annotated AST)
        call generate_fortran_code(arena, prog_index, code)

        ! Write output (only if not in compile mode - backend handles file creation)
        if (allocated(options%output_file) .and. allocated(options%output_file)) then
            call write_output_file(options%output_file, code, error_msg)
        end if

    end subroutine compile_from_semantic_json

    ! Phase 1: Lexical Analysis
    subroutine lex_file(source, tokens, error_msg)
        character(len=*), intent(in) :: source
        type(token_t), allocatable, intent(out) :: tokens(:)
        character(len=*), intent(out) :: error_msg

        error_msg = ""
        call tokenize_core(source, tokens)
    end subroutine lex_file

    ! Phase 2: Parsing
    subroutine parse_tokens(tokens, arena, prog_index, error_msg)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: prog_index
        character(len=*), intent(out) :: error_msg

        ! Local variables for arena-based parsing
        integer, allocatable :: body_indices(:)
        integer :: stmt_index
        integer :: i, unit_start, unit_end, stmt_count
        type(token_t), allocatable :: unit_tokens(:)
        logical :: has_explicit_program_unit

        error_msg = ""
        stmt_count = 0
        allocate (body_indices(0))
        has_explicit_program_unit = .false.

        ! Check if file starts with explicit 'program', 'module', &
        ! 'function', or 'subroutine' statement
        do i = 1, size(tokens)
            if (tokens(i)%kind == TK_KEYWORD) then
                if (tokens(i)%text == "program" .or. tokens(i)%text == "module") then
                    has_explicit_program_unit = .true.
                    exit  ! Found explicit program unit
         else if (tokens(i)%text == "function" .or. tokens(i)%text == "subroutine") then
                    ! Check if it's a function/subroutine definition (not a call)
                    if (i == 1) then
                        has_explicit_program_unit = .true.
                        exit
                    else if (i > 1 .and. tokens(i - 1)%line < tokens(i)%line) then
                        ! At start of file or start of new line
                        has_explicit_program_unit = .true.
                        exit
                    else if (i > 1 .and. tokens(i - 1)%kind == TK_KEYWORD .and. &
               (tokens(i - 1)%text == "real" .or. tokens(i - 1)%text == "integer" .or. &
           tokens(i - 1)%text == "logical" .or. tokens(i - 1)%text == "character")) then
                        ! Type prefixed function/subroutine
                        has_explicit_program_unit = .true.
                        exit
                    end if
                else
                    ! Found other keyword, not a program unit
                    exit
                end if
            else if (tokens(i)%kind /= TK_EOF) then
                exit  ! Stop at first non-EOF token
            end if
        end do

        ! Parse program units, not individual lines
        i = 1
        do while (i <= size(tokens))
            if (tokens(i)%kind == TK_EOF) exit

            ! Skip empty lines (just EOF tokens)
            if (i < size(tokens) .and. tokens(i)%kind == TK_EOF) then
                i = i + 1
                cycle
            end if

            ! Find program unit boundary
            call find_program_unit_boundary(tokens, i, unit_start, unit_end, &
                                           has_explicit_program_unit)

            block
                character(len=20) :: start_str, end_str
                write (start_str, '(I0)') unit_start
                write (end_str, '(I0)') unit_end
                ! call log_verbose("parsing", "Found program unit from token "// &
                !                 trim(start_str)//" to "//trim(end_str))
            end block

            ! Check if this unit has any meaningful content
            block
                logical :: unit_has_content
                integer :: j
                unit_has_content = .false.
                do j = unit_start, unit_end
                    if (tokens(j)%kind /= TK_EOF .and. &
                        tokens(j)%kind /= TK_NEWLINE) then
                        unit_has_content = .true.
                        exit
                    end if
                end do

                ! Skip units without any meaningful content (only EOF/newlines)
                if (.not. unit_has_content) then
                    i = unit_end + 1
                    cycle
                end if
            end block

            ! Skip empty units, units with just EOF, or single-token keywords
            ! that are part of larger constructs
            if (unit_end >= unit_start .and. &
          .not. (unit_end == unit_start .and. tokens(unit_start)%kind == TK_EOF) .and. &
       .not. (unit_end == unit_start .and. tokens(unit_start)%kind == TK_KEYWORD .and. &
     (tokens(unit_start)%text == "real" .or. tokens(unit_start)%text == "integer" .or. &
 tokens(unit_start)%text == "logical" .or. tokens(unit_start)%text == "character" .or. &
                            tokens(unit_start)%text == "function" .or. &
                            tokens(unit_start)%text == "subroutine" .or. &
        tokens(unit_start)%text == "module" .or. tokens(unit_start)%text == "end" .or. &
      tokens(unit_start)%text == "else" .or. tokens(unit_start)%text == "elseif"))) then
                ! Extract unit tokens and add EOF
                allocate (unit_tokens(unit_end - unit_start + 2))
                unit_tokens(1:unit_end - unit_start + 1) = tokens(unit_start:unit_end)
                ! Add EOF token
                unit_tokens(unit_end - unit_start + 2)%kind = TK_EOF
                unit_tokens(unit_end - unit_start + 2)%text = ""
                unit_tokens(unit_end - unit_start + 2)%line = tokens(unit_end)%line
             unit_tokens(unit_end - unit_start + 2)%column = tokens(unit_end)%column + 1

                ! Debug: Extracted tokens for do construct

                    ! call log_verbose("parsing", "Extracted " // &
                    !      trim(adjustl(int_to_str(size(unit_tokens)))) // &
                    !      " tokens for unit")

                ! Parse the program unit
                stmt_index = parse_program_unit(unit_tokens, arena, &
                                               has_explicit_program_unit)

                if (stmt_index > 0) then
                    ! Add to body indices
                    body_indices = [body_indices, stmt_index]
                    stmt_count = stmt_count + 1
                end if

                deallocate (unit_tokens)
            end if

            i = unit_end + 1
            ! call log_verbose("parsing", "Next iteration will start at token "// &
            !                  trim(adjustl(int_to_str(i))))

            ! For lazy fortran without explicit program units,
            ! parse_all_statements has already processed everything
            if (.not. has_explicit_program_unit .and. unit_end >= size(tokens) - 1) then
                exit  ! We've processed all tokens
            end if
        end do

        ! Create program node with collected body indices
        ! Only wrap in implicit program if there's no explicit program unit
        ! (program/module/function/subroutine)
        if (.not. has_explicit_program_unit) then
            ! For lazy fortran, parse_all_statements already created the program node
            if (stmt_count > 0) then
                prog_index = body_indices(1)  ! This is the program node from &
                                             ! parse_all_statements
            else
                error_msg = "No statements found in file"
                prog_index = 0
            end if
        else if (stmt_count > 0) then
            ! Handle multiple top-level program units (modules, programs, etc.)
            
            ! Filter out empty implicit main programs that might have been created
            ! due to Issue #321 mixed construct handling
            block
                integer, allocatable :: filtered_indices(:)
                integer :: i, filtered_count
                
                allocate(filtered_indices(0))
                filtered_count = 0
                
                do i = 1, size(body_indices)
                    if (body_indices(i) > 0 .and. body_indices(i) <= arena%size) then
                        ! Check if this is an empty main program
                        select type (node => arena%entries(body_indices(i))%node)
                        type is (program_node)
                            if (node%name == "main") then
                                ! Check if program has no meaningful content
                                if (.not. allocated(node%body_indices) .or. &
                                    size(node%body_indices) == 0) then
                                    ! Skip empty main program
                                    cycle
                                else
                                    ! Check if all statements are meaningless (just EOF/whitespace)
                                    block
                                        integer :: j, meaningful_stmts
                                        logical :: has_meaningful_content
                                        meaningful_stmts = 0
                                        has_meaningful_content = .false.
                                        
                                        do j = 1, size(node%body_indices)
                                            if (node%body_indices(j) > 0 .and. &
                                                node%body_indices(j) <= arena%size) then
                                                select type (stmt_node => arena%entries(node%body_indices(j))%node)
                                                type is (comment_node)
                                                    ! Comments don't count
                                                class default
                                                    ! Any non-comment node counts as meaningful
                                                    has_meaningful_content = .true.
                                                    exit
                                                end select
                                            end if
                                        end do
                                        
                                        if (.not. has_meaningful_content) then
                                            ! Skip empty main program
                                            cycle
                                        end if
                                    end block
                                end if
                            end if
                        end select
                        
                        ! Add this unit to filtered list
                        filtered_indices = [filtered_indices, body_indices(i)]
                        filtered_count = filtered_count + 1
                    end if
                end do
                
                ! Use filtered indices
                if (filtered_count == 1) then
                    ! Single program unit - use it directly
                    prog_index = filtered_indices(1)
                else if (filtered_count > 1) then
                    ! Multiple program units - create a special multi-unit container
                    prog_index = create_multi_unit_container(arena, filtered_indices)
                else
                    ! No meaningful units left after filtering
                    error_msg = "No meaningful program units found after filtering"
                    prog_index = 0
                end if
            end block
        else
            ! No program unit found
            error_msg = "No program unit found in file"
            prog_index = 0
        end if
    end subroutine parse_tokens

    ! Find program unit boundary (function/subroutine/module spans multiple lines)
    subroutine find_program_unit_boundary(tokens, start_pos, unit_start, unit_end, &
                                         has_explicit_program)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        integer, intent(out) :: unit_start, unit_end
        logical, intent(in) :: has_explicit_program

        integer :: i, current_line, nesting_level
        logical :: in_function, in_subroutine, in_module, in_do_loop, &
                   in_select_case, in_if_block

        unit_start = start_pos
        unit_end = start_pos
        in_function = .false.
        in_subroutine = .false.
        in_module = .false.
        in_do_loop = .false.
        in_select_case = .false.
        in_if_block = .false.
        nesting_level = 0

        ! Check if starting token indicates a multi-line construct
        if (start_pos <= size(tokens)) then
            ! Look for function definition patterns
            if (is_function_start(tokens, start_pos)) then
                in_function = .true.
                nesting_level = 1
                ! call log_verbose("parsing", "Starting function at token "// &
                !                  trim(adjustl(int_to_str(start_pos))))
            else if (is_subroutine_start(tokens, start_pos)) then
                in_subroutine = .true.
                nesting_level = 1
            else if (is_module_start(tokens, start_pos)) then
                in_module = .true.
                nesting_level = 1
            else if (is_program_start(tokens, start_pos)) then
                ! Handle explicit program blocks
                in_module = .true.  ! Reuse module logic for program blocks
                nesting_level = 1
            else if (is_do_while_start(tokens, start_pos)) then
                in_do_loop = .true.
                nesting_level = 1
            else if (is_do_loop_start(tokens, start_pos)) then
                in_do_loop = .true.
                nesting_level = 1
                ! Boundary detection: found do loop at token
            else if (is_select_case_start(tokens, start_pos)) then
                in_select_case = .true.
                nesting_level = 1
            else if (is_if_then_start(tokens, start_pos)) then
                in_if_block = .true.
                nesting_level = 1
            end if
        end if

        ! If this is a multi-line construct, find the end
        if (in_function .or. in_subroutine .or. in_module .or. in_do_loop .or. &
            in_select_case .or. in_if_block) then
            i = start_pos
            do while (i <= size(tokens) .and. nesting_level > 0)
                if (tokens(i)%kind == TK_EOF) exit

                ! Check for nested constructs (but skip the first one at start_pos)
                if (i /= start_pos) then
                    if (in_function .and. is_function_start(tokens, i)) then
                        nesting_level = nesting_level + 1
                       ! call log_verbose("parsing", &
                       !     "Found nested function at token "// &
                       !      trim(adjustl(int_to_str(i)))//", nesting level now: "// &
                       !      trim(adjustl(int_to_str(nesting_level))))
                    else if (in_subroutine .and. is_subroutine_start(tokens, i)) then
                        nesting_level = nesting_level + 1
                    else if (in_module .and. is_module_start(tokens, i)) then
                        nesting_level = nesting_level + 1
                    else if (in_do_loop .and. (is_do_loop_start(tokens, i) .or. &
                                                is_do_while_start(tokens, i))) then
                        nesting_level = nesting_level + 1
                    else if (in_select_case .and. is_select_case_start(tokens, i)) then
                        nesting_level = nesting_level + 1
                    else if (in_if_block .and. is_if_then_start(tokens, i)) then
                        nesting_level = nesting_level + 1
                    end if
                end if

                ! Check for end constructs
                if (in_function .and. is_end_function(tokens, i)) then
                    nesting_level = nesting_level - 1
               ! call log_verbose("parsing", &
               !      "Found END FUNCTION, nesting level now: "// &
               !      trim(adjustl(int_to_str(nesting_level))))
                    unit_end = i + 1  ! Include both "end" and "function" tokens
                    i = i + 2  ! Skip both "end" and "function" tokens
                    ! Don't fall through to else block
                else if (in_subroutine .and. is_end_subroutine(tokens, i)) then
                    nesting_level = nesting_level - 1
                    unit_end = i + 1  ! Include both "end" and "subroutine" tokens
                    i = i + 2  ! Skip both "end" and "subroutine" tokens
                    ! Don't fall through to else block
                else if (in_module .and. is_end_module(tokens, i)) then
                    nesting_level = nesting_level - 1
                    unit_end = i + 1  ! Include both "end" and "module" tokens
                    i = i + 2  ! Skip both "end" and "module" tokens
                    ! Don't fall through to else block
                else if (in_do_loop .and. is_end_do(tokens, i)) then
                    nesting_level = nesting_level - 1
                    unit_end = i + 1  ! Include both "end" and "do" tokens
                    i = i + 2  ! Skip both "end" and "do" tokens
                    ! Don't fall through to else block
                else if (in_select_case .and. is_end_select(tokens, i)) then
                    nesting_level = nesting_level - 1
                    unit_end = i + 1  ! Include both "end" and "select" tokens
                    i = i + 2  ! Skip both "end" and "select" tokens
                    ! Don't fall through to else block
                else if (in_if_block .and. is_end_if(tokens, i)) then
                    nesting_level = nesting_level - 1
                    ! Check if it's "endif" (single token) or "end if" (two tokens)
                    if (tokens(i)%text == "endif") then
                        unit_end = i  ! Include just the "endif" token
                        i = i + 1  ! Skip one token
                    else
                        unit_end = i + 1  ! Include both "end" and "if" tokens
                        i = i + 2  ! Skip both tokens
                    end if
                    ! Don't fall through to else block
                else
                    unit_end = i
                    i = i + 1
                end if

                ! Stop when we've closed all nested constructs
                if (nesting_level == 0) exit
            end do
        else
            ! For lazy fortran without explicit program units,
            ! parse the entire remaining file as one unit
            if (.not. has_explicit_program) then
                ! Find end of all tokens (excluding final EOF)
                unit_end = size(tokens)
                do while (unit_end > start_pos .and. tokens(unit_end)%kind == TK_EOF)
                    unit_end = unit_end - 1
                end do
            else
                ! For mixed module/main program files, check if remaining tokens
                ! should be grouped as one implicit main program
                if (has_explicit_program .and. &
                    start_pos <= size(tokens) .and. &
                    .not. is_function_start(tokens, start_pos) .and. &
                    .not. is_subroutine_start(tokens, start_pos) .and. &
                    .not. is_module_start(tokens, start_pos) .and. &
                    .not. is_program_start(tokens, start_pos)) then
                    ! Check if there are any executable statements to group
                    block
                        logical :: has_meaningful_content
                        integer :: check_pos
                        has_meaningful_content = .false.
                        do check_pos = start_pos, size(tokens)
                            ! Skip EOF, newlines, comments
                            if (tokens(check_pos)%kind == TK_EOF .or. &
                                tokens(check_pos)%kind == TK_NEWLINE .or. &
                                tokens(check_pos)%kind == TK_COMMENT) then
                                cycle
                            end if
                            
                            ! Look for executable statement patterns that warrant main program wrapping
                            ! Keywords that typically start executable statements
                            if (tokens(check_pos)%kind == TK_KEYWORD) then
                                select case (trim(tokens(check_pos)%text))
                                case ("use", "call", "print", "write", "read", "stop", "end", &
                                      "if", "do", "select", "where", "forall", "goto", &
                                      "allocate", "deallocate", "assign", "pause", "return")
                                    has_meaningful_content = .true.
                                    exit
                                case default
                                    ! Variable declarations and other non-executable keywords
                                    ! should not trigger main program wrapping by themselves
                                end select
                            ! Identifier could be start of assignment or procedure call
                            else if (tokens(check_pos)%kind == TK_IDENTIFIER) then
                                has_meaningful_content = .true.
                                exit
                            ! Numbers, strings, operators in unexpected context
                            else if (tokens(check_pos)%kind == TK_NUMBER .or. &
                                     tokens(check_pos)%kind == TK_STRING .or. &
                                     tokens(check_pos)%kind == TK_OPERATOR) then
                                has_meaningful_content = .true.
                                exit
                            end if
                        end do
                        
                        if (has_meaningful_content) then
                            ! This looks like implicit main program statements after explicit units
                            ! Group all remaining non-explicit statements together
                            unit_end = size(tokens)
                            do while (unit_end > start_pos .and. tokens(unit_end)%kind == TK_EOF)
                                unit_end = unit_end - 1
                            end do
                        else
                            ! No meaningful executable content - skip this entirely
                            ! Set unit_end < start_pos to indicate no valid unit found
                            unit_end = start_pos - 1
                        end if
                    end block
                else
                    ! Single line construct - find end of current line
                    current_line = tokens(start_pos)%line
                    i = start_pos
                    do while (i <= size(tokens))
                        if (tokens(i)%line == current_line) then
                            unit_end = i
                            i = i + 1
                        else
                            exit
                        end if
                    end do
                end if
            end if

            ! Skip empty lines (single EOF token on its own line)
            if (unit_end == unit_start .and. tokens(unit_start)%kind == TK_EOF) then
                unit_end = unit_start - 1  ! Signal to skip this unit
            end if

            ! Skip single "real", "integer", etc. that are part of function definitions
            if (unit_end == unit_start .and. start_pos < size(tokens) .and. &
                tokens(start_pos)%kind == TK_KEYWORD .and. &
       (tokens(start_pos)%text == "real" .or. tokens(start_pos)%text == "integer" .or. &
   tokens(start_pos)%text == "logical" .or. tokens(start_pos)%text == "character")) then
                ! Check if next token is "function"
                if (start_pos + 1 <= size(tokens) .and. &
                    tokens(start_pos + 1)%kind == TK_KEYWORD .and. &
                    tokens(start_pos + 1)%text == "function") then
                    unit_end = unit_start - 1  ! Signal to skip this unit - &
                                              ! it's part of a function def
                end if
            end if
        end if
    end subroutine find_program_unit_boundary

    ! Parse a program unit (function, subroutine, module, or statements)
    function parse_program_unit(tokens, arena, has_explicit_program) result(unit_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        logical, intent(in) :: has_explicit_program
        integer :: unit_index
        type(parser_state_t) :: parser
        integer :: i
        logical :: has_content

        ! Note: Parsing program unit

        ! Check if tokens contain any real content (not just comments/EOF/newlines)
        has_content = .false.
        do i = 1, size(tokens)
            if (tokens(i)%kind /= TK_COMMENT .and. tokens(i)%kind /= TK_EOF .and. &
                tokens(i)%kind /= TK_NEWLINE) then
                has_content = .true.
                exit
            end if
        end do

        ! For lazy Fortran, comments-only input should still create a program unit
        ! Only skip if there are no tokens at all (completely empty)
        if (.not. has_content .and. size(tokens) <= 1) then
            unit_index = 0
            return
        end if

        parser = create_parser_state(tokens)

        ! Check what type of program unit this is
        if (is_function_start(tokens, 1)) then
            ! Multi-line function definition
            block
                type(parser_state_t) :: parser
                parser = create_parser_state(tokens)
                unit_index = parse_function_definition(parser, arena)
            end block
        else if (is_subroutine_start(tokens, 1)) then
            ! Subroutine definition - wrap in program like functions
            block
                type(parser_state_t) :: parser
                integer :: subroutine_index, prog_index
                parser = create_parser_state(tokens)
                subroutine_index = parse_statement_dispatcher(tokens, arena)
                
                ! Wrap standalone subroutine in program structure for standard compliance
                if (subroutine_index > 0) then
                    prog_index = push_program(arena, "main", [subroutine_index])
                    unit_index = prog_index
                else
                    unit_index = subroutine_index
                end if
            end block
        else if (is_module_start(tokens, 1)) then
            ! Module definition
            unit_index = parse_statement_dispatcher(tokens, arena)
        else if (is_program_start(tokens, 1)) then
            ! Program definition
            unit_index = parse_statement_dispatcher(tokens, arena)
        else
            ! For mixed module/main program files, we need to parse implicit main programs
            ! even when explicit program units exist elsewhere in the file.
            ! Check if we have any executable statements to parse as implicit main program
            block
                logical :: has_executable_content
                integer :: k
                has_executable_content = .false.
                do k = 1, size(tokens)
                    ! Skip EOF, newlines, comments
                    if (tokens(k)%kind == TK_EOF .or. &
                        tokens(k)%kind == TK_NEWLINE .or. &
                        tokens(k)%kind == TK_COMMENT) then
                        cycle
                    end if
                    
                    ! Look for executable statement patterns
                    if (tokens(k)%kind == TK_KEYWORD) then
                        select case (trim(tokens(k)%text))
                        case ("use", "call", "print", "write", "read", "stop", "end", &
                              "if", "do", "select", "where", "forall", "goto", &
                              "allocate", "deallocate", "assign", "pause", "return")
                            has_executable_content = .true.
                            exit
                        case default
                            ! Declaration keywords and other non-executable patterns
                            ! should not trigger main program creation by themselves
                        end select
                    ! Identifier could be start of assignment or procedure call
                    else if (tokens(k)%kind == TK_IDENTIFIER) then
                        has_executable_content = .true.
                        exit
                    ! Numbers, strings, operators in unexpected context
                    else if (tokens(k)%kind == TK_NUMBER .or. &
                             tokens(k)%kind == TK_STRING .or. &
                             tokens(k)%kind == TK_OPERATOR) then
                        has_executable_content = .true.
                        exit
                    end if
                end do

                if (has_executable_content) then
                    unit_index = parse_all_statements(tokens, arena)
                else
                    unit_index = 0
                end if
            end block
        end if
    end function parse_program_unit

    ! Parse all statements in a token array (for lazy fortran)
    function parse_all_statements(tokens, arena) result(prog_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer :: prog_index
        integer, allocatable :: body_indices(:)
        integer :: i, stmt_start, stmt_end, stmt_index
        type(token_t), allocatable :: stmt_tokens(:)

        allocate (body_indices(0))

        ! Parse each statement
        i = 1
        do while (i <= size(tokens))
            if (tokens(i)%kind == TK_EOF) then
                ! call log_verbose("parse_all", "Hit EOF token at position "// &
                !                  trim(adjustl(int_to_str(i))))
                exit
            end if

            ! Special handling for comments - treat as standalone statements
            if (tokens(i)%kind == TK_COMMENT) then
                ! Create a token array with just the comment and EOF
                allocate (stmt_tokens(2))
                stmt_tokens(1) = tokens(i)
                stmt_tokens(2)%kind = TK_EOF
                stmt_tokens(2)%text = ""
                stmt_tokens(2)%line = tokens(i)%line
                stmt_tokens(2)%column = tokens(i)%column + len(tokens(i)%text)

                ! Parse the comment
                stmt_index = parse_statement_dispatcher(stmt_tokens, arena)
                if (stmt_index > 0) then
                    body_indices = [body_indices, stmt_index]
                end if

                deallocate (stmt_tokens)
                i = i + 1
                cycle
            end if

            ! Find statement boundary
            call find_statement_boundary(tokens, i, stmt_start, stmt_end)

            if (stmt_end >= stmt_start) then
                ! Extract statement tokens
                allocate (stmt_tokens(stmt_end - stmt_start + 2))
                stmt_tokens(1:stmt_end - stmt_start + 1) = tokens(stmt_start:stmt_end)
                ! Add EOF token
                stmt_tokens(stmt_end - stmt_start + 2)%kind = TK_EOF
                stmt_tokens(stmt_end - stmt_start + 2)%text = ""
                stmt_tokens(stmt_end - stmt_start + 2)%line = tokens(stmt_end)%line
             stmt_tokens(stmt_end - stmt_start + 2)%column = tokens(stmt_end)%column + 1

                ! Parse the statement
                stmt_index = parse_statement_dispatcher(stmt_tokens, arena)
                if (stmt_index > 0) then
                    body_indices = [body_indices, stmt_index]
                end if

                deallocate (stmt_tokens)
            end if

            i = stmt_end + 1
            ! call log_verbose("parse_all", "Next statement starts at token "// &
            !                  trim(adjustl(int_to_str(i)))//" of "// &
            !                  trim(adjustl(int_to_str(size(tokens)))))

            ! Debug: Show next token if available
            if (i <= size(tokens)) then
                ! call log_verbose("parse_all", "Next token: '"//tokens(i)%text// &
                !      "' (kind="//trim(adjustl(int_to_str(tokens(i)%kind)))// &
                !      ", line="//trim(adjustl(int_to_str(tokens(i)%line)))//")")
            end if

            ! Check bounds
            if (i > size(tokens)) then
                ! call log_verbose("parse_all", "Reached end of tokens array")
                exit
            end if
        end do

        ! Create program node with all statements
        ! Only create program node if we have actual statements
        if (size(body_indices) > 0) then
            prog_index = push_program(arena, "main", body_indices, 1, 1)
        else
            prog_index = 0
        end if
    end function parse_all_statements

    ! Find statement boundary (handles multi-line constructs)
    subroutine find_statement_boundary(tokens, start_pos, stmt_start, stmt_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        integer, intent(out) :: stmt_start, stmt_end
        integer :: i, nesting_level
        logical :: in_if_block, in_do_loop, in_select_case, in_function

        stmt_start = start_pos
        stmt_end = start_pos
        in_if_block = .false.
        in_do_loop = .false.
        in_select_case = .false.
        in_function = .false.
        nesting_level = 0

        ! Check for multi-line constructs
        if (is_if_then_start(tokens, start_pos)) then
            in_if_block = .true.
            nesting_level = 1
        else if (is_do_loop_start(tokens, start_pos) .or. &
                 is_do_while_start(tokens, start_pos)) then
            in_do_loop = .true.
            nesting_level = 1
        else if (is_select_case_start(tokens, start_pos)) then
            in_select_case = .true.
            nesting_level = 1
        else if (is_function_start(tokens, start_pos)) then
            in_function = .true.
            nesting_level = 1
        end if

        if (nesting_level > 0) then
            ! Multi-line construct - find matching end
            i = start_pos
            do while (i <= size(tokens) .and. nesting_level > 0)
                if (tokens(i)%kind == TK_EOF) exit

                ! Check for nested constructs
                if (i /= start_pos) then
                    if (in_if_block .and. is_if_then_start(tokens, i)) then
                        nesting_level = nesting_level + 1
                    else if (in_do_loop .and. (is_do_loop_start(tokens, i) .or. &
                                                is_do_while_start(tokens, i))) then
                        nesting_level = nesting_level + 1
                    else if (in_select_case .and. is_select_case_start(tokens, i)) then
                        nesting_level = nesting_level + 1
                    else if (in_function .and. is_function_start(tokens, i)) then
                        nesting_level = nesting_level + 1
                    end if
                end if

                ! Check for end constructs
                if (in_if_block .and. is_end_if(tokens, i)) then
                    nesting_level = nesting_level - 1
                    if (nesting_level == 0) then
                        ! Check if it's "end if" (two tokens) or "endif" (one token)
                        if (i + 1 <= size(tokens) .and. tokens(i)%text == "end" .and. &
                 tokens(i + 1)%kind == TK_KEYWORD .and. tokens(i + 1)%text == "if") then
                            stmt_end = i + 1  ! Include both "end" and "if"
                        else
                            stmt_end = i  ! Just "endif"
                        end if
                        exit
                    end if
                else if (in_do_loop .and. is_end_do(tokens, i)) then
                    nesting_level = nesting_level - 1
                    if (nesting_level == 0) then
                        ! "end do" is always two tokens
                        stmt_end = i + 1  ! Include both "end" and "do"
                        exit
                    end if
                else if (in_select_case .and. is_end_select(tokens, i)) then
                    nesting_level = nesting_level - 1
                    if (nesting_level == 0) then
                        ! "end select" is always two tokens
                        stmt_end = i + 1  ! Include both "end" and "select"
                        exit
                    end if
                else if (in_function .and. is_end_function(tokens, i)) then
                    nesting_level = nesting_level - 1
                    if (nesting_level == 0) then
                        ! "end function" is always two tokens
                        stmt_end = i + 1  ! Include both "end" and "function"
                        exit
                    end if
                end if

                i = i + 1
            end do

            if (stmt_end == start_pos) then
                stmt_end = i - 1  ! Couldn't find matching end
            end if
        else
            ! Single-line statement - find end of line or comment
            ! Track parentheses to handle array literals (/ ... /)
            block
                integer :: paren_depth
                paren_depth = 0
                i = start_pos
                do while (i <= size(tokens))
                    ! Track parentheses depth
                    if (tokens(i)%kind == TK_OPERATOR) then
                        if (tokens(i)%text == "(") then
                            paren_depth = paren_depth + 1
                        else if (tokens(i)%text == ")") then
                            paren_depth = paren_depth - 1
                        end if
                    end if

                    if (tokens(i)%kind == TK_EOF) then
                        stmt_end = i - 1
                        exit
                    else if (tokens(i)%kind == TK_COMMENT .and. i > start_pos) then
                        ! Stop before comment - let comment be parsed separately
                        stmt_end = i - 1
                        exit
                    else if (i < size(tokens) .and. tokens(i)%line < tokens(i + 1)%line .and. &
                             paren_depth == 0) then
                        ! Only end statement at line break if parentheses are balanced
                        stmt_end = i
                        exit
                    else
                        stmt_end = i
                        i = i + 1
                    end if
                end do
            end block
        end if
    end subroutine find_statement_boundary

    ! Helper functions to detect program unit types
    logical function is_function_start(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos

        is_function_start = .false.
        if (pos < 1 .or. pos > size(tokens)) return

        ! Only detect function start at the beginning of a line/statement
        ! Check for "type function" pattern first
        if (tokens(pos)%kind == TK_KEYWORD .and. &
            (tokens(pos)%text == "real" .or. tokens(pos)%text == "integer" .or. &
             tokens(pos)%text == "logical" .or. tokens(pos)%text == "character")) then
            if (pos + 1 <= size(tokens) .and. &
                tokens(pos + 1)%kind == TK_KEYWORD .and. &
                tokens(pos + 1)%text == "function") then
                ! Check if this is at the start of a line or after a statement boundary
                if (pos == 1) then
                    is_function_start = .true.
                else if (pos > 1 .and. tokens(pos - 1)%line < tokens(pos)%line) then
                    is_function_start = .true.  ! New line
                else if (pos > 2 .and. tokens(pos - 2)%text == "end" .and. &
                         tokens(pos - 1)%text == "function") then
                    is_function_start = .true.  ! After "end function"
                end if
            end if
            ! Check for standalone "function" keyword (not preceded by a type)
      else if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "function") then
            ! Make sure this isn't the second part of "type function" or "end function"
            if (pos > 1) then
                if (tokens(pos - 1)%kind == TK_KEYWORD .and. &
           (tokens(pos - 1)%text == "real" .or. tokens(pos - 1)%text == "integer" .or. &
       tokens(pos - 1)%text == "logical" .or. tokens(pos - 1)%text == "character" .or. &
                     tokens(pos - 1)%text == "end")) then
                    is_function_start = .false.  ! Already counted with the type &
                                                 ! or it's "end function"
                else
                    is_function_start = .true.
                end if
            else
                is_function_start = .true.
            end if
        end if
    end function is_function_start

    logical function is_subroutine_start(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos

        is_subroutine_start = .false.
        if (pos < 1 .or. pos > size(tokens)) return

        if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "subroutine") then
            is_subroutine_start = .true.
        end if
    end function is_subroutine_start

    logical function is_module_start(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos

        is_module_start = .false.
        if (pos < 1 .or. pos > size(tokens)) return

        if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "module") then
            is_module_start = .true.
        end if
    end function is_module_start

    logical function is_program_start(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos

        is_program_start = .false.
        if (pos < 1 .or. pos > size(tokens)) return

        if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "program") then
            is_program_start = .true.
        end if
    end function is_program_start

    logical function is_end_function(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos

        is_end_function = .false.
        if (pos + 1 > size(tokens)) return

        if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "end" .and. &
       tokens(pos + 1)%kind == TK_KEYWORD .and. tokens(pos + 1)%text == "function") then
            is_end_function = .true.
        end if
    end function is_end_function

    logical function is_end_subroutine(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos

        is_end_subroutine = .false.
        if (pos + 1 > size(tokens)) return

        if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "end" .and. &
     tokens(pos + 1)%kind == TK_KEYWORD .and. tokens(pos + 1)%text == "subroutine") then
            is_end_subroutine = .true.
        end if
    end function is_end_subroutine

    logical function is_end_module(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos

        is_end_module = .false.
        if (pos + 1 > size(tokens)) return

        if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "end" .and. &
         tokens(pos + 1)%kind == TK_KEYWORD .and. tokens(pos + 1)%text == "module") then
            is_end_module = .true.
        end if
    end function is_end_module

    ! Phase 4: Code Generation
    subroutine generate_fortran_code(arena, prog_index, code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: prog_index
        character(len=:), allocatable, intent(out) :: code

        code = generate_code_from_arena(arena, prog_index)
    end subroutine generate_fortran_code

    ! Write output to file
    subroutine write_output_file(filename, content, error_msg)
        character(len=*), intent(in) :: filename, content
        character(len=*), intent(out) :: error_msg

        integer :: unit, iostat
        type(path_validation_result_t) :: validation_result

        ! Validate output file path for security
        validation_result = validate_output_path(filename)
        if (.not. validation_result%is_valid()) then
            error_msg = "Output path validation failed: " // validation_result%get_message()
            return
        end if

     open (newunit=unit, file=filename, status='replace', action='write', iostat=iostat)
        if (iostat /= 0) then
            error_msg = "Cannot create output file: "//filename
            return
        end if

        write (unit, '(A)') content
        close (unit)
        error_msg = ""
    end subroutine write_output_file

    ! Helper function to convert integer to string
    function int_to_str(num) result(str)
        integer, intent(in) :: num
        character(len=20) :: str
        write (str, '(I0)') num
    end function int_to_str

    ! Check if token sequence starts a do loop
    logical function is_do_loop_start(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos

        is_do_loop_start = .false.
        if (pos >= 1 .and. pos <= size(tokens)) then
            if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "do") then
                ! Regular do loop (not do while)
                if (pos + 1 <= size(tokens)) then
      if (tokens(pos + 1)%kind == TK_KEYWORD .and. tokens(pos + 1)%text == "while") then
                        is_do_loop_start = .false.  ! It's a do while, not &
                                                    ! a regular do loop
                        ! Found do while, not do loop
                    else
                        is_do_loop_start = .true.
                        ! Found do loop start
                    end if
                else
                    is_do_loop_start = .true.
                    ! Found do loop start (end of tokens)
                end if
            end if
        end if
    end function is_do_loop_start

    ! Check if token sequence starts a do while loop
    logical function is_do_while_start(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos

        is_do_while_start = .false.
        if (pos >= 1 .and. pos <= size(tokens) - 1) then
            if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "do" .and. &
          tokens(pos + 1)%kind == TK_KEYWORD .and. tokens(pos + 1)%text == "while") then
                is_do_while_start = .true.
            end if
        end if
    end function is_do_while_start

    ! Check if token sequence starts a select case
    logical function is_select_case_start(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos

        is_select_case_start = .false.
        if (pos >= 1 .and. pos <= size(tokens) - 1) then
           if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "select" .and. &
           tokens(pos + 1)%kind == TK_KEYWORD .and. tokens(pos + 1)%text == "case") then
                is_select_case_start = .true.
            end if
        end if
    end function is_select_case_start

    ! Check if token sequence ends a do loop
    logical function is_end_do(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos

        is_end_do = .false.
        if (pos <= size(tokens) - 1) then
            if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "end" .and. &
             tokens(pos + 1)%kind == TK_KEYWORD .and. tokens(pos + 1)%text == "do") then
                is_end_do = .true.
            end if
        end if
    end function is_end_do

    ! Check if token sequence ends a select case
    logical function is_end_select(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos

        is_end_select = .false.
        if (pos <= size(tokens) - 1) then
            if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "end" .and. &
         tokens(pos + 1)%kind == TK_KEYWORD .and. tokens(pos + 1)%text == "select") then
                is_end_select = .true.
            end if
        end if
    end function is_end_select

    logical function is_if_then_start(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        integer :: i

        is_if_then_start = .false.
        if (pos < 1 .or. pos > size(tokens)) return

        ! Check if current token is "if"
        if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "if") then
            ! Check if this is "else if" - if so, it's not a new if block &
            ! for nesting purposes
            if (pos > 1 .and. pos <= size(tokens)) then
                if (tokens(pos - 1)%kind == TK_KEYWORD .and. &
                    tokens(pos - 1)%text == "else" .and. &
                    tokens(pos - 1)%line == tokens(pos)%line) then
                    ! This is "else if", not a new if block
                    is_if_then_start = .false.
                    return
                end if
            end if

            ! Look for "then" on the same line
            i = pos + 1
            do while (i <= size(tokens))
                if (tokens(i)%line /= tokens(pos)%line) exit
                if (tokens(i)%kind == TK_KEYWORD .and. tokens(i)%text == "then") then
                    is_if_then_start = .true.
                    exit
                end if
                i = i + 1
            end do
        end if
    end function is_if_then_start

    logical function is_end_if(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos

        is_end_if = .false.
        if (pos > size(tokens)) return

        ! Check for "endif" (single keyword)
        if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "endif") then
            is_end_if = .true.
            return
        end if

        ! Check for "end if" (two keywords)
        if (pos + 1 <= size(tokens)) then
            if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "end" .and. &
             tokens(pos + 1)%kind == TK_KEYWORD .and. tokens(pos + 1)%text == "if") then
                is_end_if = .true.
            end if
        end if
    end function is_end_if

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

    ! String-based transformation function for CLI usage
    subroutine transform_lazy_fortran_string(input, output, error_msg)
        character(len=*), intent(in) :: input
        character(len=:), allocatable, intent(out) :: output
        character(len=:), allocatable, intent(out) :: error_msg

        ! Local variables for 4-phase pipeline
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        


        allocate(character(len=0) :: error_msg)
        error_msg = ""

        ! Handle empty or whitespace-only input
        if (len_trim(input) == 0 .or. is_whitespace_only(input)) then
            output = "program main" // new_line('A') // &
                     "    implicit none" // new_line('A') // &
                     "end program main" // new_line('A')
            return
        end if

        ! Phase 1: Lexical Analysis
        call lex_source(input, tokens, error_msg)
        if (error_msg /= "") return

        ! Phase 1.5: Enhanced syntax validation with comprehensive error reporting (Issue #256)
        call validate_basic_syntax(input, tokens, error_msg)
        if (error_msg /= "") then
            ! Issue #256 requirement #4: No silent fallback to empty programs
            ! Issue #256 requirement #5: Meaningful output for invalid syntax
            ! Always preserve the error message for reporting and include it in output
            output = "! COMPILATION FAILED" // new_line('A') // &
                    "! Error: " // error_msg // new_line('A') // &
                    "program main" // new_line('A') // &
                    "    implicit none" // new_line('A') // &
                    "    ! Original code could not be parsed" // new_line('A') // &
                    "end program main" // new_line('A')
            return
        end if

        ! Check if validation passed but we have no meaningful content to parse
        ! This handles cases where input is only comments, whitespace, or empty
        block
            integer :: meaningful_tokens
            integer :: i

            meaningful_tokens = 0
            do i = 1, size(tokens)
                if (tokens(i)%kind == TK_EOF .or. tokens(i)%kind == TK_NEWLINE .or. &
                    tokens(i)%kind == TK_COMMENT) cycle
                meaningful_tokens = meaningful_tokens + 1
            end do

            if (meaningful_tokens == 0) then
                ! Create minimal program for empty/meaningless input
                output = "program main" // new_line('A') // &
                         "    implicit none" // new_line('A') // &
                         "end program main" // new_line('A')
                return
            end if
        end block

        ! Check if we have any tokens to parse
        if (size(tokens) == 0) then
            ! No tokens - create minimal program
            output = "program main" // new_line('A') // &
                     "    implicit none" // new_line('A') // &
                     "end program main" // new_line('A')
            return
        end if

        ! Check if we have only meaningless tokens (whitespace, comments, newlines)
        if (has_only_meaningless_tokens(tokens)) then
            ! Only meaningless tokens - create minimal program
            output = "program main" // new_line('A') // &
                     "    implicit none" // new_line('A') // &
                     "end program main" // new_line('A')
            return
        end if

        ! Phase 2: Parsing
        call init_ast_arena(arena)
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (error_msg /= "") then
            ! Enhanced error reporting for parsing failures (Issue #256 requirements)
            if (prog_index > 0) then
                call emit_fortran(arena, prog_index, output)
                ! Append comprehensive error information to output
                output = output // new_line('A') // &
                        "! COMPILATION FAILED - PARSING ERROR" // new_line('A') // &
                        "! " // error_msg // new_line('A') // &
                        "!" // new_line('A') // &
                        "! fortfront encountered errors while parsing the code structure." // new_line('A') // &
                        "! The partial output above may be incomplete or incorrect." // new_line('A')
            else
                ! No valid output, provide meaningful error information
                output = "! COMPILATION FAILED - PARSING ERROR" // new_line('A') // &
                        "! " // error_msg // new_line('A') // &
                        "!" // new_line('A') // &
                        "! fortfront could not understand the structure of your code." // new_line('A') // &
                        "! Please check for missing keywords, unmatched parentheses," // new_line('A') // &
                        "! or other structural issues." // new_line('A') // &
                        "program main" // new_line('A') // &
                        "    implicit none" // new_line('A') // &
                        "    ! ERROR: Original code could not be parsed" // new_line('A') // &
                        "end program main" // new_line('A')
            end if
            return
        end if

        ! Debug: check if we got a valid program index
        if (prog_index <= 0) then
            error_msg = "Parsing succeeded but no valid program unit was created"
            output = "! COMPILATION FAILED" // new_line('A') // &
                    "! Error: " // error_msg // new_line('A') // &
                    "program main" // new_line('A') // &
                    "    implicit none" // new_line('A') // &
                    "    ! Original code could not be structured as a program" // new_line('A') // &
                    "end program main" // new_line('A')
            return
        end if

        ! Phase 3: Semantic Analysis with Variable Declaration Generation
        block
            type(semantic_context_t) :: semantic_ctx
            call analyze_semantics_with_pipeline(arena, prog_index, semantic_ctx)

            ! Phase 4: Standardization
            ! Skip standardization for multi-unit containers
            block
                logical :: skip_standardization
                skip_standardization = .false.
                if (prog_index > 0 .and. prog_index <= arena%size) then
                    if (allocated(arena%entries(prog_index)%node)) then
                        select type (node => arena%entries(prog_index)%node)
                        type is (program_node)
                            if (node%name == "__MULTI_UNIT__") then
                                skip_standardization = .true.
                            end if
                        end select
                    end if
                end if

                if (.not. skip_standardization) then
                    call standardize_ast_with_semantics(arena, prog_index, semantic_ctx)
                end if
            end block
        end block

        ! Phase 5: Code Generation
        output = generate_code_from_arena(arena, prog_index)
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
        call get_indent_config(saved_size, saved_char)
        call get_line_length_config(saved_line_length)
        call get_type_standardization(saved_standardize_types)
        call get_standardizer_type_standardization(saved_standardizer_types)

        ! Set new configuration
        call set_indent_config(format_opts%indent_size, format_opts%indent_char)
        call set_line_length_config(format_opts%line_length)
        call set_type_standardization(format_opts%standardize_types)
        call set_standardizer_type_standardization(format_opts%standardize_types)

        ! Call the regular transformation function
        call transform_lazy_fortran_string(input, output, error_msg)

        ! Restore original configuration
        call set_indent_config(saved_size, saved_char)
        call set_line_length_config(saved_line_length)
        call set_type_standardization(saved_standardize_types)
        call set_standardizer_type_standardization(saved_standardizer_types)
    end subroutine transform_lazy_fortran_string_with_format

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

        call analyze_semantics_with_pipeline(arena, prog_index)
    end subroutine analyze_semantics

    subroutine emit_fortran(arena, prog_index, fortran_code)
        type(ast_arena_t), intent(in) :: arena  ! Made intent(in) to prevent corruption
        integer, intent(in) :: prog_index  ! Made intent(in) to prevent modification
        character(len=:), allocatable, intent(out) :: fortran_code

        ! CRITICAL FIX: Do NOT call standardize_ast here - it causes double standardization
        ! and memory corruption when called in error paths. Standardization happens once
        ! in the main transform pipeline only.
        fortran_code = generate_code_from_arena(arena, prog_index)
    end subroutine emit_fortran

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

end module frontend
