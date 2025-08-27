module frontend_transformation
    ! fortfront - Transformation functions module
    ! Contains string-based transformation functionality

    use lexer_core, only: token_t, tokenize_core, TK_EOF, TK_KEYWORD, &
                           TK_COMMENT, TK_NEWLINE, TK_OPERATOR, TK_IDENTIFIER, &
                           TK_NUMBER, TK_STRING, TK_UNKNOWN
    use compiler_arena, only: compiler_arena_t, create_compiler_arena, destroy_compiler_arena
    use semantic_analyzer, only: semantic_context_t, create_semantic_context, &
                                   analyze_program
    use standardizer, only: standardize_ast, set_standardizer_type_standardization, &
                           get_standardizer_type_standardization
    use codegen_core, only: generate_code_from_arena, set_type_standardization, &
                           get_type_standardization
    use codegen_indent, only: set_indent_config, get_indent_config, &
                               set_line_length_config, get_line_length_config
    use input_validation, only: validate_basic_syntax, has_only_meaningless_tokens
    use ast_nodes_core, only: program_node
    use frontend_parsing, only: parse_tokens
    use frontend_core, only: lex_source, emit_fortran

    implicit none
    private

    public :: transform_lazy_fortran_string, &
              transform_lazy_fortran_string_with_format, format_options_t

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
        type(token_t), allocatable :: tokens(:)
        type(compiler_arena_t) :: compiler_arena
        integer :: prog_index

        allocate(character(len=0) :: error_msg)
        error_msg = ""

        ! Initialize unified compiler arena
        compiler_arena = create_compiler_arena()

        ! Handle empty or whitespace-only input
        if (is_empty_or_whitespace_only(input)) then
            call create_minimal_program(output)
            call destroy_compiler_arena(compiler_arena)
            return
        end if

        ! Phase 1: Lexical Analysis
        call run_lexical_analysis(input, tokens, compiler_arena, error_msg)
        if (error_msg /= "") then
            call handle_lexical_error(input, error_msg, output, compiler_arena)
            return
        end if

        ! Phase 1.5: Enhanced syntax validation with comprehensive error reporting (Issue #256)
        call validate_syntax_with_reporting(input, tokens, error_msg, output, compiler_arena)
        if (error_msg /= "") return

        ! Check for meaningful content
        if (not_meaningful_for_parsing(tokens)) then
            call create_minimal_program(output)
            call destroy_compiler_arena(compiler_arena)
            return
        end if

        ! Phase 2: Parsing
        call run_parsing_phase(tokens, compiler_arena, prog_index, error_msg, output)
        if (error_msg /= "") return

        ! Phases 3-5: Semantic Analysis, Standardization, Code Generation
        print *, "DEBUG: About to call run_final_phases with prog_index=", prog_index
        call run_final_phases(compiler_arena, prog_index, output)
        print *, "DEBUG: run_final_phases completed"

        ! Cleanup unified compiler arena
        call destroy_compiler_arena(compiler_arena)
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

        output = "! COMPILATION FAILED" // new_line('A') // &
                "! Error: " // error_msg // new_line('A') // &
                "program main" // new_line('A') // &
                "    implicit none" // new_line('A') // &
                "    ! Original code could not be parsed" // new_line('A') // &
                "end program main" // new_line('A')
        call destroy_compiler_arena(compiler_arena)
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
            ! Issue #256 requirement #4: No silent fallback to empty programs
            ! Issue #256 requirement #5: Meaningful output for invalid syntax
            ! Always preserve the error message for reporting and include it in output
            output = "! COMPILATION FAILED" // new_line('A') // &
                    "! Error: " // error_msg // new_line('A') // &
                    "program main" // new_line('A') // &
                    "    implicit none" // new_line('A') // &
                    "    ! Original code could not be parsed" // new_line('A') // &
                    "end program main" // new_line('A')
            call destroy_compiler_arena(compiler_arena)
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

        ! Phase 2: Parsing
        call compiler_arena%next_phase("parser")
        call parse_tokens(tokens, compiler_arena%ast, prog_index, error_msg)
        if (error_msg /= "") then
            call handle_parsing_error(compiler_arena, prog_index, error_msg, output)
            return
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

        ! Enhanced error reporting for parsing failures (Issue #256 requirements)
        if (prog_index > 0) then
            call emit_fortran(compiler_arena%ast, prog_index, output)
            ! Append comprehensive error information to output
            output = output // new_line('A') // &
                    "! COMPILATION FAILED - PARSING ERROR" // new_line('A') // &
                    "! " // error_msg // new_line('A') // &
                    "!" // new_line('A') // &
                    "! fortfront encountered errors while parsing the code structure." // new_line('A') // &
                    "! The partial output above may be incomplete or incorrect." // new_line('A')
        else
            call create_parsing_error_program(error_msg, output)
        end if
        call destroy_compiler_arena(compiler_arena)
    end subroutine handle_parsing_error

    ! Create parsing error program
    subroutine create_parsing_error_program(error_msg, output)
        character(len=*), intent(in) :: error_msg
        character(len=:), allocatable, intent(out) :: output

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
    end subroutine create_parsing_error_program

    ! Handle invalid program index
    subroutine handle_invalid_program_index(error_msg, output, compiler_arena)
        character(len=:), allocatable, intent(inout) :: error_msg
        character(len=:), allocatable, intent(out) :: output
        type(compiler_arena_t), intent(inout) :: compiler_arena

        error_msg = "Parsing succeeded but no valid program unit was created"
        output = "! COMPILATION FAILED" // new_line('A') // &
                "! Error: " // error_msg // new_line('A') // &
                "program main" // new_line('A') // &
                "    implicit none" // new_line('A') // &
                "    ! Original code could not be structured as a program" // new_line('A') // &
                "end program main" // new_line('A')
        call destroy_compiler_arena(compiler_arena)
    end subroutine handle_invalid_program_index

    ! Run final phases (semantic, standardization, codegen)
    subroutine run_final_phases(compiler_arena, prog_index, output)
        type(compiler_arena_t), intent(inout) :: compiler_arena
        integer, intent(inout) :: prog_index
        character(len=:), allocatable, intent(out) :: output

        ! Phase 3: Semantic Analysis
        call run_semantic_analysis_phase(compiler_arena, prog_index)

        ! Phase 4: Standardization
        call run_standardization_phase(compiler_arena, prog_index)

        ! Phase 5: Code Generation
        call run_code_generation_phase(compiler_arena, prog_index, output)
    end subroutine run_final_phases

    ! Run semantic analysis phase
    subroutine run_semantic_analysis_phase(compiler_arena, prog_index)
        type(compiler_arena_t), intent(inout) :: compiler_arena
        integer, intent(in) :: prog_index

        print *, "DEBUG: run_semantic_analysis_phase called with prog_index=", prog_index
        call compiler_arena%next_phase("semantic")
        block
            type(semantic_context_t) :: ctx
            ctx = create_semantic_context()
            print *, "DEBUG: About to call analyze_program"
            call analyze_program(ctx, compiler_arena%ast, prog_index)
            print *, "DEBUG: analyze_program completed"
        end block
    end subroutine run_semantic_analysis_phase

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
        output = generate_code_from_arena(compiler_arena%ast, prog_index)
    end subroutine run_code_generation_phase

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

end module frontend_transformation