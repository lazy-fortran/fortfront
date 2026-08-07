module frontend_core
    ! fortfront - Core frontend API module
    ! Main entry points for compilation pipeline

    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront_constants, only: MAX_FRONTEND_ERROR_LEN
    use string_builder_mod, only: join_strings
    use lexer_core, only: token_t, tokenize_core, &
        validate_source_characters, &
        find_unterminated_character_constant, &
        TK_COMMENT, TK_NEWLINE, TK_OPERATOR, TK_IDENTIFIER, &
        TK_NUMBER, TK_STRING, TK_UNKNOWN, TK_WHITESPACE, &
        to_lower
    use parser_dispatcher_module, only: &
        get_additional_indices, clear_additional_indices
    use parser_control_flow_module, only: &
        parse_select_case
    ! Migrated from ast_core: use explicit imports for better dependency management
    use ast_arena_modern, only: ast_arena_t
    use compiler_arena, only: compiler_arena_t, create_compiler_arena, &
        & destroy_compiler_arena
    use semantic_analyzer, only: semantic_context_t, create_semantic_context, &
        analyze_program, has_semantic_errors
    use type_system_unified, only: reset_type_system
    use standardizer, only: standardize_ast, &
        get_standardizer_type_standardization, &
        standardize_multi_unit_children
    use codegen_arena_interface, only: generate_code_from_arena
    use codegen_core, only: initialize_codegen
    use codegen_indent, only: &
        set_line_length_config, get_line_length_config
    use path_validation, only: validate_input_path, &
        & path_validation_result_t
    use frontend_parsing, only: parse_tokens, parse_tokens_safe, &
        parse_result_with_index_t
    use frontend_transformation_analysis, only: promote_functions_to_internal_program
    use frontend_transformation_semantics, only: get_detailed_semantic_errors
    use frontend_utilities, only: write_output_file
    use semantic_input_mode, only: INPUT_MODE_LAZY

    implicit none
    private

    public :: lex_source, analyze_semantics, emit_fortran
    public :: compile_source, compilation_options_t
    public :: lex_file
    public :: parse_tokens_safe, parse_result_with_index_t
    public :: normalize_fixed_form_source_text, is_fixed_form_file

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
        type(token_t), allocatable, target :: tokens(:)
        type(compiler_arena_t) :: compiler_arena
        integer :: prog_index
        character(len=:), allocatable :: code, source
        logical :: is_fixed_form
        integer :: unit, iostat
        type(path_validation_result_t) :: validation_result

        ! Log compilation start with proper logging
        write (error_unit, '(A)') "INFO [frontend_core]: Starting compilation of " &
            & //input_file

        error_msg = ""

        ! Reset type system arena to prevent type accumulation across compilations
        ! IMPORTANT: Must be called to clear global type state before each compilation
        call reset_type_system()

        ! Validate input file path for security
        validation_result = validate_input_path(input_file)
        if (.not. validation_result%is_valid()) then
            error_msg = "Input path validation failed: "// &
                & validation_result%get_message()
            return
        end if

        ! Detect fixed-form source so continuation markers are preserved
        is_fixed_form = is_fixed_form_file(input_file)

        ! Read source file
        call read_source_file(input_file, source, error_msg, &
            fixed_form=is_fixed_form)
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
        call standardize_multi_unit_children(compiler_arena%ast, prog_index)
        call standardize_ast(compiler_arena%ast, prog_index)
        call promote_functions_to_internal_program(compiler_arena%ast, prog_index)

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
        call unterminated_character_constant_error(source, error_msg)
        if (len_trim(error_msg) > 0) return
        call tokenize_core(source, tokens)
        if (allocated(tokens)) then
            call normalize_line_continuations(tokens)
        end if
    end subroutine lex_file

    ! Reject a character literal that is never closed. Leaves error_msg empty
    ! when every character context on every line is terminated.
    subroutine unterminated_character_constant_error(source, error_msg)
        character(len=*), intent(in) :: source
        character(len=*), intent(out) :: error_msg
        integer :: bad_line, bad_column
        character(len=32) :: line_text, column_text

        error_msg = ""
        if (.not. find_unterminated_character_constant(source, bad_line, &
                                                       bad_column)) return
        write (line_text, '(I0)') bad_line
        write (column_text, '(I0)') bad_column
        error_msg = "Unterminated character constant at line "// &
                    trim(line_text)//", column "//trim(column_text)
    end subroutine unterminated_character_constant_error

    ! Simple interface functions for clean pipeline usage
    subroutine lex_source(source_code, tokens, error_msg)
        character(len=*), intent(in) :: source_code
        type(token_t), allocatable, intent(out) :: tokens(:)
        character(len=:), allocatable, intent(out) :: error_msg

        call validate_source_characters(source_code, error_msg)
        if (len_trim(error_msg) > 0) then
            allocate (tokens(0))
            return
        end if

        block
            character(len=MAX_FRONTEND_ERROR_LEN) :: literal_error

            call unterminated_character_constant_error(source_code, literal_error)
            if (len_trim(literal_error) > 0) then
                allocate (tokens(0))
                error_msg = trim(literal_error)
                return
            end if
        end block

        call tokenize_core(source_code, tokens)
        if (.not. allocated(tokens)) then
            error_msg = "Failed to tokenize source"
        else
            error_msg = ""
            call normalize_line_continuations(tokens)
        end if
    end subroutine lex_source

    subroutine analyze_semantics(arena, prog_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: prog_index
        character(len=MAX_FRONTEND_ERROR_LEN) :: error_msg

        call run_semantic_analysis(arena, prog_index, error_msg)
        ! Note: For backward compatibility, this version doesn't propagate errors
    end subroutine analyze_semantics

    subroutine emit_fortran(arena, prog_index, fortran_code)
        type(ast_arena_t), intent(in) :: arena ! Made intent(in) to prevent corruption
        integer, intent(in) :: prog_index ! Made intent(in) to prevent modification
        character(len=:), allocatable, intent(out) :: fortran_code

        ! Initialize the codegen system
        call initialize_codegen()

        ! CRITICAL FIX: Do not call standardize_ast here. It causes double
        ! standardization and memory corruption when called in error paths.
        ! Standardization happens once in the main transform pipeline.
        fortran_code = generate_code_from_arena(arena, prog_index)
        call normalize_emitted_code(fortran_code)
    end subroutine emit_fortran

    ! Private helper subroutines to break down large functions

    subroutine normalize_emitted_code(code)
        character(len=:), allocatable, intent(inout) :: code
        character(len=:), allocatable :: result
        character(len=:), allocatable :: line
        integer :: start_pos, newline_pos, code_len

        if (.not. allocated(code)) return

        code_len = len(code)
        if (code_len == 0) return

        result = ''
        start_pos = 1

        do
            newline_pos = index(code(start_pos:), new_line('A'))
            if (newline_pos == 0) then
                line = code(start_pos:)
                call append_trimmed_line(line, .false., result)
                exit
            else
                if (newline_pos > 1) then
                    line = code(start_pos:start_pos + newline_pos - 2)
                else
                    line = ''
                end if
                call append_trimmed_line(line, .true., result)
                start_pos = start_pos + newline_pos
                if (start_pos > code_len) exit
            end if
        end do

        call move_alloc(result, code)
    contains
        subroutine append_trimmed_line(raw_line, add_newline, buffer)
            character(len=*), intent(in) :: raw_line
            logical, intent(in) :: add_newline
            character(len=:), allocatable, intent(inout) :: buffer
            character(len=:), allocatable :: trimmed_line
            integer :: last_idx

            trimmed_line = raw_line
            last_idx = len(trimmed_line)

            do while (last_idx >= 1)
                if (trimmed_line(last_idx:last_idx) /= ' ') exit
                last_idx = last_idx - 1
            end do

            if (last_idx >= 1) then
                trimmed_line = trimmed_line(1:last_idx)
            else
                trimmed_line = ''
            end if

            if (add_newline) then
                buffer = buffer//trimmed_line//new_line('A')
            else
                buffer = buffer//trimmed_line
            end if
        end subroutine append_trimmed_line
    end subroutine normalize_emitted_code

    subroutine read_source_file(input_file, source, error_msg, fixed_form)
        character(len=*), intent(in) :: input_file
        character(len=:), allocatable, intent(out) :: source
        character(len=*), intent(out) :: error_msg
        logical, intent(in), optional :: fixed_form
        integer :: unit, iostat
        logical :: use_fixed_form

        ! Read source file
        open (newunit=unit, file=input_file, status='old', action='read', iostat=iostat)
        if (iostat /= 0) then
            error_msg = "Cannot open input file: "//input_file
            return
        end if

        use_fixed_form = .false.
        if (present(fixed_form)) use_fixed_form = fixed_form

        block
            character(len=:), allocatable :: line
            character(len=:), allocatable :: lines(:)
            character(len=:), allocatable :: temp_lines(:)
            integer :: line_count
            integer :: capacity

            allocate (character(len=1000) :: line)
            capacity = 100
            allocate (character(len=1000) :: lines(capacity))
            line_count = 0

            do
                read (unit, '(A)', iostat=iostat) line
                if (iostat /= 0) exit

                line_count = line_count + 1
                if (line_count > capacity) then
                    allocate (character(len=1000) :: temp_lines(capacity * 2))
                    temp_lines(1:capacity) = lines(1:capacity)
                    call move_alloc(temp_lines, lines)
                    capacity = capacity * 2
                end if
                lines(line_count) = trim(line)
            end do

            if (use_fixed_form) then
                call normalize_fixed_form_lines(lines, line_count)
            end if

            if (line_count > 0) then
                source = join_strings(lines(1:line_count), new_line('a'))
            else
                allocate (character(len=0) :: source)
            end if
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
            allocate (ctx)
            call create_semantic_context(ctx)

            ! Use lazy mode here; will be upgraded to standard mode if implicit none
            ! is detected in the semantic analyzer
            ctx%input_mode = INPUT_MODE_LAZY

            call analyze_program(ctx, arena, prog_index)

            ! Check for semantic errors and provide detailed error messages
            if (has_semantic_errors(ctx)) then
                error_msg = get_detailed_semantic_errors(ctx)
                return
            end if

        end block

        error_msg = ""
    end subroutine run_semantic_analysis

    subroutine run_compilation_pipeline_from_phase2(tokens, compiler_arena, &
            prog_index, &
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
        character(len=MAX_FRONTEND_ERROR_LEN) :: error_msg

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

    ! Normalize free-form line continuations indicated with '&'
    subroutine normalize_line_continuations(tokens)
        type(token_t), allocatable, intent(inout) :: tokens(:)
        type(token_t), allocatable :: normalized(:)
        type(token_t) :: token_to_append
        integer :: i, count
        logical :: suppress_newline
        logical :: skip_leading_ampersand
        logical :: continuation_active
        integer :: continuation_line

        if (.not. allocated(tokens)) return
        if (size(tokens) == 0) return

        allocate (normalized(size(tokens)))
        suppress_newline = .false.
        skip_leading_ampersand = .false.
        continuation_active = .false.
        continuation_line = 0
        count = 0

        do i = 1, size(tokens)
            if (tokens(i)%kind == TK_OPERATOR) then
                if (allocated(tokens(i)%text)) then
                    if (is_line_continuation_token(tokens(i)%text)) then
                        continuation_active = .true.
                        if (count > 0) continuation_line = normalized(count)%line
                    end if
                end if
            end if
            if (should_skip_token(tokens, i, tokens(i), suppress_newline, &
                skip_leading_ampersand, continuation_active)) then
                cycle
            end if
            token_to_append = tokens(i)
            if (continuation_active .and. token_to_append%kind /= TK_NEWLINE) then
                token_to_append%line = continuation_line
            else if (token_to_append%kind == TK_NEWLINE) then
                continuation_active = .false.
            end if
            call append_token(normalized, count, token_to_append)
        end do

        call finalize_normalized_tokens(tokens, normalized, count)
    end subroutine normalize_line_continuations

    logical function should_skip_token(tokens, token_index, token, suppress_newline, &
            skip_leading_ampersand, continuation_active)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: token_index
        type(token_t), intent(in) :: token
        logical, intent(inout) :: suppress_newline
        logical, intent(inout) :: skip_leading_ampersand
        logical, intent(in) :: continuation_active

        should_skip_token = .false.
        select case (token%kind)
        case (TK_WHITESPACE, TK_COMMENT)
            if (skip_leading_ampersand) should_skip_token = .true.
        case (TK_OPERATOR)
            if (.not. allocated(token%text)) then
                skip_leading_ampersand = .false.
                return
            end if
            if (.not. is_line_continuation_token(token%text)) then
                skip_leading_ampersand = .false.
                return
            end if
            if (skip_leading_ampersand) then
                should_skip_token = .true.
                return
            end if
            skip_leading_ampersand = .true.
            suppress_newline = .true.
            should_skip_token = .true.
        case (TK_NEWLINE)
            ! A full-line comment (or blank line) between the trailing
            ! ampersand and the next source line is still part of the same
            ! free-form continuation.  Keep the continuation state alive and
            ! suppress this physical newline; otherwise expression parsers see
            ! an unexpected statement boundary after the trivia line.
            if (continuation_active .and. skip_leading_ampersand .and. &
                .not. suppress_newline) then
                should_skip_token = .true.
                return
            end if
            if (.not. suppress_newline) then
                if (line_starts_with_continuation(tokens, token_index)) then
                    suppress_newline = .true.
                    skip_leading_ampersand = .true.
                else
                    skip_leading_ampersand = .false.
                end if
            end if
            if (suppress_newline) then
                suppress_newline = .false.
                should_skip_token = .true.
            end if
        case default
            skip_leading_ampersand = .false.
        end select
    end function should_skip_token

    pure logical function line_starts_with_continuation(tokens, token_index) &
            result(has_continuation)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: token_index
        integer :: j

        has_continuation = .false.
        if (token_index >= size(tokens)) return
        do j = token_index + 1, size(tokens)
            select case (tokens(j)%kind)
            case (TK_WHITESPACE, TK_COMMENT)
                cycle
            case (TK_OPERATOR)
                if (.not. allocated(tokens(j)%text)) return
                has_continuation = is_line_continuation_token(tokens(j)%text)
                return
            case (TK_NEWLINE)
                return
            case default
                return
            end select
        end do
    end function line_starts_with_continuation

    subroutine append_token(buffer, count, token)
        type(token_t), allocatable, intent(inout) :: buffer(:)
        integer, intent(inout) :: count
        type(token_t), intent(in) :: token

        count = count + 1
        buffer(count) = token
    end subroutine append_token

    subroutine finalize_normalized_tokens(tokens, normalized, count)
        type(token_t), allocatable, intent(inout) :: tokens(:)
        type(token_t), allocatable, intent(inout) :: normalized(:)
        integer, intent(in) :: count
        type(token_t), allocatable :: trimmed(:)

        if (count == size(tokens)) then
            deallocate (normalized)
            return
        end if

        if (count <= 0) then
            if (allocated(tokens)) deallocate (tokens)
            allocate (tokens(0))
            if (allocated(normalized)) deallocate (normalized)
            return
        end if

        allocate (trimmed(count))
        trimmed = normalized(1:count)
        call move_alloc(trimmed, tokens)
        if (allocated(normalized)) deallocate (normalized)
    end subroutine finalize_normalized_tokens

    pure logical function is_line_continuation_token(text) result(is_continuation)
        character(len=*), intent(in) :: text
        integer :: idx, n
        character(len=1) :: ch

        is_continuation = .false.
        n = len(text)
        if (n == 0) return

        idx = 1
        do while (idx <= n)
            ch = text(idx:idx)
            if (.not. is_whitespace_char(ch)) exit
            idx = idx + 1
        end do
        if (idx > n) return
        if (text(idx:idx) /= "&") return

        idx = idx + 1
        do while (idx <= n)
            ch = text(idx:idx)
            if (.not. is_whitespace_char(ch)) return
            idx = idx + 1
        end do
        is_continuation = .true.
    end function is_line_continuation_token

    pure logical function is_whitespace_char(ch) result(is_ws)
        character(len=1), intent(in) :: ch

        is_ws = (iachar(ch) <= 32)
    end function is_whitespace_char

    pure logical function is_fixed_form_file(path) result(is_fixed)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: lower_path

        lower_path = to_lower(trim(path))
        is_fixed = has_suffix(lower_path, ".f") .or. &
            has_suffix(lower_path, ".for") .or. &
            has_suffix(lower_path, ".ftn") .or. &
            has_suffix(lower_path, ".f77")
    end function is_fixed_form_file

    pure logical function has_suffix(text, suffix) result(matches)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: suffix
        integer :: text_len, suffix_len

        text_len = len_trim(text)
        suffix_len = len_trim(suffix)
        if (text_len < suffix_len) then
            matches = .false.
        else
            matches = text(text_len - suffix_len + 1:text_len) == suffix
        end if
    end function has_suffix

    subroutine normalize_fixed_form_lines(lines, line_count)
        character(len=:), allocatable, intent(inout) :: lines(:)
        integer, intent(in) :: line_count
        integer :: i

        if (.not. allocated(lines)) return
        if (line_count <= 0) return

        do i = 1, line_count
            call normalize_fixed_form_line(lines(i))
        end do
    end subroutine normalize_fixed_form_lines

    subroutine normalize_fixed_form_line(line)
        character(len=*), intent(inout) :: line
        integer :: len_line
        character(len=1) :: cont_char
        character(len=:), allocatable :: body

        len_line = len(line)
        if (len_trim(line) > 0) then
            if (line(1:1) == "&") return ! Already normalized to free form
        end if
        if (len_line < 6) return
        if (is_fixed_form_comment(line)) then
            ! The lexer understands free-form `!` comments.  Preserve the
            ! comment text while translating the fixed-form column-1 marker;
            ! otherwise a source with no continuation line tokenizes `C` as
            ! an identifier and fails before parsing.
            line(1:1) = "!"
            return
        end if

        cont_char = line(6:6)
        if (cont_char == " " .or. cont_char == "0") return

        if (len_line > 6) then
            body = adjustl(line(7:len_line))
        else
            allocate (character(len=0) :: body)
        end if

        if (len(body) > 0) then
            line = "& "//trim(body)
        else
            line = "&"
        end if
    end subroutine normalize_fixed_form_line

    pure logical function is_fixed_form_comment(line) result(is_comment)
        character(len=*), intent(in) :: line
        character(len=1) :: first_char

        is_comment = .false.
        if (len(line) == 0) return

        first_char = line(1:1)
        select case (first_char)
        case ("c", "C", "*", "!")
            is_comment = .true.
        case default
            is_comment = .false.
        end select
    end function is_fixed_form_comment

    subroutine normalize_fixed_form_source_text(source)
        character(len=:), allocatable, intent(inout) :: source
        character(len=:), allocatable :: lines(:)
        integer :: line_count

        if (.not. allocated(source)) return
        if (len(source) == 0) return

        call split_source_into_lines(source, lines, line_count)
        if (line_count <= 0) return

        call normalize_fixed_form_lines(lines, line_count)
        source = join_strings(lines(1:line_count), new_line('a'))
    end subroutine normalize_fixed_form_source_text

    subroutine split_source_into_lines(source, lines, line_count)
        character(len=*), intent(in) :: source
        character(len=:), allocatable, intent(out) :: lines(:)
        integer, intent(out) :: line_count
        integer :: i, src_len
        integer :: start_pos, end_pos
        integer :: current_line
        character(len=1), parameter :: nl = new_line('a')

        src_len = len(source)
        line_count = 1

        do i = 1, src_len
            if (source(i:i) == nl) line_count = line_count + 1
        end do

        allocate (character(len=src_len) :: lines(line_count))

        start_pos = 1
        current_line = 1
        do while (start_pos <= src_len .and. current_line <= line_count)
            end_pos = index(source(start_pos:), nl)
            if (end_pos == 0) then
                lines(current_line) = source(start_pos:)
                exit
            end if

            if (end_pos == 1) then
                lines(current_line) = ""
            else
                lines(current_line) = source(start_pos:start_pos + end_pos - 2)
            end if

            start_pos = start_pos + end_pos
            current_line = current_line + 1
        end do

        if (current_line <= line_count) then
            lines(current_line:line_count) = ""
        end if
    end subroutine split_source_into_lines

end module frontend_core
