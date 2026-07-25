module frontend_program_units
    ! Program unit detection and parsing functionality
    ! Handles module, function, subroutine, type, and program unit parsing

    use lexer_core, only: token_t, TK_EOF, TK_KEYWORD, TK_COMMENT, &
        TK_NEWLINE, TK_OPERATOR, TK_IDENTIFIER, TK_NUMBER, &
        TK_STRING, TK_UNKNOWN, TK_WHITESPACE, to_lower
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_definition_statements_module, only: parse_function_definition
    use parser_procedure_definitions_module, only: &
        init_interface_procedure_parser
    use parser_prefix_buffer_module, only: parser_prefix_buffer_t
    use parser_dispatcher_module, only: parse_statement_dispatcher, &
        get_last_parser_errors
    use mixed_construct_detector, only: function_follows_type_spec
    use frontend_statement_processing, only: parse_all_statements, &
        parse_explicit_program_unit
    use frontend_statement_token_parsing, only: clear_statement_label_error, &
        get_statement_label_error
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_program
    use frontend_utilities, only: is_type_start
    use error_reporting, only: error_collection_t

    implicit none
    private

    ! Public program unit parsing interface
    public :: parse_program_unit, parse_module_unit, parse_submodule_unit
    public :: parse_function_unit
    public :: parse_subroutine_unit, parse_type_unit, parse_explicit_program_unit
    public :: parse_implicit_main_program, parse_block_data_unit
    public :: not_meaningful_program_unit, has_any_non_comment_content, &
        has_executable_statements

    ! Helper functions for unit detection
    public :: is_function_start, is_subroutine_start, is_module_start, &
        is_submodule_start, is_program_start, is_type_start, is_block_data_start

contains

    ! Main program unit parsing dispatch
    function parse_program_unit(tokens, arena, has_explicit_program, error_msg, &
            diagnostic_sink) &
            result(unit_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        logical, intent(in) :: has_explicit_program
        character(len=:), allocatable, intent(out), optional :: error_msg
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink
        integer :: unit_index
        type(parser_prefix_buffer_t) :: prefix_buffer
        integer :: first_token_pos
        type(token_t), allocatable :: trimmed_tokens(:)
        character(len=:), allocatable :: parse_error

        ! Check for meaningful content first
        if (not_meaningful_program_unit(tokens)) then
            unit_index = 0
            return
        end if

        first_token_pos = find_first_meaningful_token(tokens)
        if (first_token_pos == 0) then
            unit_index = 0
            return
        end if

        trimmed_tokens = tokens(first_token_pos:)
        if (size(trimmed_tokens) == 0) then
            unit_index = 0
            return
        end if

        ! Initialize parse_error
        parse_error = ""

        ! Determine unit type and parse accordingly
        block
            if (is_function_start(trimmed_tokens, 1)) then
                unit_index = parse_function_unit(trimmed_tokens, arena, parse_error, &
                    diagnostic_sink)
            else if (is_subroutine_start(trimmed_tokens, 1)) then
                unit_index = parse_subroutine_unit(trimmed_tokens, arena, parse_error, &
                    diagnostic_sink)
            else if (is_submodule_start(trimmed_tokens, 1)) then
                ! Parse the entire submodule with its content
                unit_index = parse_submodule_unit(trimmed_tokens, arena, parse_error, &
                    diagnostic_sink)
            else if (is_module_start(trimmed_tokens, 1)) then
                ! Parse the entire module with its content
                unit_index = parse_module_unit(trimmed_tokens, arena, parse_error, &
                    diagnostic_sink)
            else if (is_template_start(trimmed_tokens, 1)) then
                unit_index = parse_template_unit(trimmed_tokens, arena, parse_error, &
                    diagnostic_sink)
            else if (is_program_start(trimmed_tokens, 1)) then
                unit_index = parse_explicit_program_unit(trimmed_tokens, arena, &
                    parse_error, diagnostic_sink)
            else if (is_block_data_start(trimmed_tokens, 1)) then
                ! Parse BLOCK DATA unit
                unit_index = parse_block_data_unit(trimmed_tokens, arena, parse_error, &
                    diagnostic_sink)
            else if (is_interface_start(trimmed_tokens, 1)) then
                unit_index = parse_interface_unit(trimmed_tokens, arena, &
                    parse_error, diagnostic_sink)
            else
                ! Mixed module/main files still require implicit main detection
                unit_index = parse_implicit_main_program(trimmed_tokens, arena, &
                    has_explicit_program, &
                    parse_error, diagnostic_sink)
            end if
        end block

        ! Propagate error message if requested
        block
            if (allocated(parse_error)) then
            end if

            if (present(error_msg)) then
                if (allocated(parse_error)) then
                    error_msg = parse_error
                else
                    error_msg = ""
                end if
            end if
        end block
    end function parse_program_unit

    ! Parse module unit with all its content
    function parse_module_unit(tokens, arena, error_msg, diagnostic_sink) &
            result(unit_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        character(len=:), allocatable, intent(out), optional :: error_msg
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink
        integer :: unit_index
        type(parser_prefix_buffer_t) :: prefix_buffer

        ! Parse the complete module including its content
        ! Module should be parsed with all its statements
        unit_index = parse_statement_dispatcher(tokens, arena, prefix_buffer, &
            diagnostic_sink, error_msg)
    end function parse_module_unit

    function parse_template_unit(tokens, arena, error_msg, diagnostic_sink) &
            result(unit_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        character(len=:), allocatable, intent(out), optional :: error_msg
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink
        integer :: unit_index
        type(parser_prefix_buffer_t) :: prefix_buffer

        unit_index = parse_statement_dispatcher(tokens, arena, prefix_buffer, &
            diagnostic_sink, error_msg)
    end function parse_template_unit

    ! Parse submodule unit with all its content (Fortran 2008)
    function parse_submodule_unit(tokens, arena, error_msg, diagnostic_sink) &
            result(unit_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        character(len=:), allocatable, intent(out), optional :: error_msg
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink
        integer :: unit_index
        type(parser_prefix_buffer_t) :: prefix_buffer

        ! Parse the complete submodule including its content
        unit_index = parse_statement_dispatcher(tokens, arena, prefix_buffer, &
            diagnostic_sink, error_msg)
    end function parse_submodule_unit

    ! Check if program unit has meaningful content
    function not_meaningful_program_unit(tokens) result(not_meaningful)
        type(token_t), intent(in) :: tokens(:)
        logical :: not_meaningful
        logical :: has_content
        integer :: i

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
        not_meaningful = (.not. has_content .and. size(tokens) <= 1)
    end function not_meaningful_program_unit

    ! Parse function unit
    function parse_function_unit(tokens, arena, error_msg, diagnostic_sink) &
            result(unit_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        character(len=:), allocatable, intent(out), optional :: error_msg
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink
        integer :: unit_index

        ! Initialize error message
        if (present(error_msg)) error_msg = ""

        ! Multi-line function definition
        block
            type(parser_state_t) :: parser
            type(parser_prefix_buffer_t) :: prefix_buffer
            call init_interface_procedure_parser()
            parser = create_parser_state(tokens, diagnostic_sink)
            unit_index = parse_function_definition(parser, arena, prefix_buffer)
            ! Extract parser errors before parser goes out of scope
            if (present(error_msg) .and. parser%has_errors()) then
                error_msg = parser%get_error_messages()
            end if
        end block
    end function parse_function_unit

    ! Parse subroutine unit
    function parse_subroutine_unit(tokens, arena, error_msg, diagnostic_sink) &
            result(unit_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        character(len=:), allocatable, intent(out), optional :: error_msg
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink
        integer :: unit_index
        type(parser_prefix_buffer_t) :: prefix_buffer

        ! Parse the subroutine using the dispatcher
        unit_index = parse_statement_dispatcher(tokens, arena, prefix_buffer, &
            diagnostic_sink, error_msg)
    end function parse_subroutine_unit

    ! Parse type unit
    function parse_type_unit(tokens, arena, error_msg, diagnostic_sink) &
            result(unit_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        character(len=:), allocatable, intent(out), optional :: error_msg
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink
        integer :: unit_index
        type(parser_prefix_buffer_t) :: prefix_buffer

        ! Statement dispatcher handles parser creation for type definitions
        unit_index = parse_statement_dispatcher(tokens, arena, prefix_buffer, &
            diagnostic_sink, error_msg)
    end function parse_type_unit

    ! Parse BLOCK DATA unit
    function parse_block_data_unit(tokens, arena, error_msg, diagnostic_sink) &
            result(unit_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        character(len=:), allocatable, intent(out), optional :: error_msg
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink
        integer :: unit_index
        type(parser_prefix_buffer_t) :: prefix_buffer

        ! Parse BLOCK DATA using statement dispatcher
        unit_index = parse_statement_dispatcher(tokens, arena, prefix_buffer, &
            diagnostic_sink, error_msg)
    end function parse_block_data_unit

    function parse_interface_unit(tokens, arena, error_msg, diagnostic_sink) &
            result(unit_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        character(len=:), allocatable, intent(out), optional :: error_msg
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink
        integer :: unit_index
        type(parser_prefix_buffer_t) :: prefix_buffer

        unit_index = parse_statement_dispatcher(tokens, arena, prefix_buffer, &
            diagnostic_sink)

        if (present(error_msg)) error_msg = ""
    end function parse_interface_unit

    ! Parse implicit main program
    function parse_implicit_main_program(tokens, arena, has_explicit_program, &
            error_msg, diagnostic_sink) result(prog_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        logical, intent(in) :: has_explicit_program
        character(len=:), allocatable, intent(out), optional :: error_msg
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink
        integer :: prog_index
        character(len=:), allocatable :: errors
        character(len=:), allocatable :: label_error

        call clear_statement_label_error()

        ! Check for meaningful content that should become an implicit main
        if (has_any_non_comment_content(tokens)) then
            if (has_executable_statements(tokens)) then
                ! Parse all statements into a program block (multi-statement aware)
                prog_index = parse_all_statements(tokens, arena, diagnostic_sink)
            else
                ! Just declarations - still creates implicit main program
                prog_index = parse_all_statements(tokens, arena, diagnostic_sink)
            end if
        else
            ! No meaningful content - create empty program
            prog_index = push_program(arena, "main", [integer ::], 1, 1)
        end if

        ! Extract parser errors if requested (implicit main can have parse errors too)
        if (present(error_msg)) then
            ! Invalid statement labels are hard errors even when a diagnostic
            ! sink collects them, so report them through error_msg as well.
            label_error = get_statement_label_error()
            if (len_trim(label_error) > 0) then
                error_msg = label_error
                return
            end if
            if (present(diagnostic_sink)) return
            errors = get_last_parser_errors()
            if (len_trim(errors) > 0) then
                error_msg = errors
            end if
        end if
    end function parse_implicit_main_program

    ! Check if tokens contain non-comment content
    function has_any_non_comment_content(tokens) result(has_content)
        type(token_t), intent(in) :: tokens(:)
        logical :: has_content
        integer :: i

        has_content = .false.
        do i = 1, size(tokens)
            if (tokens(i)%kind /= TK_COMMENT .and. tokens(i)%kind /= TK_EOF .and. &
                tokens(i)%kind /= TK_NEWLINE) then
                has_content = .true.
                exit
            end if
        end do
    end function has_any_non_comment_content

    ! Check if tokens contain executable statements
    function has_executable_statements(tokens) result(has_executable_content)
        type(token_t), intent(in) :: tokens(:)
        logical :: has_executable_content
        integer :: i
        character(len=:), allocatable :: lowered

        has_executable_content = .false.
        do i = 1, size(tokens)
            if (tokens(i)%kind == TK_IDENTIFIER .or. tokens(i)%kind == TK_KEYWORD) then
                lowered = to_lower(trim(tokens(i)%text))
                if (lowered == "print" .or. lowered == "write" .or. &
                    lowered == "call" .or. lowered == "if" .or. &
                    lowered == "do" .or. lowered == "select" .or. &
                    lowered == "goto" .or. lowered == "stop" .or. &
                    lowered == "return") then
                    has_executable_content = .true.
                    exit
                end if
                if (tokens(i)%kind == TK_IDENTIFIER .and. i < size(tokens)) then
                    if (tokens(i + 1)%kind == TK_OPERATOR .and. &
                        tokens(i + 1)%text == "=") then
                        has_executable_content = .true.
                        exit
                    end if
                end if
            end if
        end do
    end function has_executable_statements

    ! Remove single-statement version to avoid shadowing the multi-statement parser

    ! Unit type detection functions
    function is_function_start(tokens, pos) result(is_start)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        logical :: is_start

        is_start = .false.
        if (pos > size(tokens)) return
        if (tokens(pos)%kind /= TK_KEYWORD) return
        if (to_lower(tokens(pos)%text) == "function") then
            is_start = .true.
            return
        end if
        is_start = function_follows_type_spec(tokens, pos)
    end function is_function_start

    function is_subroutine_start(tokens, pos) result(is_start)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        logical :: is_start

        is_start = .false.
        if (pos <= size(tokens)) then
            if (tokens(pos)%kind == TK_KEYWORD .and. &
                to_lower(tokens(pos)%text) == "subroutine") then
                is_start = .true.
            end if
        end if
    end function is_subroutine_start

    function is_module_start(tokens, pos) result(is_start)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        logical :: is_start

        is_start = .false.
        if (pos <= size(tokens)) then
            if (tokens(pos)%kind == TK_KEYWORD .and. &
                to_lower(tokens(pos)%text) == "module") then
                is_start = .true.
            end if
        end if
    end function is_module_start

    function is_template_start(tokens, pos) result(is_start)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        logical :: is_start

        is_start = .false.
        if (pos <= size(tokens)) then
            if (tokens(pos)%kind == TK_KEYWORD .and. &
                to_lower(tokens(pos)%text) == "template") then
                is_start = .true.
            end if
        end if
    end function is_template_start

    function is_submodule_start(tokens, pos) result(is_start)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        logical :: is_start

        is_start = .false.
        if (pos <= size(tokens)) then
            if (tokens(pos)%kind == TK_KEYWORD .and. &
                to_lower(tokens(pos)%text) == "submodule") then
                is_start = .true.
            end if
        end if
    end function is_submodule_start

    function is_program_start(tokens, pos) result(is_start)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        logical :: is_start

        is_start = .false.
        if (pos <= size(tokens)) then
            if (tokens(pos)%kind == TK_KEYWORD .and. to_lower(tokens(pos)%text) == &
                "program") then
                is_start = .true.
            end if
        end if
    end function is_program_start

    function is_interface_start(tokens, pos) result(is_start)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        logical :: is_start
        integer :: idx
        integer :: next_idx

        is_start = .false.
        idx = pos
        do while (idx <= size(tokens))
            select case (tokens(idx)%kind)
            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                idx = idx + 1
                cycle
            case (TK_NUMBER)
                idx = idx + 1
                cycle
            case default
                exit
            end select
        end do

        if (idx > size(tokens)) return
        if (tokens(idx)%kind /= TK_KEYWORD) return

        select case (to_lower(trim(tokens(idx)%text)))
        case ("interface")
            is_start = .true.
        case ("abstract")
            next_idx = idx + 1
            do while (next_idx <= size(tokens))
                select case (tokens(next_idx)%kind)
                case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                    next_idx = next_idx + 1
                    cycle
                case default
                    exit
                end select
            end do
            if (next_idx <= size(tokens)) then
                if (tokens(next_idx)%kind == TK_KEYWORD) then
                    if (to_lower(trim(tokens(next_idx)%text)) == "interface") then
                        is_start = .true.
                    end if
                end if
            end if
        end select
    end function is_interface_start

    function is_block_data_start(tokens, pos) result(is_start)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        logical :: is_start
        integer :: lookahead
        integer :: current_pos
        character(len=:), allocatable :: keyword_text

        is_start = .false.
        if (pos > size(tokens)) return

        current_pos = pos
        do while (current_pos <= size(tokens))
            select case (tokens(current_pos)%kind)
            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                current_pos = current_pos + 1
                cycle
            case (TK_NUMBER)
                current_pos = current_pos + 1
                do while (current_pos <= size(tokens))
                    select case (tokens(current_pos)%kind)
                    case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                        current_pos = current_pos + 1
                        cycle
                    case default
                        exit
                    end select
                end do
                exit
            case default
                exit
            end select
        end do

        if (current_pos > size(tokens)) return
        if (tokens(current_pos)%kind /= TK_KEYWORD) return
        if (to_lower(trim(tokens(current_pos)%text)) /= "block") return

        lookahead = current_pos + 1
        do while (lookahead <= size(tokens))
            select case (tokens(lookahead)%kind)
            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                lookahead = lookahead + 1
                cycle
            case (TK_KEYWORD, TK_IDENTIFIER)
                keyword_text = to_lower(trim(tokens(lookahead)%text))
                if (keyword_text == "data") is_start = .true.
                return
            case default
                return
            end select
        end do
    end function is_block_data_start

    pure integer function find_first_meaningful_token(tokens) result(pos)
        type(token_t), intent(in) :: tokens(:)
        integer :: i

        pos = 0
        do i = 1, size(tokens)
            select case (tokens(i)%kind)
            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                cycle
            case (TK_EOF)
                return
            case default
                pos = i
                return
            end select
        end do
    end function find_first_meaningful_token

end module frontend_program_units
