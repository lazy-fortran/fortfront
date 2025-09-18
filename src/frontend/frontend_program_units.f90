module frontend_program_units
    ! Program unit detection and parsing functionality
    ! Handles module, function, subroutine, type, and program unit parsing

    use lexer_core, only: token_t, TK_EOF, TK_KEYWORD, TK_COMMENT, TK_NEWLINE, &
                           TK_OPERATOR, TK_IDENTIFIER, TK_NUMBER, TK_STRING, TK_UNKNOWN
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_definition_statements_module, only: parse_function_definition
    use parser_dispatcher_module, only: parse_statement_dispatcher
    use frontend_statement_processing, only: parse_all_statements => parse_all_statements
    use parser_declarations, only: parse_derived_type_def
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node
    use ast_factory, only: push_program

    implicit none
    private

    ! Public program unit parsing interface
    public :: parse_program_unit, parse_module_unit, parse_function_unit
    public :: parse_subroutine_unit, parse_type_unit, parse_explicit_program_unit
    public :: parse_implicit_main_program
    public :: not_meaningful_program_unit, has_any_non_comment_content, has_executable_statements

    ! Helper functions for unit detection
    public :: is_function_start, is_subroutine_start, is_module_start, is_program_start, is_type_start

contains

    ! Main program unit parsing dispatch
    function parse_program_unit(tokens, arena, has_explicit_program) result(unit_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        logical, intent(in) :: has_explicit_program
        integer :: unit_index

        ! Check for meaningful content first
        if (not_meaningful_program_unit(tokens)) then
            unit_index = 0
            return
        end if

        ! Determine unit type and parse accordingly
        if (is_function_start(tokens, 1)) then
            unit_index = parse_function_unit(tokens, arena)
        else if (is_subroutine_start(tokens, 1)) then
            unit_index = parse_subroutine_unit(tokens, arena)
        else if (is_module_start(tokens, 1)) then
            ! Parse the entire module with its content
            unit_index = parse_module_unit(tokens, arena)
        else if (is_program_start(tokens, 1)) then
            unit_index = parse_explicit_program_unit(tokens, arena)
        else if (is_type_start(tokens, 1)) then
            ! Type definitions should be parsed as structured constructs
            unit_index = parse_statement_dispatcher(tokens, arena)
        else
            ! For mixed module/main program files, we always need to check for implicit main
            unit_index = parse_implicit_main_program(tokens, arena, has_explicit_program)
        end if
    end function parse_program_unit

    ! Parse module unit with all its content
    function parse_module_unit(tokens, arena) result(unit_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer :: unit_index

        ! Parse the complete module including its content
        ! Module should be parsed with all its statements
        unit_index = parse_statement_dispatcher(tokens, arena)
    end function parse_module_unit

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
    function parse_function_unit(tokens, arena) result(unit_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer :: unit_index

        ! Multi-line function definition
        block
            type(parser_state_t) :: parser
            parser = create_parser_state(tokens)
            unit_index = parse_function_definition(parser, arena)
        end block
    end function parse_function_unit

    ! Parse subroutine unit
    function parse_subroutine_unit(tokens, arena) result(unit_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer :: unit_index

        ! Parse the subroutine using the dispatcher
        unit_index = parse_statement_dispatcher(tokens, arena)
    end function parse_subroutine_unit

    ! Parse type unit
    function parse_type_unit(tokens, arena) result(unit_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer :: unit_index

        ! Parse type definition using statement dispatcher (which handles parser creation)
        unit_index = parse_statement_dispatcher(tokens, arena)
    end function parse_type_unit

    ! Parse implicit main program
    function parse_implicit_main_program(tokens, arena, has_explicit_program) &
        result(prog_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        logical, intent(in) :: has_explicit_program
        integer :: prog_index

        ! Check if there's meaningful content that should become an implicit main
        if (has_any_non_comment_content(tokens)) then
            if (has_executable_statements(tokens)) then
                ! Parse all statements into a program block (multi-statement aware)
                prog_index = parse_all_statements(tokens, arena)
            else
                ! Just declarations - still creates implicit main program
                prog_index = parse_all_statements(tokens, arena)
            end if
        else
            ! No meaningful content - create empty program
            prog_index = push_program(arena, "main", [integer::], 1, 1)
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

        has_executable_content = .false.
        do i = 1, size(tokens)
            if (tokens(i)%kind == TK_IDENTIFIER .or. tokens(i)%kind == TK_KEYWORD) then
                ! Look for executable keywords or assignment-like patterns
                if (tokens(i)%text == "print" .or. tokens(i)%text == "write" .or. &
                    tokens(i)%text == "call" .or. tokens(i)%text == "if" .or. &
                    tokens(i)%text == "do" .or. tokens(i)%text == "select" .or. &
                    tokens(i)%text == "goto" .or. tokens(i)%text == "stop" .or. &
                    tokens(i)%text == "return") then
                    has_executable_content = .true.
                    exit
                end if
                ! Check for assignment (identifier followed by =)
                if (i < size(tokens) .and. tokens(i)%kind == TK_IDENTIFIER .and. &
                    tokens(i + 1)%kind == TK_OPERATOR .and. tokens(i + 1)%text == "=") then
                    has_executable_content = .true.
                    exit
                end if
            end if
        end do
    end function has_executable_statements

    ! Remove single-statement version to avoid shadowing the multi-statement parser

    ! Parse explicit program unit
    function parse_explicit_program_unit(tokens, arena) result(prog_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer :: prog_index

        ! Parse explicit program statement
        prog_index = parse_statement_dispatcher(tokens, arena)
    end function parse_explicit_program_unit

    ! Unit type detection functions
    function is_function_start(tokens, pos) result(is_start)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        logical :: is_start

        is_start = .false.
        if (pos <= size(tokens)) then
            if (tokens(pos)%kind == TK_KEYWORD .and. &
                (tokens(pos)%text == "function" .or. &
                 (pos < size(tokens) .and. tokens(pos + 1)%kind == TK_KEYWORD .and. &
                  tokens(pos + 1)%text == "function"))) then
                is_start = .true.
            end if
        end if
    end function is_function_start

    function is_subroutine_start(tokens, pos) result(is_start)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        logical :: is_start

        is_start = .false.
        if (pos <= size(tokens)) then
            if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "subroutine") then
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
            if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "module") then
                is_start = .true.
            end if
        end if
    end function is_module_start

    function is_program_start(tokens, pos) result(is_start)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        logical :: is_start

        is_start = .false.
        if (pos <= size(tokens)) then
            if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "program") then
                is_start = .true.
            end if
        end if
    end function is_program_start

    function is_type_start(tokens, pos) result(is_start)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        logical :: is_start

        is_start = .false.
        if (pos <= size(tokens)) then
            if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "type") then
                is_start = .true.
            end if
        end if
    end function is_type_start

end module frontend_program_units
