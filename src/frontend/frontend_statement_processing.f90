module frontend_statement_processing
    ! Statement-level parsing and processing functionality
    ! Handles parsing of all statements into a program structure

    use lexer_core, only: token_t, TK_EOF, TK_KEYWORD, TK_COMMENT, TK_NEWLINE, &
                          TK_OPERATOR, TK_IDENTIFIER, TK_WHITESPACE, to_lower
    use parser_dispatcher_module, only: parse_statement_dispatcher, &
                                        get_additional_indices, &
                                        clear_additional_indices
    use parser_prefix_buffer_module, only: parser_prefix_buffer_t
    use ast_arena_modern, only: ast_arena_t
    use frontend_statement_boundary, only: find_statement_boundary
    use frontend_program_structure, only: create_final_program_structure, &
                                          handle_multiple_program_units, &
                                          should_include_program_unit, &
                                          is_empty_main_program

    implicit none
    private

    ! Public statement processing interface
    public :: parse_all_statements, parse_explicit_program_unit
    public :: process_comment_statement, process_regular_statement

    ! Re-export from other modules
    public :: create_final_program_structure, handle_multiple_program_units
    public :: should_include_program_unit, is_empty_main_program
    public :: find_statement_boundary

contains

    ! Parse all statements into a program block
    function parse_all_statements(tokens, arena) result(prog_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer :: prog_index

        integer, allocatable :: body_indices(:)
        type(parser_prefix_buffer_t) :: prefix_buffer
        integer :: i, stmt_start, stmt_end, stmt_index, stmt_count
        integer :: merged_start, merged_end, look_ahead

        allocate (body_indices(0))
        stmt_count = 0
        i = 1

        ! Process all statements
        do while (i <= size(tokens))
            if (tokens(i)%kind == TK_EOF) exit

            call find_statement_boundary(tokens, i, stmt_start, stmt_end)

            if (is_prefix_only_statement(tokens, stmt_start, stmt_end)) then
                look_ahead = stmt_end + 1
                do while (look_ahead <= size(tokens))
                    if (tokens(look_ahead)%kind == TK_WHITESPACE .or. &
                        tokens(look_ahead)%kind == TK_NEWLINE) then
                        look_ahead = look_ahead + 1
                    else
                        exit
                    end if
                end do
                if (look_ahead <= size(tokens)) then
                    call find_statement_boundary(tokens, look_ahead, merged_start, &
                                                 merged_end)
                    if (merged_start == look_ahead) then
                        if (tokens(merged_start)%kind == TK_KEYWORD .and. &
                            (tokens(merged_start)%text == "function" .or. &
                             tokens(merged_start)%text == "subroutine")) then
                            stmt_end = merged_end
                        end if
                    end if
                end if
            end if

            if (tokens(stmt_start)%kind == TK_COMMENT) then
                call process_comment_statement(tokens, stmt_start, arena, &
                                               prefix_buffer, stmt_index, body_indices)
            else
                call process_regular_statement(tokens, stmt_start, stmt_end, arena, &
                                               prefix_buffer, stmt_index, body_indices)
            end if

            if (stmt_index > 0) stmt_count = stmt_count + 1
            i = stmt_end + 1
        end do

        ! Create final program structure
        call create_final_program_structure(arena, body_indices, stmt_count, &
                                            prog_index)
    end function parse_all_statements

    ! Process comment statement
    subroutine process_comment_statement(tokens, i, arena, prefix_buffer, stmt_index, &
                                         body_indices)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: i
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        integer, intent(out) :: stmt_index
        integer, allocatable, intent(inout) :: body_indices(:)
        type(token_t), allocatable, target :: stmt_tokens(:)

        ! Create tokens for comment parsing
        allocate (stmt_tokens(2))
        stmt_tokens(1) = tokens(i)
        ! Add EOF token
        stmt_tokens(2)%kind = TK_EOF
        stmt_tokens(2)%text = ""
        stmt_tokens(2)%line = tokens(i)%line
        stmt_tokens(2)%column = tokens(i)%column + len(tokens(i)%text)

        ! Parse the comment
        stmt_index = parse_statement_dispatcher(stmt_tokens, arena, prefix_buffer)
        if (stmt_index > 0) then
            body_indices = [body_indices, stmt_index]
        end if
    end subroutine process_comment_statement

    ! Process regular statement
    subroutine process_regular_statement(tokens, stmt_start, stmt_end, arena, &
                                         prefix_buffer, stmt_index, body_indices)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: stmt_start, stmt_end
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        integer, intent(out) :: stmt_index
        integer, allocatable, intent(inout) :: body_indices(:)
        type(token_t), allocatable, target :: stmt_tokens(:)

        ! Skip empty statements (can happen with consecutive semicolons)
        if (stmt_end < stmt_start) then
            stmt_index = 0
            return
        end if

        ! Heuristic: skip non-Fortran prefixes like "Simple test:" before real code (fixes #843)
        block
            integer :: i, colon_pos, eq_pos, local_start, paren_depth
            logical :: saw_keyword
            colon_pos = 0
            eq_pos = 0
            saw_keyword = .false.
            ! Local adjustable start to avoid modifying INTENT(IN) argument
            local_start = stmt_start
            paren_depth = 0

            ! Locate first '=' in the statement (assignment anchor)
            do i = local_start, stmt_end
                if (tokens(i)%kind == TK_KEYWORD) then
                    saw_keyword = .true.
                end if
                if (tokens(i)%kind == TK_OPERATOR .and. tokens(i)%text == "=") then
                    eq_pos = i
                    exit
                end if
            end do

            if (eq_pos > 0) then
                ! If there's a ':' before '=', and the prefix contains no Fortran
                ! keywords, treat everything up to and including ':' as a non-Fortran
                ! label and skip it. Do NOT do this when ':' is inside parentheses
                ! (e.g., array slices/substrings like a(1:3) or s(2:4)).
                do i = local_start, eq_pos - 1
                    if (tokens(i)%kind == TK_OPERATOR) then
                        select case (tokens(i)%text)
                        case ("(")
                            paren_depth = paren_depth + 1
                        case (")")
                            if (paren_depth > 0) paren_depth = paren_depth - 1
                        case (":")
                            if (paren_depth == 0) then
                                colon_pos = i
                                exit
                            end if
                        end select
                    end if
                end do
                if (colon_pos > 0 .and. .not. saw_keyword) then
                    local_start = colon_pos + 1
                end if
            end if

            ! Extract statement tokens (after any prefix adjustment)
            allocate (stmt_tokens(stmt_end - local_start + 2))
            stmt_tokens(1:stmt_end - local_start + 1) = tokens(local_start:stmt_end)
            ! Add EOF token
            stmt_tokens(stmt_end - local_start + 2)%kind = TK_EOF
            stmt_tokens(stmt_end - local_start + 2)%text = ""
            stmt_tokens(stmt_end - local_start + 2)%line = tokens(stmt_end)%line
            stmt_tokens(stmt_end - local_start + 2)%column = &
                tokens(stmt_end)%column + 1
        end block

        ! Note: stmt_tokens already allocated and filled in the block above

        ! Parse the statement
        stmt_index = parse_statement_dispatcher(stmt_tokens, arena, prefix_buffer)
        if (stmt_index > 0) then
            body_indices = [body_indices, stmt_index]

            ! Handle additional indices from multi-declaration parsing
            block
                integer, allocatable :: extra_indices(:)
                extra_indices = get_additional_indices()
                if (size(extra_indices) > 0) then
                    body_indices = [body_indices, extra_indices]
                end if
                call clear_additional_indices()
            end block
        end if
    end subroutine process_regular_statement

    ! Parse explicit program unit
    function parse_explicit_program_unit(tokens, arena) result(prog_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer :: prog_index
        type(parser_prefix_buffer_t) :: prefix_buffer

        ! Parse explicit program statement
        prog_index = parse_statement_dispatcher(tokens, arena, prefix_buffer)
    end function parse_explicit_program_unit

    logical function is_prefix_only_statement(tokens, start_idx, end_idx) &
        result(is_prefix)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_idx, end_idx
        integer :: idx
        character(len=:), allocatable :: lowered

        is_prefix = .false.
        if (start_idx < 1 .or. end_idx < start_idx) return

        do idx = start_idx, end_idx
            select case (tokens(idx)%kind)
            case (TK_WHITESPACE, TK_NEWLINE)
                cycle
            case (TK_IDENTIFIER)
                lowered = to_lower(tokens(idx)%text)
                select case (trim(lowered))
                case ('elemental', 'pure', 'impure', 'recursive', 'module', &
                      'nonrecursive', 'non_recursive')
                    is_prefix = .true.
                case default
                    is_prefix = .false.
                    return
                end select
            case default
                is_prefix = .false.
                return
            end select
        end do
    end function is_prefix_only_statement

end module frontend_statement_processing
