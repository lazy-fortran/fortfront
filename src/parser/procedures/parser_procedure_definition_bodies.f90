module parser_procedure_definition_bodies_module
    use, intrinsic :: iso_fortran_env, only: error_unit
    use string_utils_mod, only: to_lower
    use lexer_core, only: token_t, TK_KEYWORD, TK_IDENTIFIER, TK_NEWLINE, &
                          TK_WHITESPACE, TK_OPERATOR, TK_COMMENT, TK_EOF
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_if_statements_module, only: parse_if_statement_tokens
    use parser_do_constructs_module, only: parse_do_loop
    use parser_statement_utilities_module, only: parse_statement_in_if_block, &
                                                 parse_comment_or_directive
    use parser_prefix_buffer_module, only: parser_prefix_buffer_t, &
                                           append_prefix_token
    use parser_procedure_shared_module, only: consume_optional_kind_spec
    use ast_nodes_misc, only: contains_node
    use ast_arena_modern, only: ast_arena_t
    implicit none
    private

    public :: parse_procedure_body
    public :: reset_nested_internal_procedure_error
    public :: has_nested_internal_procedure_error
    public :: get_nested_internal_procedure_message

    abstract interface
        function parse_function_definition_iface(parser, arena, prefix_buffer, &
                                                 prefix_list) result(func_index)
            import :: parser_state_t, ast_arena_t, parser_prefix_buffer_t
            type(parser_state_t), intent(inout) :: parser
            type(ast_arena_t), intent(inout) :: arena
            type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
            character(len=16), intent(in), optional :: prefix_list(:)
            integer :: func_index
        end function parse_function_definition_iface

        function parse_subroutine_definition_iface(parser, arena, prefix_buffer) &
            result(sub_index)
            import :: parser_state_t, ast_arena_t, parser_prefix_buffer_t
            type(parser_state_t), intent(inout) :: parser
            type(ast_arena_t), intent(inout) :: arena
            type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
            integer :: sub_index
        end function parse_subroutine_definition_iface
    end interface

    logical :: nested_internal_procedure_error = .false.
    character(len=:), allocatable :: nested_internal_procedure_message

contains

    subroutine parse_procedure_body(parser, arena, procedure_name, end_keyword, &
                                    body_indices, infer_recursive_flag, &
                                    parse_function_proc, parse_subroutine_proc)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: end_keyword
        integer, allocatable, intent(out) :: body_indices(:)
        logical, intent(inout), optional :: infer_recursive_flag
        procedure(parse_function_definition_iface), optional :: parse_function_proc
        procedure(parse_subroutine_definition_iface), optional :: parse_subroutine_proc

        type(token_t) :: token
        character(len=:), allocatable :: lowered
        integer :: stmt_index

        allocate (body_indices(0))

        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_KEYWORD .or. token%kind == TK_IDENTIFIER) then
                lowered = to_lower(token%text)
            else
                lowered = ""
            end if

            if (check_procedure_end(parser, token, end_keyword, procedure_name)) exit

            if (token%kind == TK_NEWLINE) then
                token = parser%consume()
                cycle
            end if

            if (token%kind == TK_COMMENT) then
                stmt_index = parse_comment_or_directive(parser, arena, token)
                if (stmt_index > 0) then
                    body_indices = [body_indices, stmt_index]
                end if
                cycle
            end if

            if ((token%kind == TK_KEYWORD .or. token%kind == TK_IDENTIFIER) .and. &
                trim(lowered) == "contains") then
                ! Only treat lone "contains" statements as structural section markers.
                ! Any continuation (assignment, call, array reference, etc.) means the
                ! identifier should be preserved as regular code (issue #2247).
                block
                    integer :: lookahead_idx
                    type(token_t) :: lookahead
                    logical :: is_structural_contains
                    character(len=:), allocatable :: op_text

                    is_structural_contains = .true.
                    lookahead_idx = parser%current_token + 1

                    do while (lookahead_idx <= size(parser%tokens))
                        lookahead = parser%tokens(lookahead_idx)
                        select case (lookahead%kind)
                        case (TK_WHITESPACE)
                            lookahead_idx = lookahead_idx + 1
                            cycle
                        case (TK_NEWLINE, TK_EOF, TK_COMMENT)
                            exit
                        case (TK_OPERATOR)
                            op_text = trim(lookahead%text)
                            select case (op_text)
                            case (";")
                                exit
                            case ("&")
                                is_structural_contains = .false.
                                exit
                            case default
                                is_structural_contains = .false.
                                exit
                            end select
                        case default
                            is_structural_contains = .false.
                            exit
                        end select
                    end do

                    if (is_structural_contains) then
                        token = parser%consume()
                        call parse_contains_section(parser, arena, procedure_name, &
                                                    end_keyword, body_indices, &
                                                    parse_function_proc, &
                                                    parse_subroutine_proc)
                        exit
                    end if
                    ! Otherwise fall through and treat "contains" as an identifier
                end block
            end if

            ! Lazy fortran: detect nested internal procedures (not supported)
            if (token%kind == TK_KEYWORD .and. &
                (to_lower(token%text) == "function" .or. &
                 to_lower(token%text) == "subroutine")) then
                call record_nested_internal_procedure_error(token, procedure_name)
                call skip_nested_internal_procedure(parser)
                cycle
            end if

            call parse_body_statement(parser, arena, token, procedure_name, &
                                      infer_recursive_flag, stmt_index)

            if (stmt_index > 0) then
                body_indices = [body_indices, stmt_index]
            end if
        end do
    end subroutine parse_procedure_body

    subroutine reset_nested_internal_procedure_error()
        nested_internal_procedure_error = .false.
        if (allocated(nested_internal_procedure_message)) then
            deallocate (nested_internal_procedure_message)
        end if
    end subroutine reset_nested_internal_procedure_error

    logical function has_nested_internal_procedure_error()
        has_nested_internal_procedure_error = nested_internal_procedure_error
    end function has_nested_internal_procedure_error

    function get_nested_internal_procedure_message() result(message)
        character(len=:), allocatable :: message

        if (allocated(nested_internal_procedure_message)) then
            message = nested_internal_procedure_message
        else
            allocate (character(len=0) :: message)
        end if
    end function get_nested_internal_procedure_message

    subroutine record_nested_internal_procedure_error(token, procedure_name)
        type(token_t), intent(in) :: token
        character(len=*), intent(in) :: procedure_name
        character(len=32) :: line_str
        character(len=32) :: column_str
        character(len=:), allocatable :: message
        character(len=:), allocatable :: lowered_keyword

        write (line_str, '(I0)') token%line
        write (column_str, '(I0)') token%column
        lowered_keyword = to_lower(trim(token%text))

        call write_nested_procedure_error(lowered_keyword, procedure_name, &
                                          trim(line_str), trim(column_str))

        message = 'Nested internal procedures are not supported.' // &
                  new_line('a') // 'Found "' // trim(lowered_keyword) // &
                  '" inside procedure "' // trim(procedure_name) // '" at line ' // &
                  trim(line_str) // ', column ' // trim(column_str) // '.' // &
                  new_line('a') // 'Internal procedures cannot contain other ' // &
                  'internal procedures.' // new_line('a') // 'Move the inner ' // &
                  'procedure to the same contains section as the outer ' // &
                  'procedure.'

        if (.not. nested_internal_procedure_error) then
            nested_internal_procedure_message = message
            nested_internal_procedure_error = .true.
        end if
    end subroutine record_nested_internal_procedure_error

    subroutine write_nested_procedure_error(keyword, procedure_name, line_str, &
                                            column_str)
        character(len=*), intent(in) :: keyword
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: line_str
        character(len=*), intent(in) :: column_str

        write (error_unit, '(A)') &
            'Error: Nested internal procedures are not supported.'
        write (error_unit, '(A)') '  Found "' // trim(keyword) // '" inside "' // &
            trim(procedure_name) // '" at line ' // trim(line_str) // ', column ' // &
            trim(column_str) // '.'
        write (error_unit, '(A)') &
            '  Internal procedures cannot contain other internal procedures.'
        write (error_unit, '(A)') &
            '  Move the inner procedure to the same contains section as the ' // &
            'outer procedure.'
    end subroutine write_nested_procedure_error

    subroutine skip_nested_internal_procedure(parser)
        type(parser_state_t), intent(inout) :: parser
        integer :: depth
        type(token_t) :: token
        character(len=:), allocatable :: lowered

        token = parser%consume()
        if (token%kind /= TK_KEYWORD) return

        depth = 1
        do while (depth > 0 .and. .not. parser%is_at_end())
            token = parser%consume()
            if (token%kind /= TK_KEYWORD) cycle
            lowered = to_lower(token%text)

            select case (trim(lowered))
            case ("function", "subroutine")
                depth = depth + 1
            case ("end")
                call consume_end_procedure(parser, depth)
            end select
        end do
    end subroutine skip_nested_internal_procedure

    subroutine consume_end_procedure(parser, depth)
        type(parser_state_t), intent(inout) :: parser
        integer, intent(inout) :: depth
        type(token_t) :: next_token

        if (parser%is_at_end()) return

        next_token = parser%peek()
        if (next_token%kind == TK_KEYWORD .and. &
            (to_lower(next_token%text) == "function" .or. &
             to_lower(next_token%text) == "subroutine")) then
            next_token = parser%consume()
            depth = depth - 1
            call consume_optional_identifier_token(parser)
        end if
    end subroutine consume_end_procedure

    subroutine consume_optional_identifier_token(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        if (parser%is_at_end()) return

        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            token = parser%consume()
        end if
    end subroutine consume_optional_identifier_token

    logical function check_procedure_end(parser, first_token, end_keyword, &
                                         procedure_name) result(is_end)
        type(parser_state_t), intent(inout) :: parser
        type(token_t), intent(in) :: first_token
        character(len=*), intent(in) :: end_keyword
        character(len=*), intent(in) :: procedure_name
        type(token_t), allocatable, target :: all_tokens(:)
        integer :: next_idx
        type(token_t) :: token_local
        character(len=:), allocatable :: lowered_token
        character(len=:), allocatable :: combined_keyword

        is_end = .false.
        if (first_token%kind /= TK_KEYWORD) return

        lowered_token = to_lower(trim(first_token%text))
        combined_keyword = "end" // trim(end_keyword)

        if (lowered_token == combined_keyword) then
            token_local = parser%consume()
            call consume_optional_identifier_token(parser)
            is_end = .true.
            return
        end if

        if (lowered_token /= "end") return

        if (associated(parser%tokens)) then
            allocate (all_tokens(size(parser%tokens)))
            all_tokens = parser%tokens
        else
            allocate (all_tokens(0))
        end if

        next_idx = parser%current_token + 1
        if (next_idx > size(all_tokens)) then
            token_local = parser%consume()
            is_end = .true.
            return
        end if

        select case (all_tokens(next_idx)%kind)
        case (TK_KEYWORD)
            if (to_lower(trim(all_tokens(next_idx)%text)) == trim(end_keyword)) then
                token_local = parser%consume()
                token_local = parser%consume()
                if (.not. parser%is_at_end()) then
                    token_local = parser%peek()
                    if ((token_local%kind == TK_IDENTIFIER .or. &
                         token_local%kind == TK_KEYWORD) .and. &
                        token_local%text == procedure_name) then
                        token_local = parser%consume()
                    end if
                end if
                is_end = .true.
                return
            else
                return
            end if
        case (TK_NEWLINE, TK_COMMENT, TK_EOF)
            token_local = parser%consume()
            is_end = .true.
            return
        case default
            return
        end select
    end function check_procedure_end

    subroutine parse_body_statement(parser, arena, first_token, procedure_name, &
                                    infer_recursive_flag, stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: first_token
        character(len=*), intent(in) :: procedure_name
        logical, intent(inout), optional :: infer_recursive_flag
        integer, intent(out) :: stmt_index

        type(token_t), allocatable, target :: stmt_tokens(:)
        integer :: stmt_size
        integer :: stmt_end

        call collect_statement_tokens(parser, first_token, stmt_tokens, stmt_size, &
                                      stmt_end)

        if (stmt_size <= 0) then
            stmt_index = 0
            return
        end if

        call maybe_mark_recursive_from_body(stmt_tokens, stmt_size, procedure_name, &
                                            infer_recursive_flag)

        stmt_index = parse_body_statement_tokens(stmt_tokens, stmt_size, arena)

        parser%current_token = stmt_end + 1
    end subroutine parse_body_statement

    subroutine collect_statement_tokens(parser, first_token, stmt_tokens, stmt_size, &
                                        stmt_end)
        type(parser_state_t), intent(inout) :: parser
        type(token_t), intent(in) :: first_token
        type(token_t), allocatable, intent(out), target :: stmt_tokens(:)
        integer, intent(out) :: stmt_size
        integer, intent(out) :: stmt_end

        integer :: stmt_start

        ! Performance: work directly with parser%tokens instead of copying
        ! the entire token array for every statement
        stmt_start = parser%current_token
        if (is_if_statement_start(first_token)) then
            stmt_end = locate_block_statement_end(parser%tokens, stmt_start, &
                                                  first_token%text)
        else
            stmt_end = locate_single_line_end(parser%tokens, stmt_start, &
                                              first_token%line)
        end if

        stmt_size = stmt_end - stmt_start + 1
        if (stmt_size <= 0) then
            allocate (stmt_tokens(0))
            return
        end if

        call copy_statement_slice(parser%tokens, stmt_start, stmt_end, first_token, &
                                  stmt_tokens)
    end subroutine collect_statement_tokens

    logical function is_if_statement_start(first_token) result(is_if_start)
        type(token_t), intent(in) :: first_token

        is_if_start = first_token%kind == TK_KEYWORD
        if (is_if_start) then
            is_if_start = first_token%text == "if" .or. &
                          first_token%text == "do"
        end if
    end function is_if_statement_start

    integer function locate_block_statement_end(all_tokens, stmt_start, &
                                                stmt_type) result(stmt_end)
        type(token_t), intent(in) :: all_tokens(:)
        integer, intent(in) :: stmt_start
        character(len=*), intent(in) :: stmt_type

        integer :: pos
        integer :: depth
        logical :: preceded_by_end
        logical :: preceded_by_else

        stmt_end = stmt_start
        pos = stmt_start

        if (stmt_type == "if") then
            if (is_single_line_if_statement(all_tokens, stmt_start)) then
                stmt_end = locate_single_line_end(all_tokens, stmt_start, &
                                                  all_tokens(stmt_start)%line)
                return
            end if
        end if

        ! Set initial depth based on statement type
        if (stmt_type == "do") then
            depth = 1  ! We're already at the "do" keyword
            pos = stmt_start + 1  ! Skip the initial "do" token
        else
            depth = 0
            pos = stmt_start
        end if

        do while (pos <= size(all_tokens))
            if (all_tokens(pos)%kind == TK_KEYWORD) then
                select case (stmt_type)
                case ("if")
                    select case (all_tokens(pos)%text)
                    case ("if")
                        preceded_by_end = .false.
                        preceded_by_else = .false.
                        if (pos > 1) then
                            if (all_tokens(pos - 1)%kind == TK_KEYWORD) then
                                preceded_by_end = all_tokens(pos - 1)%text == "end"
                                preceded_by_else = all_tokens(pos - 1)%text == "else"
                            end if
                        end if
                        if (.not. preceded_by_end .and. .not. preceded_by_else) then
                            depth = depth + 1
                        end if
                    case ("end")
                        if (pos < size(all_tokens)) then
                            if (all_tokens(pos + 1)%kind == TK_KEYWORD) then
                                if (all_tokens(pos + 1)%text == "if") then
                                    depth = depth - 1
                                    if (depth <= 0) then
                                        stmt_end = min(size(all_tokens), pos + 1)
                                        return
                                    end if
                                end if
                            end if
                        end if
                    end select
                case ("do")
                    select case (all_tokens(pos)%text)
                    case ("do")
                        depth = depth + 1
                    case ("enddo", "end do")
                        depth = depth - 1
                        if (depth <= 0) then
                            stmt_end = min(size(all_tokens), pos)
                            return
                        end if
                    case ("end")
                        if (pos + 1 <= size(all_tokens)) then
                            if (all_tokens(pos + 1)%kind == TK_KEYWORD .and. &
                                all_tokens(pos + 1)%text == "do") then
                                depth = depth - 1
                                if (depth <= 0) then
                                    stmt_end = min(size(all_tokens), pos + 1)
                                    return
                                end if
                            end if
                        end if
                    end select
                end select
            end if
            stmt_end = pos
            pos = pos + 1
        end do
    end function locate_block_statement_end

    logical function is_single_line_if_statement(all_tokens, stmt_start) &
        result(is_single_line)
        type(token_t), intent(in) :: all_tokens(:)
        integer, intent(in) :: stmt_start

        integer :: pos
        integer :: paren_depth
        logical :: pending_continuation
        character(len=:), allocatable :: token_text

        is_single_line = .true.
        paren_depth = 0
        pending_continuation = .false.

        do pos = stmt_start + 1, size(all_tokens)
            select case (all_tokens(pos)%kind)
            case (TK_OPERATOR)
                token_text = all_tokens(pos)%text
                if (token_text == "(") then
                    paren_depth = paren_depth + 1
                else if (token_text == ")") then
                    paren_depth = max(0, paren_depth - 1)
                else if (paren_depth == 0) then
                    if (token_text == "&") then
                        pending_continuation = .true.
                    else
                        pending_continuation = .false.
                    end if
                end if
            case (TK_KEYWORD)
                if (paren_depth == 0) then
                    token_text = all_tokens(pos)%text
                    select case (token_text)
                    case ("then")
                        is_single_line = .false.
                        return
                    case ("if")
                        ! Allow ELSE IF detection later in main logic
                    case default
                        pending_continuation = .false.
                    end select
                end if
            case (TK_NEWLINE)
                if (paren_depth == 0) then
                    if (pending_continuation) then
                        pending_continuation = .false.
                    else
                        return
                    end if
                end if
            end select
        end do
    end function is_single_line_if_statement

    integer function locate_single_line_end(all_tokens, stmt_start, line_number) &
        result(stmt_end)
        type(token_t), intent(in) :: all_tokens(:)
        integer, intent(in) :: stmt_start
        integer, intent(in) :: line_number

        integer :: pos

        stmt_end = stmt_start
        do pos = stmt_start, size(all_tokens)
            if (pos > stmt_start) then
                if (all_tokens(pos)%line /= line_number) exit
            end if
            stmt_end = pos
        end do
    end function locate_single_line_end

    subroutine copy_statement_slice(all_tokens, stmt_start, stmt_end, first_token, &
                                    stmt_tokens)
        type(token_t), intent(in) :: all_tokens(:)
        integer, intent(in) :: stmt_start, stmt_end
        type(token_t), intent(in) :: first_token
        type(token_t), allocatable, intent(out), target :: stmt_tokens(:)

        integer :: stmt_size

        stmt_size = stmt_end - stmt_start + 1
        allocate (stmt_tokens(stmt_size + 1))
        stmt_tokens(1:stmt_size) = all_tokens(stmt_start:stmt_end)
        stmt_tokens(stmt_size + 1)%kind = TK_EOF
        stmt_tokens(stmt_size + 1)%text = ""
        stmt_tokens(stmt_size + 1)%line = first_token%line
        stmt_tokens(stmt_size + 1)%column = first_token%column + 1
    end subroutine copy_statement_slice

    subroutine maybe_mark_recursive_from_body(stmt_tokens, stmt_size, procedure_name, &
                                              infer_recursive_flag)
        type(token_t), intent(in) :: stmt_tokens(:)
        integer, intent(in) :: stmt_size
        character(len=*), intent(in) :: procedure_name
        logical, intent(inout), optional :: infer_recursive_flag
        integer :: i

        if (.not. present(infer_recursive_flag)) return
        if (infer_recursive_flag) return

        do i = 1, stmt_size
            if (stmt_tokens(i)%kind == TK_IDENTIFIER) then
                if (trim(stmt_tokens(i)%text) == trim(procedure_name)) then
                    if (i < stmt_size) then
                        if (stmt_tokens(i + 1)%kind == TK_OPERATOR .and. &
                            stmt_tokens(i + 1)%text == "(") then
                            infer_recursive_flag = .true.
                            return
                        end if
                    end if
                end if
            end if
        end do
    end subroutine maybe_mark_recursive_from_body

    integer function parse_body_statement_tokens(stmt_tokens, stmt_size, arena) &
        result(stmt_index)
        type(token_t), intent(in) :: stmt_tokens(:)
        integer, intent(in) :: stmt_size
        type(ast_arena_t), intent(inout) :: arena
        integer :: first_token
        character(len=:), allocatable :: token_lower
        type(parser_state_t) :: block_parser
        type(token_t) :: token

        stmt_index = 0
        first_token = 1
        do while (first_token <= stmt_size)
            select case (stmt_tokens(first_token)%kind)
            case (TK_WHITESPACE, TK_NEWLINE)
                first_token = first_token + 1
            case default
                exit
            end select
        end do

        if (first_token <= stmt_size) then
            if (stmt_tokens(first_token)%kind == TK_KEYWORD) then
                token_lower = to_lower(stmt_tokens(first_token)%text)
                if (trim(token_lower) == "if") then
                    stmt_index = parse_if_statement_tokens(stmt_tokens, arena)
                else if (trim(token_lower) == "do") then
                    stmt_index = parse_do_statement_tokens(stmt_tokens, arena)
                end if
            end if
        end if

        if (stmt_index <= 0) then
            block_parser = create_parser_state(stmt_tokens)
            do while (block_parser%current_token < first_token .and. &
                      .not. block_parser%is_at_end())
                token = block_parser%consume()
            end do
            if (.not. block_parser%is_at_end()) then
                stmt_index = parse_statement_in_if_block(block_parser, arena, &
                                                         stmt_tokens(max(1, &
                                                                         first_token)))
            else
                stmt_index = 0
            end if
        end if
    end function parse_body_statement_tokens

    integer function parse_do_statement_tokens(stmt_tokens, arena) result(do_index)
        type(token_t), intent(in) :: stmt_tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        type(parser_state_t) :: do_parser

        ! Create a parser state from the tokens
        do_parser = create_parser_state(stmt_tokens)

        ! Parse the DO loop using the existing DO loop parser
        do_index = parse_do_loop(do_parser, arena)
    end function parse_do_statement_tokens

    subroutine parse_contains_section(parser, arena, procedure_name, end_keyword, &
                                      body_indices, parse_function_proc, &
                                      parse_subroutine_proc)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: end_keyword
        integer, allocatable, intent(inout) :: body_indices(:)
        procedure(parse_function_definition_iface), optional :: parse_function_proc
        procedure(parse_subroutine_definition_iface), optional :: parse_subroutine_proc
        type(token_t) :: token
        type(parser_prefix_buffer_t) :: prefix_buffer
        type(contains_node) :: contains_stmt
        integer :: proc_index

        call prefix_buffer%clear()

        contains_stmt%line = 0
        contains_stmt%column = 0
        call arena%push(contains_stmt, "contains", 0)
        body_indices = [body_indices, arena%size]

        do while (.not. parser%is_at_end())
            token = parser%peek()

            if (check_procedure_end(parser, token, end_keyword, procedure_name)) exit

            if (token%kind == TK_NEWLINE) then
                token = parser%consume()
                cycle
            end if

            if (token%kind == TK_KEYWORD .and. &
                to_lower(token%text) == "function") then
                if (.not. present(parse_function_proc)) then
                    token = parser%consume()
                    cycle
                end if
                proc_index = parse_function_proc(parser, arena, prefix_buffer)
                if (proc_index > 0) then
                    body_indices = [body_indices, proc_index]
                end if
                cycle
            end if

            if (token%kind == TK_KEYWORD .and. &
                to_lower(token%text) == "subroutine") then
                if (.not. present(parse_subroutine_proc)) then
                    token = parser%consume()
                    cycle
                end if
                proc_index = parse_subroutine_proc(parser, arena, prefix_buffer)
                if (proc_index > 0) then
                    body_indices = [body_indices, proc_index]
                end if
                cycle
            end if

            if (token%kind == TK_KEYWORD .or. token%kind == TK_IDENTIFIER) then
                block
                    character(len=:), allocatable :: lowered, lookahead_lower, &
                                                     type_with_kind
                    character(len=16), allocatable :: stored(:)
                    type(token_t) :: lookahead
                    lowered = to_lower(token%text)
                    select case (trim(lowered))
                    case ("pure", "elemental", "impure", "recursive", &
                          "nonrecursive", "non_recursive", "module")
                        call prefix_buffer%get_all(stored)
                        call append_prefix_token(stored, trim(lowered))
                        call prefix_buffer%set(stored)
                        if (allocated(stored)) deallocate (stored)
                        token = parser%consume()
                        cycle
                    case ("integer", "real", "logical", "character", "complex", &
                          "double", "procedure")
                        call prefix_buffer%get_all(stored)
                        if (trim(lowered) == "double") then
                            lookahead = parser%get_token_at_index( &
                                        parser%current_token + 1)
                            lookahead_lower = to_lower(trim(lookahead%text))
                            select case (trim(lookahead_lower))
                            case ("precision", "complex")
                                type_with_kind = trim(token%text) // " " // &
                                    trim(lookahead%text)
                                token = parser%consume()
                                token = parser%consume()
                                call consume_optional_kind_spec(parser, type_with_kind)
                                call append_prefix_token(stored, type_with_kind)
                                call prefix_buffer%set(stored)
                                if (allocated(stored)) deallocate (stored)
                                cycle
                            end select
                        end if
                        type_with_kind = trim(token%text)
                        token = parser%consume()
                        call consume_optional_kind_spec(parser, type_with_kind)
                        call append_prefix_token(stored, type_with_kind)
                        call prefix_buffer%set(stored)
                        if (allocated(stored)) deallocate (stored)
                        cycle
                    end select
                end block
            end if

            token = parser%consume()
        end do
    end subroutine parse_contains_section

end module parser_procedure_definition_bodies_module
