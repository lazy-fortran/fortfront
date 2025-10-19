module parser_procedure_definitions_module
    use string_utils_mod, only: to_lower
    ! Parser module for function, subroutine, and interface definitions
    use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, TK_NUMBER, &
                          TK_STRING, TK_OPERATOR, TK_KEYWORD, TK_NEWLINE, TK_COMMENT, &
                          TK_WHITESPACE
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_parameter_handling_module, only: parse_typed_parameters, &
                                                merge_parameter_attributes
    use parser_statement_utilities_module, only: parse_statement_in_if_block
    use parser_expressions_module, only: parse_comparison
    use parser_prefix_buffer_module, only: parser_prefix_buffer_t
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_function_def, push_subroutine_def, &
                           push_interface_block, &
                           push_module_procedure, push_if
    use ast_factory
    use ast_base, only: string_t
    implicit none
    private

    public :: parse_function_definition, parse_subroutine_definition, &
              parse_interface_block

contains

    subroutine parse_function_prefix_keywords(parser, prefix_buffer, prefix_list, &
                                              prefix_keywords, has_recursive_keyword)
        type(parser_state_t), intent(inout) :: parser
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        character(len=16), intent(in), optional :: prefix_list(:)
        character(len=16), allocatable, intent(out) :: prefix_keywords(:)
        logical, intent(out) :: has_recursive_keyword

        character(len=16), allocatable :: pending_prefixes(:)

        has_recursive_keyword = .false.
        allocate (character(len=16) :: prefix_keywords(0))

        call initialise_function_prefix_sources(prefix_buffer, prefix_list, &
                                                pending_prefixes)
        call append_pending_prefixes(pending_prefixes, prefix_keywords, &
                                     has_recursive_keyword)
        call consume_function_prefix_tokens(parser, prefix_keywords, &
                                            has_recursive_keyword)
    end subroutine parse_function_prefix_keywords

    subroutine initialise_function_prefix_sources(prefix_buffer, prefix_list, &
                                                  pending_prefixes)
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        character(len=16), intent(in), optional :: prefix_list(:)
        character(len=16), allocatable, intent(out) :: pending_prefixes(:)

        if (present(prefix_list)) then
            if (size(prefix_list) > 0) then
                allocate (character(len=16) :: pending_prefixes(size(prefix_list)))
                pending_prefixes = prefix_list
            else
                allocate (character(len=16) :: pending_prefixes(0))
            end if
            call prefix_buffer%clear()
        else
            call prefix_buffer%consume(pending_prefixes)
            if (.not. allocated(pending_prefixes)) then
                allocate (character(len=16) :: pending_prefixes(0))
            end if
        end if
    end subroutine initialise_function_prefix_sources

    subroutine append_pending_prefixes(pending_prefixes, prefix_keywords, &
                                       has_recursive_keyword)
        character(len=16), intent(in) :: pending_prefixes(:)
        character(len=16), allocatable, intent(inout) :: prefix_keywords(:)
        logical, intent(inout) :: has_recursive_keyword
        integer :: i

        do i = 1, size(pending_prefixes)
            call append_prefix_keyword(prefix_keywords, pending_prefixes(i))
            if (trim(pending_prefixes(i)) == "recursive") then
                has_recursive_keyword = .true.
            end if
        end do
    end subroutine append_pending_prefixes

    subroutine consume_function_prefix_tokens(parser, prefix_keywords, &
                                              has_recursive_keyword)
        type(parser_state_t), intent(inout) :: parser
        character(len=16), allocatable, intent(inout) :: prefix_keywords(:)
        logical, intent(inout) :: has_recursive_keyword

        type(token_t) :: token
        character(len=:), allocatable :: lowered_text

        do
            token = parser%peek()
            if (.not. (token%kind == TK_KEYWORD .or. token%kind == TK_IDENTIFIER)) then
                exit
            end if

            lowered_text = to_lower(token%text)
            select case (trim(lowered_text))
            case ("recursive")
                has_recursive_keyword = .true.
                call append_prefix_keyword(prefix_keywords, "recursive")
                token = parser%consume()
            case ("pure")
                call append_prefix_keyword(prefix_keywords, "pure")
                token = parser%consume()
            case ("elemental")
                call append_prefix_keyword(prefix_keywords, "elemental")
                token = parser%consume()
            case default
                exit
            end select
        end do
    end subroutine consume_function_prefix_tokens

    subroutine parse_function_signature(parser, return_type_str, function_name, &
                                        line, column, is_valid)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: return_type_str, function_name
        integer, intent(out) :: line, column
        logical, intent(out) :: is_valid

        type(token_t) :: token

        return_type_str = ""
        is_valid = .true.

        token = parser%peek()
        if (token%kind == TK_KEYWORD) then
            select case (trim(to_lower(token%text)))
            case ("real", "integer", "logical", "character")
                return_type_str = token%text
                token = parser%consume()
            end select
        end if

        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. token%text == "function") then
            line = token%line
            column = token%column
            token = parser%consume()
        else
            is_valid = .false.
            return
        end if

        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            function_name = token%text
            token = parser%consume()
        else if (token%kind == TK_KEYWORD .and. &
                 keyword_can_be_function_name(parser, token)) then
            function_name = token%text
            token = parser%consume()
        else
            function_name = "unnamed_function"
        end if
    end subroutine parse_function_signature

    subroutine parse_function_result_clause(parser, result_variable_name)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: result_variable_name

        type(token_t) :: token

        result_variable_name = ""
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER .and. token%text == "result") then
            token = parser%consume()
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "(") then
                token = parser%consume()
                token = parser%peek()
                if (token%kind == TK_IDENTIFIER) then
                    result_variable_name = token%text
                    token = parser%consume()
                end if
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ")") then
                    token = parser%consume()
                end if
            end if
        end if
    end subroutine parse_function_result_clause

    function parse_function_definition(parser, arena, prefix_buffer, prefix_list) &
        result(func_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        character(len=16), intent(in), optional :: prefix_list(:)
        integer :: func_index

        character(len=:), allocatable :: function_name, return_type_str, &
                                         result_variable_name
        integer :: line, column
        integer, allocatable :: param_indices(:), body_indices(:)
        logical :: has_recursive_keyword, is_valid
        logical :: infer_recursive_from_body
        character(len=16), allocatable :: prefix_keywords(:)

        infer_recursive_from_body = .false.

        call parse_function_prefix_keywords(parser, prefix_buffer, prefix_list, &
                                            prefix_keywords, has_recursive_keyword)

        call parse_function_signature(parser, return_type_str, function_name, &
                                      line, column, is_valid)
        if (.not. is_valid) then
            func_index = 0
            return
        end if

        call parse_parameter_list(parser, arena, param_indices)
        call parse_function_result_clause(parser, result_variable_name)
        call parse_procedure_body(parser, arena, function_name, "function", &
                                  body_indices, infer_recursive_from_body)

        call merge_parameter_attributes_if_needed(arena, param_indices, &
                                                  body_indices)
        call ensure_recursive_prefix(has_recursive_keyword, &
                                     infer_recursive_from_body, prefix_keywords)

        func_index = push_function_def(arena, function_name, param_indices, &
                                       return_type_str, body_indices, &
                                       line, column, &
                                       result_variable=result_variable_name, &
                                       is_recursive=has_recursive_keyword, &
                                       prefix_keywords=prefix_keywords)
    end function parse_function_definition

    function parse_if_statement_tokens(stmt_tokens, arena) result(if_index)
        type(token_t), intent(in) :: stmt_tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer :: if_index
        integer :: token_count
        integer :: then_pos, else_pos, end_pos
        integer :: condition_index
        integer, allocatable :: then_body_indices(:), else_body_indices(:)

        token_count = size(stmt_tokens)
        if (token_count <= 1) then
            if_index = 0
            return
        end if
        token_count = token_count - 1

        call locate_if_statement_sections(stmt_tokens, token_count, then_pos, &
                                          else_pos, end_pos)
        if (then_pos < 0 .or. end_pos < 0) then
            if_index = 0
            return
        end if

        condition_index = build_if_condition(stmt_tokens, then_pos, arena)
        call parse_then_branch(stmt_tokens, then_pos, else_pos, end_pos, arena, &
                               then_body_indices)
        call parse_else_branch(stmt_tokens, else_pos, end_pos, arena, &
                               else_body_indices)

        if_index = push_if(arena, condition_index, then_body_indices, &
                           else_body_indices=else_body_indices, &
                           line=stmt_tokens(1)%line, column=stmt_tokens(1)%column)
    end function parse_if_statement_tokens

    subroutine locate_if_statement_sections(stmt_tokens, token_count, then_pos, &
                                            else_pos, end_pos)
        type(token_t), intent(in) :: stmt_tokens(:)
        integer, intent(in) :: token_count
        integer, intent(out) :: then_pos, else_pos, end_pos

        integer :: i
        character(len=:), allocatable :: lowered_text

        then_pos = -1
        else_pos = -1
        end_pos = -1

        do i = 2, token_count
            if (stmt_tokens(i)%kind /= TK_KEYWORD) cycle
            lowered_text = to_lower(stmt_tokens(i)%text)
            select case (trim(lowered_text))
            case ("then")
                if (then_pos < 0) then_pos = i
            case ("else")
                if (else_pos < 0) else_pos = i
            case ("end")
                if (i < token_count) then
                    if (stmt_tokens(i + 1)%kind == TK_KEYWORD) then
                        lowered_text = to_lower(stmt_tokens(i + 1)%text)
                        if (trim(lowered_text) == "if") then
                            end_pos = i
                            exit
                        end if
                    end if
                end if
            end select
        end do
    end subroutine locate_if_statement_sections

    integer function build_if_condition(stmt_tokens, then_pos, arena) &
        result(condition_index)
        type(token_t), intent(in) :: stmt_tokens(:)
        integer, intent(in) :: then_pos
        type(ast_arena_t), intent(inout) :: arena

        integer :: condition_length
        type(token_t), allocatable, target :: condition_tokens(:)
        type(parser_state_t) :: condition_parser

        condition_index = 0
        condition_length = then_pos - 2
        if (condition_length < 1) return

        allocate (condition_tokens(condition_length + 1))
        condition_tokens(1:condition_length) = stmt_tokens(2:then_pos - 1)
        condition_tokens(condition_length + 1)%kind = TK_EOF
        condition_tokens(condition_length + 1)%text = ""
        condition_tokens(condition_length + 1)%line = stmt_tokens(2)%line
        condition_tokens(condition_length + 1)%column = stmt_tokens(2)%column

        condition_parser = create_parser_state(condition_tokens)
        condition_index = parse_comparison(condition_parser, arena)
        deallocate (condition_tokens)
    end function build_if_condition

    subroutine parse_then_branch(stmt_tokens, then_pos, else_pos, end_pos, arena, &
                                 then_body_indices)
        type(token_t), intent(in) :: stmt_tokens(:)
        integer, intent(in) :: then_pos, else_pos, end_pos
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(out) :: then_body_indices(:)

        integer :: then_start, then_end

        then_start = then_pos + 1
        if (else_pos > 0) then
            then_end = else_pos - 1
        else
            then_end = end_pos - 1
        end if

        then_body_indices = parse_if_body_tokens(stmt_tokens, then_start, &
                                                 then_end, arena)
    end subroutine parse_then_branch

    subroutine parse_else_branch(stmt_tokens, else_pos, end_pos, arena, &
                                 else_body_indices)
        type(token_t), intent(in) :: stmt_tokens(:)
        integer, intent(in) :: else_pos, end_pos
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(out) :: else_body_indices(:)

        integer :: else_start, else_end

        if (else_pos <= 0) then
            allocate (else_body_indices(0))
            return
        end if

        else_start = else_pos + 1
        else_end = end_pos - 1
        else_body_indices = parse_if_body_tokens(stmt_tokens, else_start, &
                                                 else_end, arena)
    end subroutine parse_else_branch

    function parse_if_body_tokens(stmt_tokens, start_idx, end_idx, arena) &
        result(body_indices)
        type(token_t), intent(in) :: stmt_tokens(:)
        integer, intent(in) :: start_idx, end_idx
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable :: body_indices(:)

        type(token_t), allocatable, target :: body_tokens(:)

        if (end_idx < start_idx) then
            allocate (body_indices(0))
            return
        end if

        call allocate_if_body_tokens(stmt_tokens, start_idx, end_idx, body_tokens)
        call parse_if_body_statements(body_tokens, arena, body_indices)

        if (allocated(body_tokens)) deallocate (body_tokens)
    end function parse_if_body_tokens

    subroutine allocate_if_body_tokens(stmt_tokens, start_idx, end_idx, body_tokens)
        type(token_t), intent(in) :: stmt_tokens(:)
        integer, intent(in) :: start_idx, end_idx
        type(token_t), allocatable, intent(out), target :: body_tokens(:)

        integer :: body_len

        body_len = end_idx - start_idx + 1
        if (body_len <= 0) then
            allocate (body_tokens(0))
            return
        end if

        allocate (body_tokens(body_len + 1))
        body_tokens(1:body_len) = stmt_tokens(start_idx:end_idx)
        body_tokens(body_len + 1)%kind = TK_EOF
        body_tokens(body_len + 1)%text = ""
        body_tokens(body_len + 1)%line = stmt_tokens(start_idx)%line
        body_tokens(body_len + 1)%column = stmt_tokens(start_idx)%column
    end subroutine allocate_if_body_tokens

    subroutine parse_if_body_statements(body_tokens, arena, body_indices)
        type(token_t), intent(in) :: body_tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(out) :: body_indices(:)

        type(parser_state_t) :: body_parser
        integer :: stmt_start, stmt_end

        if (size(body_tokens) == 0) then
            allocate (body_indices(0))
            return
        end if

        body_parser = create_parser_state(body_tokens)
        allocate (body_indices(0))

        do while (.not. body_parser%is_at_end())
            call skip_if_body_padding(body_parser)
            if (body_parser%is_at_end()) exit
            stmt_start = body_parser%current_token
            stmt_end = find_if_body_line_end(body_tokens, stmt_start)
            call parse_if_body_line(body_tokens, stmt_start, stmt_end, arena, &
                                    body_indices)
            body_parser%current_token = stmt_end + 1
        end do
    end subroutine parse_if_body_statements

    subroutine skip_if_body_padding(body_parser)
        type(parser_state_t), intent(inout) :: body_parser
        type(token_t) :: token

        do while (.not. body_parser%is_at_end())
            token = body_parser%peek()
            if (token%kind == TK_NEWLINE .or. token%kind == TK_WHITESPACE) then
                token = body_parser%consume()
            else
                exit
            end if
        end do
    end subroutine skip_if_body_padding

    integer function find_if_body_line_end(body_tokens, stmt_start) result(stmt_end)
        type(token_t), intent(in) :: body_tokens(:)
        integer, intent(in) :: stmt_start

        integer :: i
        type(token_t) :: first_token

        stmt_end = stmt_start
        if (stmt_start > size(body_tokens)) return

        first_token = body_tokens(stmt_start)
        do i = stmt_start, size(body_tokens)
            if (body_tokens(i)%kind == TK_EOF) exit
            if (i > stmt_start) then
                if (body_tokens(i)%line /= first_token%line) exit
            end if
            stmt_end = i
        end do
    end function find_if_body_line_end

    subroutine parse_if_body_line(body_tokens, stmt_start, stmt_end, arena, &
                                  body_indices)
        type(token_t), intent(in) :: body_tokens(:)
        integer, intent(in) :: stmt_start, stmt_end
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(inout) :: body_indices(:)

        type(token_t), allocatable, target :: line_tokens(:)
        type(parser_state_t) :: line_parser
        integer :: stmt_size
        integer :: stmt_index
        type(token_t) :: consumed_token

        stmt_size = stmt_end - stmt_start + 1
        if (stmt_size <= 0) return

        allocate (line_tokens(stmt_size + 1))
        line_tokens(1:stmt_size) = body_tokens(stmt_start:stmt_end)
        line_tokens(stmt_size + 1)%kind = TK_EOF
        line_tokens(stmt_size + 1)%text = ""
        line_tokens(stmt_size + 1)%line = body_tokens(stmt_start)%line
        line_tokens(stmt_size + 1)%column = body_tokens(stmt_start)%column

        line_parser = create_parser_state(line_tokens)
        call skip_if_body_line_padding(line_parser)

        if (.not. line_parser%is_at_end()) then
            stmt_index = parse_statement_in_if_block(line_parser, arena, &
                                                     line_parser%peek())
            if (stmt_index > 0) then
                body_indices = [body_indices, stmt_index]
            end if
        end if

        deallocate (line_tokens)
    end subroutine parse_if_body_line

    subroutine skip_if_body_line_padding(line_parser)
        type(parser_state_t), intent(inout) :: line_parser
        type(token_t) :: token

        do while (.not. line_parser%is_at_end())
            token = line_parser%peek()
            if (token%kind == TK_WHITESPACE .or. token%kind == TK_NEWLINE) then
                token = line_parser%consume()
            else
                exit
            end if
        end do
    end subroutine skip_if_body_line_padding

    subroutine append_prefix_keyword(prefixes, value)
        character(len=16), allocatable, intent(inout) :: prefixes(:)
        character(len=*), intent(in) :: value
        integer :: n, i
        character(len=16), allocatable :: temp(:)
        logical :: already_present

        already_present = .false.
        if (allocated(prefixes)) then
            do i = 1, size(prefixes)
                if (trim(prefixes(i)) == trim(value)) then
                    already_present = .true.
                    exit
                end if
            end do
        else
            allocate (character(len=16) :: prefixes(0))
        end if

        if (already_present) return

        n = size(prefixes)
        allocate (character(len=16) :: temp(n + 1))
        if (n > 0) temp(1:n) = prefixes
        temp(n + 1) = trim(value)
        call move_alloc(temp, prefixes)
    end subroutine append_prefix_keyword

    function parse_subroutine_definition(parser, arena, prefix_buffer) &
        result(sub_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        integer :: sub_index

        character(len=:), allocatable :: subroutine_name
        integer :: line, column
        integer, allocatable :: param_indices(:), body_indices(:)

        call parse_subroutine_header(parser, subroutine_name, line, column)
        call parse_parameter_list(parser, arena, param_indices)
        call parse_procedure_body(parser, arena, subroutine_name, "subroutine", &
                                  body_indices)

        call merge_parameter_attributes_if_needed(arena, param_indices, &
                                                  body_indices)

        sub_index = push_subroutine_def(arena, subroutine_name, param_indices, &
                                        body_indices, &
                                        line, column)
    end function parse_subroutine_definition

    subroutine parse_parameter_list(parser, arena, param_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(out) :: param_indices(:)

        type(token_t) :: token

        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            token = parser%consume()
            call parse_typed_parameters(parser, arena, param_indices)
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()
            end if
        else
            allocate (param_indices(0))
        end if
    end subroutine parse_parameter_list

    subroutine merge_parameter_attributes_if_needed(arena, param_indices, &
                                                    body_indices)
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(inout) :: param_indices(:)
        integer, allocatable, intent(in) :: body_indices(:)

        if (.not. allocated(param_indices)) return
        if (.not. allocated(body_indices)) return
        if (size(param_indices) == 0) return
        if (size(body_indices) == 0) return

        call merge_parameter_attributes(arena, param_indices, body_indices)
    end subroutine merge_parameter_attributes_if_needed

    subroutine ensure_recursive_prefix(has_recursive_keyword, &
                                       infer_recursive_from_body, &
                                       prefix_keywords)
        logical, intent(inout) :: has_recursive_keyword
        logical, intent(in) :: infer_recursive_from_body
        character(len=16), allocatable, intent(inout) :: prefix_keywords(:)

        if (has_recursive_keyword) return
        if (.not. infer_recursive_from_body) return

        has_recursive_keyword = .true.
        call append_prefix_keyword(prefix_keywords, "recursive")
    end subroutine ensure_recursive_prefix

    subroutine parse_subroutine_header(parser, subroutine_name, line, column)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: subroutine_name
        integer, intent(out) :: line, column

        type(token_t) :: token

        token = parser%consume()
        line = token%line
        column = token%column

        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            subroutine_name = token%text
            token = parser%consume()
        else
            subroutine_name = "unnamed_subroutine"
        end if
    end subroutine parse_subroutine_header

    subroutine parse_procedure_body(parser, arena, procedure_name, end_keyword, &
                                    body_indices, infer_recursive_flag)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: end_keyword
        integer, allocatable, intent(out) :: body_indices(:)
        logical, intent(inout), optional :: infer_recursive_flag

        type(token_t) :: token
        integer :: stmt_index

        allocate (body_indices(0))

        do while (.not. parser%is_at_end())
            token = parser%peek()

            if (check_procedure_end(parser, token, end_keyword, procedure_name)) exit

            if (token%kind == TK_NEWLINE) then
                token = parser%consume()
                cycle
            end if

            call parse_body_statement(parser, arena, token, procedure_name, &
                                      infer_recursive_flag, stmt_index)

            if (stmt_index > 0) then
                body_indices = [body_indices, stmt_index]
            end if
        end do
    end subroutine parse_procedure_body

    logical function check_procedure_end(parser, first_token, end_keyword, &
                                         procedure_name) result(is_end)
        type(parser_state_t), intent(inout) :: parser
        type(token_t), intent(in) :: first_token
        character(len=*), intent(in) :: end_keyword
        character(len=*), intent(in) :: procedure_name
        type(token_t), allocatable, target :: all_tokens(:)
        integer :: next_idx
        type(token_t) :: token_local

        is_end = .false.
        if (first_token%kind /= TK_KEYWORD) return
        if (first_token%text /= "end") return

        if (associated(parser%tokens)) then
            allocate (all_tokens(size(parser%tokens)))
            all_tokens = parser%tokens
        else
            allocate (all_tokens(0))
        end if

        next_idx = parser%current_token + 1
        if (next_idx > size(all_tokens)) return

        if (all_tokens(next_idx)%kind == TK_KEYWORD .and. &
            all_tokens(next_idx)%text == end_keyword) then
            token_local = parser%consume()
            token_local = parser%consume()
            if (.not. parser%is_at_end()) then
                token_local = parser%peek()
                if (token_local%kind == TK_IDENTIFIER .and. &
                    token_local%text == procedure_name) then
                    token_local = parser%consume()
                end if
            end if
            is_end = .true.
        end if
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

        if (allocated(stmt_tokens)) deallocate (stmt_tokens)
    end subroutine parse_body_statement

    subroutine collect_statement_tokens(parser, first_token, stmt_tokens, stmt_size, &
                                        stmt_end)
        type(parser_state_t), intent(inout) :: parser
        type(token_t), intent(in) :: first_token
        type(token_t), allocatable, intent(out), target :: stmt_tokens(:)
        integer, intent(out) :: stmt_size
        integer, intent(out) :: stmt_end

        type(token_t), allocatable, target :: all_tokens(:)
        integer :: stmt_start

        call copy_parser_tokens(parser, all_tokens)

        stmt_start = parser%current_token
        if (is_if_statement_start(first_token)) then
            stmt_end = locate_if_statement_end(all_tokens, stmt_start)
        else
            stmt_end = locate_single_line_end(all_tokens, stmt_start, &
                                              first_token%line)
        end if

        stmt_size = stmt_end - stmt_start + 1
        if (stmt_size <= 0) then
            allocate (stmt_tokens(0))
            return
        end if

        call copy_statement_slice(all_tokens, stmt_start, stmt_end, first_token, &
                                  stmt_tokens)
    end subroutine collect_statement_tokens

    subroutine copy_parser_tokens(parser, tokens)
        type(parser_state_t), intent(in) :: parser
        type(token_t), allocatable, intent(out), target :: tokens(:)

        if (associated(parser%tokens)) then
            allocate (tokens(size(parser%tokens)))
            tokens = parser%tokens
        else
            allocate (tokens(0))
        end if
    end subroutine copy_parser_tokens

    logical function is_if_statement_start(first_token) result(is_if_start)
        type(token_t), intent(in) :: first_token

        is_if_start = first_token%kind == TK_KEYWORD
        if (is_if_start) is_if_start = first_token%text == "if"
    end function is_if_statement_start

    integer function locate_if_statement_end(all_tokens, stmt_start) result(stmt_end)
        type(token_t), intent(in) :: all_tokens(:)
        integer, intent(in) :: stmt_start

        integer :: pos
        integer :: depth
        logical :: preceded_by_end
        logical :: preceded_by_else

        stmt_end = stmt_start
        depth = 0
        pos = stmt_start

        do while (pos <= size(all_tokens))
            if (all_tokens(pos)%kind == TK_KEYWORD) then
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
            end if
            stmt_end = pos
            pos = pos + 1
        end do
    end function locate_if_statement_end

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

    function parse_interface_block(parser, arena, prefix_buffer) &
        result(interface_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        integer :: interface_index

        character(len=:), allocatable :: interface_name
        integer :: line, column
        integer, allocatable :: body_indices(:)
        type(token_t) :: token
        integer :: stmt_index

        call begin_interface_block(parser, interface_name, line, column)
        call prefix_buffer%clear()

        allocate (body_indices(0))

        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (handle_interface_end(parser, token)) exit
            if (process_interface_body_token(parser, arena, token, body_indices)) cycle

            call parser%error("Unexpected token '"//trim(token%text)// &
                              "' in interface block.")
            token = parser%consume()
        end do

        interface_index = push_interface_block(arena, interface_name, body_indices, &
                                               line, column)
    end function parse_interface_block

    subroutine begin_interface_block(parser, interface_name, line, column)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: interface_name
        integer, intent(out) :: line, column

        type(token_t) :: token

        token = parser%consume()
        line = token%line
        column = token%column

        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            token = parser%consume()
            interface_name = token%text
        else
            interface_name = ""
        end if
    end subroutine begin_interface_block

    logical function handle_interface_end(parser, first_token) result(is_end)
        type(parser_state_t), intent(inout) :: parser
        type(token_t), intent(in) :: first_token

        type(token_t) :: next_token
        character(len=:), allocatable :: lowered_text

        is_end = .false.
        if (first_token%kind /= TK_KEYWORD) return

        lowered_text = to_lower(first_token%text)
        if (trim(lowered_text) /= "end") return

        next_token = parser%get_token_at_index(parser%current_token + 1)
        if (next_token%kind /= TK_KEYWORD) return

        lowered_text = to_lower(next_token%text)
        if (trim(lowered_text) /= "interface") return

        next_token = parser%consume()
        next_token = parser%consume()

        next_token = parser%peek()
        if (next_token%kind == TK_IDENTIFIER) then
            next_token = parser%consume()
        end if

        is_end = .true.
    end function handle_interface_end

    logical function process_interface_body_token(parser, arena, token, &
                                                  body_indices) result(handled)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: token
        integer, allocatable, intent(inout) :: body_indices(:)

        integer :: stmt_index
        type(token_t) :: consumed_token
        character(len=:), allocatable :: lowered_text

        handled = .false.
        if (token%kind == TK_KEYWORD) then
            lowered_text = to_lower(token%text)
            if (trim(lowered_text) == "module") then
                stmt_index = parse_module_procedure_statement(parser, arena)
                if (stmt_index > 0) then
                    body_indices = [body_indices, stmt_index]
                end if
                handled = .true.
                return
            end if
        end if

        select case (token%kind)
        case (TK_NEWLINE, TK_COMMENT)
            consumed_token = parser%consume()
            handled = .true.
        end select
    end function process_interface_body_token

    function parse_module_procedure_statement(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index

        type(string_t), allocatable :: procedure_names(:)
        integer :: line, column

        stmt_index = 0
        allocate (procedure_names(0))

        if (.not. consume_module_procedure_header(parser, line, column)) return

        call collect_module_procedure_names(parser, procedure_names)

        if (size(procedure_names) > 0) then
            stmt_index = push_module_procedure(arena, procedure_names, line, column)
        end if
    end function parse_module_procedure_statement

    logical function consume_module_procedure_header(parser, line, column) &
        result(is_valid)
        type(parser_state_t), intent(inout) :: parser
        integer, intent(out) :: line, column

        type(token_t) :: token
        character(len=:), allocatable :: lowered_text

        token = parser%consume()
        line = token%line
        column = token%column

        token = parser%peek()
        if (token%kind /= TK_KEYWORD) then
            is_valid = .false.
            return
        end if

        lowered_text = to_lower(token%text)
        if (trim(lowered_text) /= "procedure") then
            is_valid = .false.
            return
        end if

        token = parser%consume()

        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "::") then
            token = parser%consume()
        end if

        is_valid = .true.
    end function consume_module_procedure_header

    subroutine collect_module_procedure_names(parser, procedure_names)
        type(parser_state_t), intent(inout) :: parser
        type(string_t), allocatable, intent(inout) :: procedure_names(:)

        type(token_t) :: token

        do while (.not. parser%is_at_end())
            token = parser%peek()
            select case (token%kind)
            case (TK_IDENTIFIER)
                call append_procedure_name(procedure_names, token%text)
                token = parser%consume()
            case (TK_OPERATOR)
                if (trim(token%text) == ",") then
                    token = parser%consume()
                else
                    call parser%error("Unexpected operator '"// &
                                      trim(token%text)// &
                                      "' in module procedure list.")
                    token = parser%consume()
                    exit
                end if
            case (TK_COMMENT, TK_NEWLINE, TK_KEYWORD)
                exit
            case (TK_WHITESPACE)
                token = parser%consume()
            case default
                call parser%error("Unexpected token '"//trim(token%text)// &
                                  "' in module procedure list.")
                token = parser%consume()
                exit
            end select
        end do
    end subroutine collect_module_procedure_names

    subroutine append_procedure_name(list, value)
        type(string_t), allocatable, intent(inout) :: list(:)
        character(len=*), intent(in) :: value
        type(string_t), allocatable :: tmp(:)
        integer :: n

        if (len_trim(value) == 0) return

        if (.not. allocated(list)) then
            allocate (list(1))
            list(1)%s = trim(value)
            return
        end if

        n = size(list)
        allocate (tmp(n + 1))
        if (n > 0) tmp(1:n) = list
        tmp(n + 1)%s = trim(value)
        call move_alloc(tmp, list)
    end subroutine append_procedure_name

    logical function keyword_can_be_function_name(parser, token) result(can_use)
        type(parser_state_t), intent(in) :: parser
        type(token_t), intent(in) :: token
        type(token_t) :: lookahead
        character(len=len(token%text)) :: token_lower
        character(len=:), allocatable :: next_lower
        integer :: next_index

        token_lower = to_lower(token%text)
        can_use = .false.

        select case (trim(token_lower))
        case ('double')
            next_index = parser%current_token + 1
            lookahead = parser%get_token_at_index(next_index)
            next_lower = to_lower(trim(lookahead%text))
            if (next_lower /= 'precision') then
                can_use = .true.
            end if
        case default
            can_use = .false.
        end select
    end function keyword_can_be_function_name

end module parser_procedure_definitions_module
