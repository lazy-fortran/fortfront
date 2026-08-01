module parser_import_resolution_module
    use string_utils_mod, only: to_lower
    ! Import resolution module for use, include statements
    use lexer_core, only: token_t, TK_IDENTIFIER, TK_STRING, &
        TK_OPERATOR, TK_KEYWORD, TK_NEWLINE, TK_COMMENT, &
        TK_WHITESPACE, TK_EOF
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_use_statement, push_include_statement, push_literal
    use ast_factory, only: push_use_statement, push_include_statement, push_literal
    use ast_types, only: LITERAL_STRING
    use string_types, only: string_t
    use url_utilities, only: extract_module_from_url
    use generic_spec_names, only: make_generic_spec
    implicit none
    private

    public :: parse_use_statement, parse_include_statement, parse_identifier_list

contains

    ! Parse comma-separated list of identifiers (for only clause)
    subroutine parse_identifier_list(parser, identifier_list, rename_list)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: identifier_list(:)
        character(len=:), allocatable, intent(out) :: rename_list(:)
        type(token_t) :: token
        character(len=:), allocatable :: local_name, remote_name
        type(string_t), allocatable :: only_temp(:)
        type(string_t), allocatable :: rename_temp(:)
        integer :: count, capacity
        integer :: rename_count, rename_capacity
        integer :: alloc_len
        integer :: i, max_len

        count = 0
        capacity = 0
        rename_count = 0
        rename_capacity = 0

        ! Parse first identifier
        call parse_single_identifier_with_rename(parser)

        ! Parse additional identifiers (comma-separated)
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume() ! consume ','
                call parse_single_identifier_with_rename(parser)
            else
                exit ! Not a comma, end of list
            end if
        end do

        ! Copy to final array with exact size
        if (count > 0) then
            max_len = 0
            do i = 1, count
                max_len = max(max_len, len_trim(only_temp(i)%s))
            end do
            max_len = max(1, max_len)
            allocate (character(len=max_len) :: identifier_list(count))
            do i = 1, count
                identifier_list(i) = trim(only_temp(i)%s)
            end do
        else
            allocate (character(len=0) :: identifier_list(0))
        end if

        if (rename_count > 0) then
            max_len = 0
            do i = 1, rename_count
                max_len = max(max_len, len_trim(rename_temp(i)%s))
            end do
            max_len = max(1, max_len)
            allocate (character(len=max_len) :: rename_list(rename_count))
            do i = 1, rename_count
                rename_list(i) = trim(rename_temp(i)%s)
            end do
        else
            allocate (character(len=0) :: rename_list(0))
        end if

    contains

        subroutine ensure_only_capacity(required)
            integer, intent(in) :: required
            type(string_t), allocatable :: new_array(:)
            integer :: new_capacity

            if (capacity >= required) return
            new_capacity = max(4, capacity)
            do while (new_capacity < required)
                new_capacity = max(4, new_capacity * 2)
            end do
            allocate (new_array(new_capacity))
            if (capacity > 0 .and. allocated(only_temp)) then
                new_array(1:count) = only_temp(1:count)
            end if
            call move_alloc(new_array, only_temp)
            capacity = new_capacity
        end subroutine ensure_only_capacity

        subroutine ensure_rename_capacity(required)
            integer, intent(in) :: required
            type(string_t), allocatable :: new_array(:)
            integer :: new_capacity

            if (rename_capacity >= required) return
            new_capacity = max(4, rename_capacity)
            do while (new_capacity < required)
                new_capacity = max(4, new_capacity * 2)
            end do
            allocate (new_array(new_capacity))
            if (rename_capacity > 0 .and. allocated(rename_temp)) then
                new_array(1:rename_count) = rename_temp(1:rename_count)
            end if
            call move_alloc(new_array, rename_temp)
            rename_capacity = new_capacity
        end subroutine ensure_rename_capacity

        subroutine append_only(value)
            character(len=*), intent(in) :: value
            call ensure_only_capacity(count + 1)
            count = count + 1
            only_temp(count) = string_t(value)
        end subroutine append_only

        subroutine append_rename_pair(local, remote)
            character(len=*), intent(in) :: local
            character(len=*), intent(in) :: remote
            call ensure_rename_capacity(rename_count + 2)
            rename_count = rename_count + 1
            rename_temp(rename_count) = string_t(local)
            rename_count = rename_count + 1
            rename_temp(rename_count) = string_t(remote)
        end subroutine append_rename_pair

        subroutine parse_single_identifier_with_rename(parser)
            type(parser_state_t), intent(inout) :: parser
            type(token_t) :: token
            character(len=:), allocatable :: local_name, remote_name

            token = parser%peek()
            if (parse_generic_spec_entry(parser)) return
            if (token%kind == TK_IDENTIFIER) then
                token = parser%consume()
                local_name = trimmed_or_empty(token%text)

                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == "=>") then
                    token = parser%consume() ! consume '=>'
                    token = parser%peek()
                    if (token%kind == TK_IDENTIFIER) then
                        token = parser%consume()
                        remote_name = trimmed_or_empty(token%text)
                        call append_rename_pair(local_name, remote_name)
                    else
                        ! Malformed rename - treat as simple identifier
                        call append_only(local_name)
                    end if
                else
                    call append_only(local_name)
                end if
            end if
        end subroutine parse_single_identifier_with_rename

        ! A USE ONLY list entry may be a generic spec: OPERATOR(op) or
        ! ASSIGNMENT(=). Store it in the canonical spec form so that the two
        ! spellings of a relational operator compare equal.
        logical function parse_generic_spec_entry(parser) result(handled)
            type(parser_state_t), intent(inout) :: parser
            type(token_t) :: token
            type(token_t) :: next_token
            character(len=:), allocatable :: spec_kind
            character(len=:), allocatable :: symbol
            character(len=:), allocatable :: spec
            character(len=:), allocatable :: target_spec

            handled = .false.
            token = parser%peek()
            if (token%kind /= TK_IDENTIFIER .and. token%kind /= TK_KEYWORD) return
            spec_kind = to_lower(trim(token%text))
            if (spec_kind /= "operator" .and. spec_kind /= "assignment") return
            next_token = parser%get_token_at_index(parser%current_token + 1)
            if (next_token%kind /= TK_OPERATOR) return
            if (trim(next_token%text) /= "(") return

            handled = .true.
            token = parser%consume() ! spec keyword
            token = parser%consume() ! '('
            token = parser%peek()
            symbol = trim(token%text)
            if (token%kind /= TK_EOF) token = parser%consume()
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. trim(token%text) == ")") then
                token = parser%consume()
            end if
            spec = make_generic_spec(spec_kind, symbol)

            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. trim(token%text) == "=>") then
                token = parser%consume()
                if (parse_generic_spec_target(parser, target_spec)) then
                    call append_rename_pair(spec, target_spec)
                    return
                end if
            end if
            call append_only(spec)
        end function parse_generic_spec_entry

        ! The target of a rename may itself be a generic spec, as in
        ! OPERATOR(.local.) => OPERATOR(.remote.).
        logical function parse_generic_spec_target(parser, spec) result(parsed)
            type(parser_state_t), intent(inout) :: parser
            character(len=:), allocatable, intent(out) :: spec
            type(token_t) :: token
            type(token_t) :: next_token
            character(len=:), allocatable :: spec_kind
            character(len=:), allocatable :: symbol

            parsed = .false.
            spec = ""
            token = parser%peek()
            if (token%kind /= TK_IDENTIFIER .and. token%kind /= TK_KEYWORD) return
            spec_kind = to_lower(trim(token%text))
            if (spec_kind /= "operator" .and. spec_kind /= "assignment") return
            next_token = parser%get_token_at_index(parser%current_token + 1)
            if (next_token%kind /= TK_OPERATOR) return
            if (trim(next_token%text) /= "(") return

            parsed = .true.
            token = parser%consume() ! spec keyword
            token = parser%consume() ! '('
            token = parser%peek()
            symbol = trim(token%text)
            if (token%kind /= TK_EOF) token = parser%consume()
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. trim(token%text) == ")") then
                token = parser%consume()
            end if
            spec = make_generic_spec(spec_kind, symbol)
        end function parse_generic_spec_target

        function trimmed_or_empty(text) result(clean)
            character(len=*), intent(in) :: text
            character(len=:), allocatable :: clean
            integer :: len_clean

            len_clean = len_trim(text)
            if (len_clean <= 0) len_clean = 1
            allocate (character(len=len_clean) :: clean)
            if (len_trim(text) > 0) then
                clean = trim(text)
            else
                clean = ""
            end if
        end function trimmed_or_empty

    end subroutine parse_identifier_list

    function parse_use_statement(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index
        type(token_t) :: token
        character(len=:), allocatable :: module_name
        character(len=:), allocatable :: url_spec
        character(len=:), allocatable :: only_list(:)
        character(len=:), allocatable :: rename_list(:)
        logical :: has_only, is_valid_url
        logical :: has_double_colon, intrinsic_nature, non_intrinsic_nature
        integer :: line, column

        ! Consume 'use' keyword
        token = parser%consume()
        line = token%line
        column = token%column

        has_double_colon = .false.
        intrinsic_nature = .false.
        non_intrinsic_nature = .false.

        ! Get module name (can be identifier or string for Go-style imports)
        token = parser%peek()

        ! Handle optional comma with module nature or double colon
        if (token%kind == TK_OPERATOR .and. token%text == ",") then
            token = parser%consume() ! consume ','
            token = parser%peek()
            if ((token%kind == TK_KEYWORD .or. token%kind == TK_IDENTIFIER)) then
                block
                    character(len=:), allocatable :: attr_text
                    attr_text = to_lower(token%text)
                    if (attr_text == "intrinsic") then
                        intrinsic_nature = .true.
                        token = parser%consume()
                    else if (attr_text == "non_intrinsic") then
                        non_intrinsic_nature = .true.
                        token = parser%consume()
                    end if
                end block
                token = parser%peek()
            end if
            if (token%kind == TK_OPERATOR .and. token%text == "::") then
                token = parser%consume()
                has_double_colon = .true.
            end if
            token = parser%peek()
        else if (token%kind == TK_OPERATOR .and. token%text == "::") then
            token = parser%consume()
            has_double_colon = .true.
            token = parser%peek()
        end if

        if (token%kind == TK_IDENTIFIER) then
            token = parser%consume()
            module_name = token%text
            if (allocated(url_spec)) then
                block
                    character(len=:), allocatable :: temp
                    call move_alloc(url_spec, temp)
                end block
            end if
        else if (token%kind == TK_STRING) then
            ! Go-style import with URL
            token = parser%consume()
            url_spec = token%text

            ! Remove quotes from the URL string
            if (len(url_spec) >= 2) then
                if ((url_spec(1:1) == '"' .and. &
                    url_spec(len(url_spec):len(url_spec)) == '"') .or. &
                    (url_spec(1:1) == "'" .and. &
                    url_spec(len(url_spec):len(url_spec)) == "'")) then
                    url_spec = url_spec(2:len(url_spec) - 1)
                end if
            end if

            ! Extract module name from URL
            call extract_module_from_url(url_spec, module_name, is_valid_url)

            if (.not. is_valid_url) then
                ! Invalid URL - return placeholder
                stmt_index = push_literal(arena, &
                    "! Invalid URL in use statement", &
                    LITERAL_STRING, token%line, token%column)
                return
            end if
        else
            ! Invalid use statement - return placeholder
            stmt_index = push_literal(arena, "! Invalid use statement", &
                LITERAL_STRING, token%line, token%column)
            return
        end if

        has_only = .false.

        ! Check for optional only clause or rename list
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == ",") then
            token = parser%consume() ! consume ','

            ! Check for 'only' keyword
            token = parser%peek()
            if ((token%kind == TK_KEYWORD .or. token%kind == TK_IDENTIFIER) .and. &
                trim(to_lower(token%text)) == "only") then
                token = parser%consume() ! consume 'only'
                has_only = .true.

                ! Expect ':'
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ":") then
                    token = parser%consume() ! consume ':'
                    ! Parse only list - collect identifiers
                    call parse_identifier_list(parser, only_list, rename_list)
                else
                    ! Malformed only clause
                    allocate (character(len=0) :: only_list(0))
                    allocate (character(len=0) :: rename_list(0))
                end if
            else
                ! No only keyword - parse rename list directly
                allocate (character(len=0) :: only_list(0))
                call parse_identifier_list(parser, only_list, rename_list)
            end if
        end if

        if (.not. has_only .and. .not. allocated(rename_list)) then
            allocate (character(len=0) :: only_list(0))
            allocate (character(len=0) :: rename_list(0))
        end if

        ! Create use statement node (conditionally include URL spec)
        if (allocated(url_spec)) then
            stmt_index = push_use_statement(arena, module_name, only_list, &
                rename_list, &
                has_only, line, column, &
                url_spec=url_spec, &
                has_double_colon=has_double_colon, &
                is_intrinsic=intrinsic_nature, &
                is_non_intrinsic=non_intrinsic_nature)
        else
            stmt_index = push_use_statement(arena, module_name, only_list, &
                rename_list, &
                has_only, line, column, &
                has_double_colon=has_double_colon, &
                is_intrinsic=intrinsic_nature, &
                is_non_intrinsic=non_intrinsic_nature)
        end if
    end function parse_use_statement

    function parse_include_statement(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index
        type(token_t) :: token
        character(len=:), allocatable :: filename
        integer :: line, column

        ! Consume 'include' keyword
        token = parser%consume()
        line = token%line
        column = token%column

        ! Get filename (must be string literal)
        token = parser%peek()
        if (token%kind == TK_STRING) then
            token = parser%consume()
            filename = token%text
        else
            filename = "! Invalid include statement"
        end if

        ! Create include statement node
        stmt_index = push_include_statement(arena, filename, line, column)
    end function parse_include_statement

end module parser_import_resolution_module
