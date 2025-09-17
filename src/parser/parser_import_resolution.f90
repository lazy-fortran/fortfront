module parser_import_resolution_module
    ! Import resolution module for use, include statements
    use lexer_core
    use parser_state_module
    use ast_core
    use ast_factory, only: push_use_statement, push_include_statement, push_literal
    use ast_types, only: LITERAL_STRING
    use url_utilities, only: extract_module_from_url
    implicit none
    private

    public :: parse_use_statement, parse_include_statement, parse_identifier_list

contains

    ! Parse comma-separated list of identifiers (for only clause)
    subroutine parse_identifier_list(parser, identifier_list)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: identifier_list(:)
        character(len=:), allocatable :: temp_list(:)
        type(token_t) :: token
        integer :: count, capacity
        
        count = 0
        capacity = 4  ! Initial capacity
        allocate(character(len=64) :: temp_list(capacity))
        
        ! Parse first identifier
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            token = parser%consume()
            count = count + 1
            temp_list(count) = token%text
            
            ! Parse additional identifiers (comma-separated)
            do while (.not. parser%is_at_end())
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ",") then
                    token = parser%consume()  ! consume ','
                    token = parser%peek()
                    if (token%kind == TK_IDENTIFIER) then
                        token = parser%consume()
                        
                        ! Expand array if needed
                        if (count >= capacity) then
                            capacity = capacity * 2
                            block
                                character(len=64), allocatable :: new_temp_list(:)
                                integer :: i
                                allocate(new_temp_list(capacity))
                                do i = 1, count
                                    new_temp_list(i) = temp_list(i)
                                end do
                                deallocate(temp_list)
                                call move_alloc(new_temp_list, temp_list)
                            end block
                        end if
                        
                        count = count + 1
                        temp_list(count) = token%text
                    else
                        exit  ! Not an identifier after comma
                    end if
                else
                    exit  ! Not a comma, end of list
                end if
            end do
        end if
        
        ! Copy to final array with exact size
        if (count > 0) then
            allocate(character(len=64) :: identifier_list(count))
            identifier_list(1:count) = temp_list(1:count)
        else
            allocate(character(len=0) :: identifier_list(0))
        end if
        
        deallocate(temp_list)
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
        integer :: line, column

        ! Consume 'use' keyword
        token = parser%consume()
        line = token%line
        column = token%column

        ! Get module name (can be identifier or string for Go-style imports)
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            token = parser%consume()
            module_name = token%text
            url_spec = ""  ! No URL for regular imports
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
                    url_spec = url_spec(2:len(url_spec)-1)
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

        ! Check for optional only clause (simplified for refactoring)
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == ",") then
            token = parser%consume()  ! consume ','

            ! Check for 'only' keyword
            token = parser%peek()
            if (token%kind == TK_KEYWORD .and. token%text == "only") then
                token = parser%consume()  ! consume 'only'
                has_only = .true.

                ! Expect ':'
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ":") then
                    token = parser%consume()  ! consume ':'
                    ! Parse only list - collect identifiers
                    call parse_identifier_list(parser, only_list)
                    ! For now, no rename support (simplified)
                    allocate(character(len=0) :: rename_list(0))
                else
                    ! Malformed only clause
                    allocate(character(len=0) :: only_list(0))
                    allocate(character(len=0) :: rename_list(0))
                end if
            end if
        end if

        if (.not. has_only) then
            allocate(character(len=0) :: only_list(0))
            allocate(character(len=0) :: rename_list(0))
        end if

        ! Create use statement node
        stmt_index = push_use_statement(arena, module_name, only_list, rename_list, &
                                        has_only, line, column, url_spec=url_spec)
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