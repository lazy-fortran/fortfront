module parser_import_statements_module
    ! Parser module for import/module statement types (use, implicit, include, module)
    use lexer_core
    use parser_state_module
    use ast_core
    use ast_factory
    use parser_declarations, only: parse_declaration
    use ast_types, only: LITERAL_STRING
    use url_utilities, only: extract_module_from_url
    implicit none
    private

    public :: parse_use_statement, parse_implicit_statement, parse_include_statement
    public :: parse_module

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

    ! Helper to count letter ranges for allocation
    subroutine count_letter_ranges(parser, count)
        type(parser_state_t), intent(inout) :: parser
        integer, intent(out) :: count
        type(token_t) :: token
        integer :: saved_pos
        
        count = 0
        saved_pos = parser%current_token
        
        do
            token = parser%peek()
            if (token%kind == TK_IDENTIFIER .and. len(token%text) == 1) then
                count = count + 1
                token = parser%consume()
                
                ! Check for range (a-h)
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == "-") then
                    token = parser%consume()  ! consume '-'
                    token = parser%peek()
                    if (token%kind == TK_IDENTIFIER .and. len(token%text) == 1) then
                        token = parser%consume()  ! consume end letter
                    end if
                end if
                
                ! Check for comma
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ",") then
                    token = parser%consume()  ! consume ','
                else
                    exit
                end if
            else
                exit
            end if
        end do
        
        ! Restore parser position
        parser%current_token = saved_pos
    end subroutine count_letter_ranges

    ! Helper to parse letter ranges into array
    subroutine parse_letter_ranges(parser, letter_ranges)
        type(parser_state_t), intent(inout) :: parser
        character(len=64), intent(out) :: letter_ranges(:)
        type(token_t) :: token
        integer :: i
        
        i = 1
        do while (i <= size(letter_ranges))
            token = parser%peek()
            if (token%kind == TK_IDENTIFIER .and. len(token%text) == 1) then
                letter_ranges(i) = token%text
                token = parser%consume()
                
                ! Check for range (a-h)  
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == "-") then
                    token = parser%consume()  ! consume '-'
                    token = parser%peek()
                    if (token%kind == TK_IDENTIFIER .and. len(token%text) == 1) then
                        letter_ranges(i) = trim(letter_ranges(i)) // "-" // token%text
                        token = parser%consume()  ! consume end letter
                    end if
                end if
                
                i = i + 1
                
                ! Check for comma
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ",") then  
                    token = parser%consume()  ! consume ','
                else
                    exit
                end if
            else
                exit
            end if
        end do
    end subroutine parse_letter_ranges

    ! Parse character type specification (extracted from parse_implicit_statement)
    subroutine parse_character_type_spec(parser, length_value, has_length)
        type(parser_state_t), intent(inout) :: parser
        integer, intent(out) :: length_value
        logical, intent(out) :: has_length
        type(token_t) :: token

        has_length = .false.
        length_value = 0

        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. token%text == "len") then
            token = parser%consume()  ! consume 'len'
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "=") then
                token = parser%consume()  ! consume '='
                token = parser%peek()
            end if
        end if
        if (token%kind == TK_NUMBER) then
            read(token%text, *) length_value
            has_length = .true.
            token = parser%consume()
        end if
    end subroutine parse_character_type_spec

    ! Parse kind specification (extracted from parse_implicit_statement)
    subroutine parse_kind_specification(parser, kind_value, has_kind)
        type(parser_state_t), intent(inout) :: parser
        integer, intent(out) :: kind_value
        logical, intent(out) :: has_kind
        type(token_t) :: token

        has_kind = .false.
        kind_value = 0

        token = parser%peek()
        if (token%kind == TK_NUMBER) then
            read(token%text, *) kind_value
            has_kind = .true.
            token = parser%consume()
        end if
    end subroutine parse_kind_specification

    ! Check if parentheses contain type specification (extracted logic)
    function is_type_specification(parser, type_name) result(is_type_spec)
        type(parser_state_t), intent(inout) :: parser
        character(len=*), intent(in) :: type_name
        logical :: is_type_spec
        type(token_t) :: token
        integer :: saved_pos

        is_type_spec = .false.
        saved_pos = parser%current_token

        token = parser%consume()  ! consume '('
        token = parser%peek()

        if (type_name == "character") then
            ! For character: check for "len" keyword or number
            if ((token%kind == TK_KEYWORD .and. token%text == "len") .or. &
                token%kind == TK_NUMBER) then
                is_type_spec = .true.
            end if
        else
            ! For other types: check for number (kind specification)
            if (token%kind == TK_NUMBER) then
                is_type_spec = .true.
            end if
        end if

        ! Restore position
        parser%current_token = saved_pos
    end function is_type_specification

    function parse_implicit_statement(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index
        type(token_t) :: token
        character(len=:), allocatable :: type_name
        integer :: kind_value, length_value
        logical :: has_kind, has_length, is_none
        character(len=64), allocatable :: letter_ranges(:)
        integer :: line, column, range_count
        
        ! Consume 'implicit' keyword
        token = parser%consume()
        line = token%line
        column = token%column
        
        ! Check for 'none'
        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. token%text == "none") then
            token = parser%consume()  ! consume 'none'
            is_none = .true.
            stmt_index = push_implicit_statement(arena, is_none, &
                                                line=line, column=column, parent_index=0)
            return
        end if
        
        ! Parse type specification
        is_none = .false.
        has_kind = .false.
        has_length = .false.
        kind_value = 0
        length_value = 0
        range_count = 0
        
        ! Get type name
        if (token%kind == TK_KEYWORD) then
            type_name = token%text
            token = parser%consume()
            
            ! Check for kind or length specification
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "(") then
                if (is_type_specification(parser, type_name)) then
                    ! Parse as type specification
                    token = parser%consume()  ! consume '('
                    
                    if (type_name == "character") then
                        call parse_character_type_spec(parser, length_value, has_length)
                    else
                        call parse_kind_specification(parser, kind_value, has_kind)
                    end if
                    
                    token = parser%peek()
                    if (token%kind == TK_OPERATOR .and. token%text == ")") then
                        token = parser%consume()  ! consume ')'
                    end if
                end if
            end if
        end if
        
        ! Parse letter specification list in parentheses
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            token = parser%consume()  ! consume '('
            
            ! Count letter ranges first
            call count_letter_ranges(parser, range_count)
            
            if (range_count > 0) then
                allocate(letter_ranges(range_count))
                call parse_letter_ranges(parser, letter_ranges)
            end if
            
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()  ! consume ')'
            end if
        end if
        
        ! Create implicit statement node
        stmt_index = push_implicit_statement(arena, is_none, type_name, kind_value, &
                                           has_kind, length_value, has_length, &
                                           letter_ranges, line, column, parent_index=0)
    end function parse_implicit_statement

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

    function parse_module(parser, arena) result(module_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: module_index
        type(token_t) :: token
        character(len=:), allocatable :: module_name
        integer :: line, column
        integer, allocatable :: declaration_indices(:), procedure_indices(:)
        logical :: has_contains, in_contains_section
        integer :: stmt_index

        ! Consume 'module' keyword
        token = parser%consume()
        line = token%line
        column = token%column

        ! Get module name
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            token = parser%consume()
            module_name = token%text
        else
            module_name = "unnamed_module"
        end if

        ! Initialize arrays
        allocate(declaration_indices(0))
        allocate(procedure_indices(0))
        has_contains = .false.
        in_contains_section = .false.

        ! Minimal parsing to detect structure and consume tokens
        do while (.not. parser%is_at_end())
            token = parser%peek()

            ! Check for end of module
            if (token%kind == TK_KEYWORD .and. token%text == "end") then
                ! Look ahead for "module"
                if (parser%current_token + 1 <= size(parser%tokens)) then
                    if (parser%tokens(parser%current_token + 1)%kind == TK_KEYWORD .and. &
                        parser%tokens(parser%current_token + 1)%text == "module") then
                        ! Consume "end module"
                        token = parser%consume()  ! consume "end"
                        token = parser%consume()  ! consume "module"
                        exit
                    end if
                end if
            end if

            ! Check for contains keyword
            if (token%kind == TK_KEYWORD .and. token%text == "contains") then
                has_contains = .true.
                in_contains_section = .true.
                token = parser%consume()  ! consume "contains"
                cycle  ! Continue to next iteration
            end if
            
            ! Parse declarations in module body (before contains)
            if (.not. in_contains_section) then
                if (token%kind == TK_KEYWORD) then
                    select case (token%text)
                    case ("integer", "real", "logical", "character", "complex")
                        stmt_index = parse_declaration(parser, arena)
                        if (stmt_index > 0) then
                            declaration_indices = [declaration_indices, stmt_index]
                        end if
                        cycle  ! Continue to next iteration
                    case ("implicit")
                        ! Parse implicit statement
                        call parse_simple_implicit_in_module(parser, arena, stmt_index)
                        if (stmt_index > 0) then
                            declaration_indices = [declaration_indices, stmt_index]
                        end if
                        cycle  ! Continue to next iteration
                    end select
                end if
            end if

            ! Create basic subroutine nodes for contains section
            if (in_contains_section .and. token%kind == TK_KEYWORD .and. token%text == "subroutine") then
                token = parser%consume()  ! consume "subroutine"
                token = parser%peek()
                if (token%kind == TK_IDENTIFIER) then
                    block
                        character(len=:), allocatable :: sub_name
                        integer, allocatable :: empty_params(:), empty_body(:)
                        integer :: sub_index
                        sub_name = token%text
                        token = parser%consume()  ! consume subroutine name
                        allocate(empty_params(0))
                        allocate(empty_body(0))
                        sub_index = push_subroutine_def(arena, sub_name, empty_params, empty_body, &
                                                      token%line, token%column)
                        procedure_indices = [procedure_indices, sub_index]
                    end block
                end if
                cycle
            end if
            
            ! Create basic function nodes for contains section
            if (in_contains_section .and. token%kind == TK_KEYWORD .and. token%text == "function") then
                token = parser%consume()  ! consume "function"
                token = parser%peek()
                if (token%kind == TK_IDENTIFIER) then
                    block
                        character(len=:), allocatable :: func_name
                        integer, allocatable :: empty_params(:), empty_body(:)
                        integer :: func_index
                        func_name = token%text
                        token = parser%consume()  ! consume function name
                        allocate(empty_params(0))
                        allocate(empty_body(0))
                        func_index = push_function_def(arena, func_name, empty_params, "integer", &
                                                     empty_body, token%line, token%column)
                        procedure_indices = [procedure_indices, func_index]
                    end block
                end if
                cycle
            end if
            
            ! Only advance if we haven't handled this token specifically
            ! (declarations and contains are handled above with cycle)
            if (in_contains_section) then
                token = parser%consume()  ! Always advance in contains section
            else
                token = parser%consume()  ! Advance for unhandled tokens
            end if
        end do

        ! Create module node with proper structure
        module_index = push_module_structured(arena, module_name, declaration_indices, &
                                             procedure_indices, has_contains, line, column)
    end function parse_module
    
    ! Parse a simple implicit statement in module context
    subroutine parse_simple_implicit_in_module(parser, arena, stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: stmt_index
        type(token_t) :: implicit_token, none_token
        character(len=:), allocatable :: implicit_type
        
        stmt_index = 0
        
        ! Get implicit keyword
        implicit_token = parser%consume()
        
        ! Check for 'none'
        none_token = parser%peek()
        if (none_token%kind == TK_KEYWORD .and. none_token%text == "none") then
            none_token = parser%consume()
            implicit_type = "none"
        else
            implicit_type = "default"
        end if
        
        ! Create implicit statement node
        if (implicit_type == "none") then
            stmt_index = push_implicit_statement(arena, .true., &
                                               line=implicit_token%line, column=implicit_token%column)
        else
            stmt_index = push_implicit_statement(arena, .false., &
                                               line=implicit_token%line, column=implicit_token%column)
        end if
    end subroutine parse_simple_implicit_in_module

end module parser_import_statements_module