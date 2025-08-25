module parser_import_statements_module
    ! Parser module for import/module statement types (use, implicit, include, module)
    use lexer_core
    use parser_state_module
    use ast_core
    use ast_factory
    use parser_declarations, only: parse_declaration
    ! Removed circular dependency to parser_definition_statements_module
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

            ! Parse subroutine definitions for contains section (safe module context)
            if (in_contains_section .and. token%kind == TK_KEYWORD .and. token%text == "subroutine") then
                stmt_index = parse_subroutine_in_module(parser, arena)
                if (stmt_index > 0) then
                    procedure_indices = [procedure_indices, stmt_index]
                end if
                cycle
            end if
            
            ! Parse function definitions for contains section (safe module context)
            if (in_contains_section .and. token%kind == TK_KEYWORD .and. token%text == "function") then
                stmt_index = parse_function_in_module(parser, arena)
                if (stmt_index > 0) then
                    procedure_indices = [procedure_indices, stmt_index]
                end if
                cycle
            end if
            
            ! Only advance if we haven't handled this token specifically
            ! (declarations and contains are handled above with cycle)
            if (in_contains_section) then
                ! Only consume tokens that are not function/subroutine keywords to prevent
                ! interfering with multiple procedure definitions in contains section
                if (.not. (token%kind == TK_KEYWORD .and. &
                          (token%text == "function" .or. token%text == "subroutine"))) then
                    token = parser%consume()  ! Advance for non-procedure tokens
                end if
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

    ! Safe subroutine parsing for module contexts (avoids circular dependencies)
    function parse_subroutine_in_module(parser, arena) result(sub_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: sub_index
        type(token_t) :: token
        character(len=:), allocatable :: subroutine_name
        integer :: line, column
        integer, allocatable :: param_indices(:), body_indices(:)
        
        ! Consume subroutine keyword
        token = parser%consume()
        line = token%line
        column = token%column
        
        ! Get subroutine name
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            subroutine_name = token%text
            token = parser%consume()
        else
            subroutine_name = "unnamed_subroutine"
        end if
        
        ! Parse parameters (simplified - no type info)
        allocate(param_indices(0))
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            token = parser%consume()  ! consume '('
            
            ! Parse parameter names only
            do while (.not. parser%is_at_end())
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ")") then
                    token = parser%consume()  ! consume ')'
                    exit
                end if
                
                if (token%kind == TK_IDENTIFIER) then
                    ! Create simple parameter node
                    block
                        integer :: param_index
                        param_index = push_parameter_declaration(arena, token%text, "", 0, 0, .false., &
                                                               line=token%line, column=token%column)
                        param_indices = [param_indices, param_index]
                    end block
                    token = parser%consume()
                end if
                
                ! Skip commas
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ",") then
                    token = parser%consume()
                end if
            end do
        end if
        
        ! Parse subroutine body until "end subroutine" (simplified)
        allocate(body_indices(0))
        do while (.not. parser%is_at_end())
            token = parser%peek()
            
            ! Check for end of subroutine
            if (token%kind == TK_KEYWORD .and. token%text == "end") then
                if (parser%current_token + 1 <= size(parser%tokens)) then
                    if (parser%tokens(parser%current_token + 1)%kind == TK_KEYWORD .and. &
                        parser%tokens(parser%current_token + 1)%text == "subroutine") then
                        ! Consume "end subroutine"
                        token = parser%consume()  ! consume "end"
                        token = parser%consume()  ! consume "subroutine"
                        ! Optionally consume subroutine name
                        if (.not. parser%is_at_end()) then
                            token = parser%peek()
                            if (token%kind == TK_IDENTIFIER .and. token%text == subroutine_name) then
                                token = parser%consume()
                            end if
                        end if
                        exit
                    end if
                end if
            end if
            
            ! Parse basic statements for subroutine body (avoiding circular dependencies)
            if (token%kind /= TK_NEWLINE) then
                block
                    integer :: stmt_index
                    stmt_index = parse_basic_statement_in_subroutine(parser, arena)
                    if (stmt_index > 0) then
                        body_indices = [body_indices, stmt_index]
                    end if
                end block
            else
                token = parser%consume()  ! consume newline
            end if
        end do
        
        ! Create subroutine node
        sub_index = push_subroutine_def(arena, subroutine_name, param_indices, body_indices, &
                                       line, column)
    end function parse_subroutine_in_module

    ! Safe function parsing for module contexts (avoids circular dependencies)
    function parse_function_in_module(parser, arena) result(func_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: func_index
        type(token_t) :: token
        character(len=:), allocatable :: function_name, return_type_str
        integer :: line, column
        integer, allocatable :: param_indices(:), body_indices(:)
        
        ! Initialize
        return_type_str = ""
        
        ! Check if we have a return type before "function"
        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. &
            (token%text == "real" .or. token%text == "integer" .or. &
             token%text == "logical" .or. token%text == "character")) then
            return_type_str = token%text
            token = parser%consume()
        end if
        
        ! Consume function keyword
        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. token%text == "function") then
            line = token%line
            column = token%column
            token = parser%consume()
        else
            func_index = 0
            return
        end if
        
        ! Get function name
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            function_name = token%text
            token = parser%consume()
        else
            function_name = "unnamed_function"
        end if
        
        ! Parse parameters (simplified - no type info)
        allocate(param_indices(0))
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            token = parser%consume()  ! consume '('
            
            ! Parse parameter names only
            do while (.not. parser%is_at_end())
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ")") then
                    token = parser%consume()  ! consume ')'
                    exit
                end if
                
                if (token%kind == TK_IDENTIFIER) then
                    ! Create simple parameter node
                    block
                        integer :: param_index
                        param_index = push_parameter_declaration(arena, token%text, "", 0, 0, .false., &
                                                               line=token%line, column=token%column)
                        param_indices = [param_indices, param_index]
                    end block
                    token = parser%consume()
                end if
                
                ! Skip commas
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ",") then
                    token = parser%consume()
                end if
            end do
        end if
        
        ! Skip result clause if present
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER .and. token%text == "result") then
            token = parser%consume()
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "(") then
                token = parser%consume()
                token = parser%peek()
                if (token%kind == TK_IDENTIFIER) then
                    token = parser%consume()
                end if
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ")") then
                    token = parser%consume()
                end if
            end if
        end if
        
        ! Parse function body until "end function" (simplified)
        allocate(body_indices(0))
        do while (.not. parser%is_at_end())
            token = parser%peek()
            
            ! Check for end of function
            if (token%kind == TK_KEYWORD .and. token%text == "end") then
                if (parser%current_token + 1 <= size(parser%tokens)) then
                    if (parser%tokens(parser%current_token + 1)%kind == TK_KEYWORD .and. &
                        parser%tokens(parser%current_token + 1)%text == "function") then
                        ! Consume "end function"
                        token = parser%consume()  ! consume "end"
                        token = parser%consume()  ! consume "function"
                        ! Optionally consume function name
                        if (.not. parser%is_at_end()) then
                            token = parser%peek()
                            if (token%kind == TK_IDENTIFIER .and. token%text == function_name) then
                                token = parser%consume()
                            end if
                        end if
                        exit
                    end if
                end if
            end if
            
            ! Stop parsing body if we encounter another function or subroutine definition
            ! This prevents consuming tokens that belong to subsequent procedures
            if (token%kind == TK_KEYWORD .and. &
                (token%text == "function" .or. token%text == "subroutine")) then
                exit  ! Don't consume, let the module parser handle it
            end if
            
            ! Parse basic statements for subroutine body (avoiding circular dependencies)
            if (token%kind /= TK_NEWLINE) then
                block
                    integer :: stmt_index
                    stmt_index = parse_basic_statement_in_subroutine(parser, arena)
                    if (stmt_index > 0) then
                        body_indices = [body_indices, stmt_index]
                    end if
                end block
            else
                token = parser%consume()  ! consume newline
            end if
        end do
        
        ! Create function node
        func_index = push_function_def(arena, function_name, param_indices, &
                                      return_type_str, body_indices, &
                                      line, column, result_variable="")
    end function parse_function_in_module

    ! Basic statement parsing for subroutine/function bodies (avoiding circular deps)
    function parse_basic_statement_in_subroutine(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index
        type(token_t) :: token
        
        ! Check first token to determine statement type
        token = parser%peek()
        stmt_index = 0
        
        select case (token%kind)
        case (TK_KEYWORD)
            select case (token%text)
            case ("print")
                stmt_index = parse_simple_print_statement(parser, arena)
            case ("integer", "real", "logical", "character", "complex", "double")
                stmt_index = parse_declaration(parser, arena)
            case ("call")
                stmt_index = parse_simple_call_statement(parser, arena)
            case default
                ! Skip unknown statement by consuming tokens until end of line
                call skip_to_end_of_line(parser)
                stmt_index = 0
            end select
        case (TK_IDENTIFIER)
            ! Likely assignment statement
            stmt_index = parse_simple_assignment_statement(parser, arena)
        case default
            ! Skip unknown token
            call skip_to_end_of_line(parser)
            stmt_index = 0
        end select
    end function parse_basic_statement_in_subroutine

    ! Simple print statement parser for subroutine bodies
    function parse_simple_print_statement(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index
        type(token_t) :: token
        integer, allocatable :: arg_indices(:)
        integer :: line, column
        
        ! Consume 'print' keyword
        token = parser%consume()
        line = token%line
        column = token%column
        
        ! Expect '*' or format specifier
        allocate(arg_indices(0))
        
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "*") then
            token = parser%consume()  ! consume '*'
            
            ! Optional comma (may not be present in compact format like print*,"foo")
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()  ! consume ','
            end if
            
            ! Parse print arguments until end of line
            do while (.not. parser%is_at_end())
                token = parser%peek()
                if (token%kind == TK_NEWLINE) then
                    exit
                end if
                
                if (token%kind == TK_STRING) then
                    ! String literal
                    block
                        integer :: arg_index
                        arg_index = push_literal(arena, token%text, LITERAL_STRING, &
                                               token%line, token%column)
                        arg_indices = [arg_indices, arg_index]
                    end block
                    token = parser%consume()
                    
                    ! After parsing a string literal, check if we're at end of statement
                    token = parser%peek()
                    if (token%kind == TK_NEWLINE) then
                        exit  ! End of print statement
                    else if (token%kind == TK_OPERATOR .and. token%text == ",") then
                        token = parser%consume()  ! consume comma and continue
                    else
                        ! No comma after string - end of print arguments
                        exit
                    end if
                else if (token%kind == TK_IDENTIFIER) then
                    ! Variable reference - but be careful not to consume keywords that end the statement
                    if (token%text == "end" .or. token%text == "subroutine" .or. &
                        token%text == "function") then
                        ! This identifier likely belongs to a different statement
                        exit
                    end if
                    
                    block
                        integer :: arg_index
                        arg_index = push_identifier(arena, token%text, &
                                                  token%line, token%column)
                        arg_indices = [arg_indices, arg_index]
                    end block
                    token = parser%consume()
                    
                    ! After parsing an identifier, check if we're at end of statement
                    token = parser%peek()
                    if (token%kind == TK_NEWLINE) then
                        exit  ! End of print statement
                    else if (token%kind == TK_OPERATOR .and. token%text == ",") then
                        token = parser%consume()  ! consume comma and continue
                    else
                        ! No comma after identifier - end of print arguments
                        exit
                    end if
                else if (token%kind == TK_NUMBER) then
                    ! Numeric literal
                    block
                        integer :: arg_index
                        arg_index = push_literal(arena, token%text, LITERAL_INTEGER, &
                                               token%line, token%column)
                        arg_indices = [arg_indices, arg_index]
                    end block
                    token = parser%consume()
                    
                    ! After parsing a number, check if we're at end of statement
                    token = parser%peek()
                    if (token%kind == TK_NEWLINE) then
                        exit  ! End of print statement
                    else if (token%kind == TK_OPERATOR .and. token%text == ",") then
                        token = parser%consume()  ! consume comma and continue
                    else
                        ! No comma after number - end of print arguments
                        exit
                    end if
                else if (token%kind == TK_OPERATOR .and. token%text == ",") then
                    token = parser%consume()  ! consume comma
                else
                    ! Unknown token - likely end of print statement
                    exit
                end if
            end do
        end if
        
        ! Create print statement node
        stmt_index = push_print_statement(arena, "*", arg_indices, line, column)
    end function parse_simple_print_statement

    ! Simple call statement parser for subroutine bodies
    function parse_simple_call_statement(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index
        type(token_t) :: token
        character(len=:), allocatable :: subroutine_name
        integer, allocatable :: arg_indices(:)
        integer :: line, column
        
        ! Consume 'call' keyword
        token = parser%consume()
        line = token%line
        column = token%column
        
        ! Get subroutine name
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            subroutine_name = token%text
            token = parser%consume()
            
            ! For simplicity, no arguments parsed (can be enhanced later)
            allocate(arg_indices(0))
            
            ! Skip to end of line
            call skip_to_end_of_line(parser)
            
            ! Create call statement node
            stmt_index = push_subroutine_call(arena, subroutine_name, arg_indices, &
                                            line, column)
        else
            ! Error: expected subroutine name
            stmt_index = 0
            call skip_to_end_of_line(parser)
        end if
    end function parse_simple_call_statement

    ! Simple assignment statement parser for subroutine bodies
    function parse_simple_assignment_statement(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index
        type(token_t) :: token
        integer :: lhs_index, rhs_index
        
        stmt_index = 0
        
        ! Parse left-hand side (identifier)
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            lhs_index = push_identifier(arena, token%text, token%line, token%column)
            token = parser%consume()
            
            ! Expect assignment operator
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "=") then
                token = parser%consume()  ! consume '='
                
                ! Parse right-hand side (simple expression)
                token = parser%peek()
                if (token%kind == TK_STRING) then
                    rhs_index = push_literal(arena, token%text, LITERAL_STRING, &
                                           token%line, token%column)
                    token = parser%consume()
                else if (token%kind == TK_NUMBER) then
                    rhs_index = push_literal(arena, token%text, LITERAL_INTEGER, &
                                           token%line, token%column)
                    token = parser%consume()
                else if (token%kind == TK_IDENTIFIER) then
                    rhs_index = push_identifier(arena, token%text, &
                                              token%line, token%column)
                    token = parser%consume()
                else
                    ! Skip to end of line for complex expressions
                    call skip_to_end_of_line(parser)
                    return
                end if
                
                ! Skip remaining tokens on line
                call skip_to_end_of_line(parser)
                
                ! Create assignment node
                stmt_index = push_assignment(arena, lhs_index, rhs_index, &
                                           token%line, token%column)
            else
                call skip_to_end_of_line(parser)
            end if
        else
            call skip_to_end_of_line(parser)
        end if
    end function parse_simple_assignment_statement

    ! Skip tokens until end of line
    subroutine skip_to_end_of_line(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token
        
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_NEWLINE) then
                token = parser%consume()  ! consume newline
                exit
            end if
            token = parser%consume()
        end do
    end subroutine skip_to_end_of_line

end module parser_import_statements_module