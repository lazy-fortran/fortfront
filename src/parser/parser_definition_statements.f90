module parser_definition_statements_module
    ! Parser module for definition statement types (function, subroutine, interface, derived types)
    use lexer_core
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_statements_module, only: parse_statement_in_if_block
    use parser_control_flow_module, only: parse_if
    use ast_core
    use ast_factory
    use ast_types, only: LITERAL_STRING
    use ast_nodes_data, only: declaration_node, parameter_declaration_node, &
                              INTENT_IN, INTENT_OUT, INTENT_INOUT
    implicit none
    private

    public :: parse_derived_type, parse_function_definition, parse_subroutine_definition
    public :: parse_interface_block, parse_typed_parameters

contains

    ! Merge parameter attributes from declaration nodes into parameter nodes
    subroutine merge_parameter_attributes(arena, param_indices, body_indices)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: param_indices(:)
        integer, intent(in) :: body_indices(:)
        
        integer :: i, j
        character(len=:), allocatable :: param_name, decl_name
        
        ! For each parameter node
        do i = 1, size(param_indices)
            if (param_indices(i) <= 0 .or. param_indices(i) > arena%size) cycle
            
            select type (param_node => arena%entries(param_indices(i))%node)
            type is (parameter_declaration_node)
                param_name = param_node%name
                
                ! Look for corresponding declaration in body
                do j = 1, size(body_indices)
                    if (body_indices(j) <= 0 .or. body_indices(j) > arena%size) cycle
                    
                    select type (body_node => arena%entries(body_indices(j))%node)
                    type is (declaration_node)
                        ! Check if this declaration is for the parameter
                        if (body_node%is_multi_declaration) then
                            ! Check multi-declaration var_names
                            if (allocated(body_node%var_names)) then
                                if (any(body_node%var_names == param_name)) then
                                    ! Update parameter node with attributes
                                    if (body_node%has_intent .and. allocated(body_node%intent)) then
                                        select case (body_node%intent)
                                        case ("in")
                                            param_node%intent_type = INTENT_IN
                                        case ("out")
                                            param_node%intent_type = INTENT_OUT
                                        case ("inout")
                                            param_node%intent_type = INTENT_INOUT
                                        end select
                                    end if
                                    param_node%is_optional = body_node%is_optional
                                    
                                    ! Also update type if not already set
                                    if (param_node%type_name == "" .and. allocated(body_node%type_name)) then
                                        param_node%type_name = body_node%type_name
                                        param_node%kind_value = body_node%kind_value
                                        param_node%has_kind = body_node%has_kind
                                    end if
                                end if
                            end if
                        else
                            ! Single declaration
                            if (allocated(body_node%var_name) .and. body_node%var_name == param_name) then
                                ! Update parameter node with attributes
                                if (body_node%has_intent .and. allocated(body_node%intent)) then
                                    select case (body_node%intent)
                                    case ("in")
                                        param_node%intent_type = INTENT_IN
                                    case ("out")
                                        param_node%intent_type = INTENT_OUT
                                    case ("inout")
                                        param_node%intent_type = INTENT_INOUT
                                    end select
                                end if
                                param_node%is_optional = body_node%is_optional
                                
                                ! Also update type if not already set
                                if (param_node%type_name == "" .and. allocated(body_node%type_name)) then
                                    param_node%type_name = body_node%type_name
                                    param_node%kind_value = body_node%kind_value
                                    param_node%has_kind = body_node%has_kind
                                end if
                            end if
                        end if
                    end select
                end do
            end select
        end do
    end subroutine merge_parameter_attributes

    ! Parse derived type parameters inside parentheses
    subroutine parse_derived_type_parameters(parser, arena, param_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(out) :: param_indices(:)

        type(token_t) :: token

        ! Initialize
        allocate (param_indices(0))

        ! Parse parameters separated by commas (simplified)
        do
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                exit
            end if

            if (token%kind == TK_IDENTIFIER) then
                ! Add parameter (simplified - would normally parse full parameter)
                token = parser%consume()
            end if

            ! Check for comma
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()  ! consume ','
            else
                exit
            end if
        end do
    end subroutine parse_derived_type_parameters

    function parse_derived_type(parser, arena) result(type_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: type_index

        type(token_t) :: token
        character(len=:), allocatable :: type_name
        integer :: line, column
        logical :: has_parameters
        integer, allocatable :: param_indices(:)

        ! Consume 'type' keyword
        token = parser%consume()
        line = token%line
        column = token%column
        has_parameters = .false.

        ! Check for :: or just get type name
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "::") then
            ! Consume ::
            token = parser%consume()

            ! Get type name
            token = parser%peek()
            if (token%kind == TK_IDENTIFIER) then
                token = parser%consume()
                type_name = token%text

                ! Check for parameters after type name
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == "(") then
                    has_parameters = .true.
                    token = parser%consume()  ! consume '('
                    call parse_derived_type_parameters(parser, arena, param_indices)

                    ! Consume ')'
                    token = parser%peek()
                    if (token%kind == TK_OPERATOR .and. token%text == ")") then
                        token = parser%consume()
                    end if
                end if
            else
                type_name = "unnamed_type"
            end if
        else if (token%kind == TK_IDENTIFIER) then
            ! Direct type name
            token = parser%consume()
            type_name = token%text

            ! Check for parameters after type name
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "(") then
                has_parameters = .true.
                token = parser%consume()  ! consume '('
                call parse_derived_type_parameters(parser, arena, param_indices)

                ! Consume ')'
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ")") then
                    token = parser%consume()
                end if
            end if
        else
            type_name = "unnamed_type"
        end if

        ! Create derived type node
        if (has_parameters .and. allocated(param_indices)) then
            type_index = push_derived_type(arena, type_name, &
                                            param_indices=param_indices, &
                                            line=line, column=column)
        else
            type_index = push_derived_type(arena, type_name, line=line, column=column)
        end if
    end function parse_derived_type

    ! Parse typed parameters (full implementation restored)
    subroutine parse_typed_parameters(parser, arena, param_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(out) :: param_indices(:)

        type(token_t) :: token
        integer :: param_count
        logical :: parsing_type_spec
        character(len=:), allocatable :: current_type, current_kind_str
        integer :: current_kind, current_intent
        logical :: current_is_optional
        integer, allocatable :: temp_params(:)
        integer :: line, column
        character(len=:), allocatable :: type_expr
        integer :: paren_count

        param_count = 0
        allocate (param_indices(0))
        parsing_type_spec = .false.

        do while (.not. parser%is_at_end())
            token = parser%peek()

            ! Check for closing parenthesis
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                exit
            end if

            ! Check for comma (parameter separator)
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()
                parsing_type_spec = .false.  ! Reset after comma
                cycle
            end if

            ! Check for type keywords (real, integer, etc.)
            if (token%kind == TK_KEYWORD .and. &
                (token%text == "real" .or. token%text == "integer" .or. &
                 token%text == "logical" .or. token%text == "character" .or. &
                 token%text == "type")) then

                parsing_type_spec = .true.
                current_type = token%text
                current_kind = 0
                current_intent = 0
                current_is_optional = .false.
                line = token%line
                column = token%column
                token = parser%consume()

                ! Check for kind specification (e.g., real(8)) or type &
                ! specification (e.g., type(name))
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == "(") then
                    token = parser%consume()  ! consume '('

                    ! For type keywords, parse the complete type expression &
                    ! including nested parentheses
                    if (current_type == "type") then
                        type_expr = ""
                        paren_count = 1  ! We already consumed the opening parenthesis

                        do while (.not. parser%is_at_end() .and. paren_count > 0)
                            token = parser%peek()
                            if (token%kind == TK_OPERATOR .and. token%text == "(") then
                                paren_count = paren_count + 1
                                type_expr = type_expr//token%text
                                token = parser%consume()
                        else if (token%kind == TK_OPERATOR .and. token%text == ")") then
                                paren_count = paren_count - 1
                                if (paren_count > 0) then
                                    type_expr = type_expr//token%text
                                end if
                                token = parser%consume()
                            else
                                type_expr = type_expr//token%text
                                token = parser%consume()
                            end if
                        end do

                        current_type = current_type//"("//type_expr//")"
                    else
                        ! For non-type keywords, expect a simple number for &
                        ! kind specification
                        token = parser%peek()
                        if (token%kind == TK_NUMBER) then
                            read (token%text, *) current_kind
                            token = parser%consume()
                        end if

                        ! Consume closing ')'
                        token = parser%peek()
                        if (token%kind == TK_OPERATOR .and. token%text == ")") then
                            token = parser%consume()
                        end if
                    end if
                end if

                ! Check for comma followed by attributes
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ",") then
                    token = parser%consume()  ! consume ','

                    ! Check for intent
                    token = parser%peek()
                    if ((token%kind == TK_KEYWORD .or. &
                         token%kind == TK_IDENTIFIER) .and. &
                        token%text == "intent") then
                        token = parser%consume()  ! consume 'intent'

                        token = parser%peek()
                        if (token%kind == TK_OPERATOR .and. token%text == "(") then
                            token = parser%consume()  ! consume '('

                            token = parser%peek()
                            if (token%kind == TK_KEYWORD .or. &
                                token%kind == TK_IDENTIFIER) then
                                if (token%text == "in") then
                                    current_intent = 1
                                else if (token%text == "out") then
                                    current_intent = 2
                                else if (token%text == "inout") then
                                    current_intent = 3
                                end if
                                token = parser%consume()
                            end if

                            ! Consume ')'
                            token = parser%peek()
                            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                                token = parser%consume()
                            end if
                        end if
                 else if (token%kind == TK_KEYWORD .and. token%text == "dimension") then
                        ! Skip dimension attribute for now
                        token = parser%consume()
                        token = parser%peek()
                        if (token%kind == TK_OPERATOR .and. token%text == "(") then
                            ! Skip dimension specification
                            token = parser%consume()
                            do while (.not. parser%is_at_end())
                                token = parser%peek()
                             if (token%kind == TK_OPERATOR .and. token%text == ")") then
                                    token = parser%consume()
                                    exit
                                end if
                                token = parser%consume()
                            end do
                        end if
                    else if (token%kind == TK_KEYWORD .and. &
                             token%text == "optional") then
                        ! Mark current type as optional
                        current_is_optional = .true.
                        token = parser%consume()
                    end if
                end if

                ! Expect :: after type specification
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == "::") then
                    token = parser%consume()
                end if

                ! Now collect all parameter names for this type
                allocate (temp_params(0))
                do while (.not. parser%is_at_end())
                    token = parser%peek()

                    if (token%kind == TK_IDENTIFIER) then
                        ! Parse parameter name and check for array specification
                        block
                            character(len=:), allocatable :: param_name
                            integer :: param_index
                            integer, allocatable :: dim_indices(:)
                            integer :: dim_count

                            param_name = token%text
                            token = parser%consume()

                            ! Check for array specification
                            allocate (dim_indices(0))
                            dim_count = 0
                            token = parser%peek()
                            if (token%kind == TK_OPERATOR .and. token%text == "(") then
                                token = parser%consume()  ! consume '('

                                ! Parse array dimensions
                                do while (.not. parser%is_at_end())
                                    token = parser%peek()
                             if (token%kind == TK_OPERATOR .and. token%text == ")") then
                                        token = parser%consume()  ! consume ')'
                                        exit
                                    end if

                                    ! Parse dimension expression (for now, create &
                                    ! identifier or literal nodes)
                             if (token%kind == TK_OPERATOR .and. token%text == ":") then
                                        ! Assumed shape (:)
                                        block
                                            integer :: dim_index
                       dim_index = push_identifier(arena, ":", token%line, token%column)
                                            dim_indices = [dim_indices, dim_index]
                                            dim_count = dim_count + 1
                                        end block
                                        token = parser%consume()
                        else if (token%kind == TK_OPERATOR .and. token%text == "*") then
                                        ! Assumed size (*)
                                        block
                                            integer :: dim_index
                       dim_index = push_identifier(arena, "*", token%line, token%column)
                                            dim_indices = [dim_indices, dim_index]
                                            dim_count = dim_count + 1
                                        end block
                                        token = parser%consume()
                                    else if (token%kind == TK_NUMBER) then
                                        ! Explicit size
                                        block
                                            integer :: dim_index
                          dim_index = push_literal(arena, token%text, LITERAL_INTEGER, &
                                                               token%line, token%column)
                                            dim_indices = [dim_indices, dim_index]
                                            dim_count = dim_count + 1
                                        end block
                                        token = parser%consume()
                                    else if (token%kind == TK_IDENTIFIER) then
                                        ! Variable size
                                        block
                                            integer :: dim_index
                dim_index = push_identifier(arena, token%text, token%line, token%column)
                                            dim_indices = [dim_indices, dim_index]
                                            dim_count = dim_count + 1
                                        end block
                                        token = parser%consume()
                                    else
                                        ! Skip unrecognized tokens
                                        token = parser%consume()
                                    end if

                                    ! Check for comma (more dimensions)
                                    token = parser%peek()
                             if (token%kind == TK_OPERATOR .and. token%text == ",") then
                                        token = parser%consume()  ! consume ','
                                    end if
                                end do
                            end if

                            ! Create parameter declaration node with array info
                            if (dim_count > 0) then
             param_index = push_parameter_declaration(arena, param_name, current_type, &
                                                         current_kind, current_intent, &
                                                         current_is_optional, dim_indices, line, column)
                            else
             param_index = push_parameter_declaration(arena, param_name, current_type, &
                                                         current_kind, current_intent, &
                                                         current_is_optional, &
                                                               line=line, column=column)
                            end if
                            temp_params = [temp_params, param_index]
                        end block

                        ! Check for comma (more params of same type)
                        token = parser%peek()
                        if (token%kind == TK_OPERATOR .and. token%text == ",") then
                            ! Peek ahead to see if next is a type keyword
                            if (parser%current_token + 1 <= size(parser%tokens)) then
                                block
                                    type(token_t) :: next_token
                                    next_token = parser%tokens(parser%current_token + 1)
                                    if (next_token%kind == TK_KEYWORD .and. &
                     (next_token%text == "real" .or. next_token%text == "integer" .or. &
                 next_token%text == "logical" .or. next_token%text == "character")) then
                                        ! This comma separates different type groups
                                        exit
                                    else
                                        ! This comma separates params of same type
                                        token = parser%consume()  ! consume comma
                                        cycle
                                    end if
                                end block
                            else
                                token = parser%consume()  ! consume comma
                                cycle
                            end if
                        else
                            ! No more params of this type
                            exit
                        end if
                    else
                        ! Not an identifier, stop collecting params for this type
                        exit
                    end if
                end do

                ! Add collected params to main list
                param_indices = [param_indices, temp_params]
                param_count = param_count + size(temp_params)
                deallocate (temp_params)

            else if (token%kind == TK_IDENTIFIER .and. .not. parsing_type_spec) then
                ! Simple parameter without explicit type
                ! Create a parameter_declaration_node even without type info
                block
                    integer :: param_index
                    param_index = push_parameter_declaration(arena, &
                        name=token%text, &
                        type_name="", &  ! Empty type, will be inferred from body
                        kind_value=0, &
                        intent_value=0, &  ! No intent specified in parameter list
                        is_optional=.false., &  ! Not optional by default
                        line=token%line, &
                        column=token%column)
                    param_indices = [param_indices, param_index]
                    param_count = param_count + 1
                end block
                token = parser%consume()
            else
                ! Skip unexpected token
                token = parser%consume()
            end if
        end do

    end subroutine parse_typed_parameters

    function parse_function_definition(parser, arena) result(func_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: func_index

        type(token_t) :: token
        character(len=:), allocatable :: function_name, return_type_str, result_variable_name
        integer :: line, column
        integer, allocatable :: param_indices(:), body_indices(:)

        ! Initialize
        return_type_str = ""
        result_variable_name = ""

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

        ! Parse parameters
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            token = parser%consume()
            call parse_typed_parameters(parser, arena, param_indices)
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()
            end if
        else
            allocate(param_indices(0))
        end if

        ! Check for result clause
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

        ! Parse function body until "end function"
        allocate(body_indices(0))

        do while (.not. parser%is_at_end())
            token = parser%peek()

            ! Check for end of function
            if (token%kind == TK_KEYWORD .and. token%text == "end") then
                ! Look ahead for "function"
                block
                    type(token_t), allocatable :: all_tokens(:)
                    integer :: next_idx
                    if (allocated(parser%tokens)) then
                        allocate(all_tokens(size(parser%tokens)))
                        all_tokens = parser%tokens
                    else
                        allocate(all_tokens(0))
                    end if
                    next_idx = parser%current_token + 1
                    if (next_idx <= size(all_tokens)) then
                        if (all_tokens(next_idx)%kind == TK_KEYWORD .and. &
                            all_tokens(next_idx)%text == "function") then
                            ! Consume "end function"
                            token = parser%consume()  ! consume "end"
                            token = parser%consume()  ! consume "function"
                            ! Optionally consume function name
                            if (.not. parser%is_at_end()) then
                                token = parser%peek()
                                if (token%kind == TK_IDENTIFIER .and. &
                                    token%text == function_name) then
                                    token = parser%consume()
                                end if
                            end if
                            exit  ! Exit the body parsing loop
                        end if
                    end if
                end block
            end if

            ! Skip empty lines
            if (token%kind == TK_NEWLINE) then
                token = parser%consume()
                cycle
            end if

            ! Collect tokens for the current statement line
            block
                type(token_t), allocatable :: stmt_tokens(:), all_tokens(:)
                integer :: stmt_start, stmt_end, i, stmt_size, stmt_index
                type(parser_state_t) :: block_parser
                
                if (allocated(parser%tokens)) then
                    allocate(all_tokens(size(parser%tokens)))
                    all_tokens = parser%tokens
                else
                    allocate(all_tokens(0))
                end if
                stmt_start = parser%current_token
                stmt_end = stmt_start
                
                ! Find end of statement (end of line)
                do i = stmt_start, size(all_tokens)
                    if (i > stmt_start .and. all_tokens(i)%line /= token%line) exit
                    stmt_end = i
                end do
                
                ! Extract statement tokens
                stmt_size = stmt_end - stmt_start + 1
                if (stmt_size > 0) then
                    allocate(stmt_tokens(stmt_size + 1))
                    stmt_tokens(1:stmt_size) = all_tokens(stmt_start:stmt_end)
                    ! Add EOF token
                    stmt_tokens(stmt_size + 1)%kind = TK_EOF
                    stmt_tokens(stmt_size + 1)%text = ""
                    stmt_tokens(stmt_size + 1)%line = token%line
                    stmt_tokens(stmt_size + 1)%column = token%column + 1
                    
                    ! Parse the statement - check for if statements first
                    block_parser = create_parser_state(stmt_tokens)
                    if (stmt_tokens(1)%kind == TK_KEYWORD .and. stmt_tokens(1)%text == "if") then
                        stmt_index = parse_if(block_parser, arena)
                    else
                        stmt_index = parse_statement_in_if_block(block_parser, arena, stmt_tokens(1))
                    end if
                    
                    ! Add to body
                    if (stmt_index > 0) then
                        body_indices = [body_indices, stmt_index]
                    end if
                    
                    ! Advance parser position
                    parser%current_token = stmt_end + 1
                end if
            end block
        end do

        ! Merge parameter attributes from body declarations
        if (allocated(param_indices) .and. allocated(body_indices)) then
            if (size(param_indices) > 0 .and. size(body_indices) > 0) then
                call merge_parameter_attributes(arena, param_indices, body_indices)
            end if
        end if
        
        ! Create function node
        func_index = push_function_def(arena, function_name, param_indices, &
                                       return_type_str, body_indices, &
                                       line, column, result_variable=result_variable_name)
    end function parse_function_definition

    function parse_subroutine_definition(parser, arena) result(sub_index)
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

        ! Parse parameters
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            token = parser%consume()
            call parse_typed_parameters(parser, arena, param_indices)
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()
            end if
        else
            allocate(param_indices(0))
        end if

        ! Parse subroutine body until "end subroutine"
        allocate(body_indices(0))

        do while (.not. parser%is_at_end())
            token = parser%peek()

            ! Check for end of subroutine
            if (token%kind == TK_KEYWORD .and. token%text == "end") then
                ! Look ahead for "subroutine"
                block
                    type(token_t), allocatable :: all_tokens(:)
                    integer :: next_idx
                    if (allocated(parser%tokens)) then
                        allocate(all_tokens(size(parser%tokens)))
                        all_tokens = parser%tokens
                    else
                        allocate(all_tokens(0))
                    end if
                    next_idx = parser%current_token + 1
                    if (next_idx <= size(all_tokens)) then
                        if (all_tokens(next_idx)%kind == TK_KEYWORD .and. &
                            all_tokens(next_idx)%text == "subroutine") then
                            ! Consume "end subroutine"
                            token = parser%consume()  ! consume "end"
                            token = parser%consume()  ! consume "subroutine"
                            ! Optionally consume subroutine name
                            if (.not. parser%is_at_end()) then
                                token = parser%peek()
                                if (token%kind == TK_IDENTIFIER .and. &
                                    token%text == subroutine_name) then
                                    token = parser%consume()
                                end if
                            end if
                            exit  ! Exit the body parsing loop
                        end if
                    end if
                end block
            end if

            ! Skip empty lines
            if (token%kind == TK_NEWLINE) then
                token = parser%consume()
                cycle
            end if

            ! Collect tokens for the current statement line
            block
                type(token_t), allocatable :: stmt_tokens(:), all_tokens(:)
                integer :: stmt_start, stmt_end, i, stmt_size, stmt_index
                type(parser_state_t) :: block_parser
                
                if (allocated(parser%tokens)) then
                    allocate(all_tokens(size(parser%tokens)))
                    all_tokens = parser%tokens
                else
                    allocate(all_tokens(0))
                end if
                stmt_start = parser%current_token
                stmt_end = stmt_start
                
                ! Find end of statement (end of line)
                do i = stmt_start, size(all_tokens)
                    if (i > stmt_start .and. all_tokens(i)%line /= token%line) exit
                    stmt_end = i
                end do
                
                ! Extract statement tokens
                stmt_size = stmt_end - stmt_start + 1
                if (stmt_size > 0) then
                    allocate(stmt_tokens(stmt_size + 1))
                    stmt_tokens(1:stmt_size) = all_tokens(stmt_start:stmt_end)
                    ! Add EOF token
                    stmt_tokens(stmt_size + 1)%kind = TK_EOF
                    stmt_tokens(stmt_size + 1)%text = ""
                    stmt_tokens(stmt_size + 1)%line = token%line
                    stmt_tokens(stmt_size + 1)%column = token%column + 1
                    
                    ! Parse the statement - check for if statements first
                    block_parser = create_parser_state(stmt_tokens)
                    if (stmt_tokens(1)%kind == TK_KEYWORD .and. stmt_tokens(1)%text == "if") then
                        stmt_index = parse_if(block_parser, arena)
                    else
                        stmt_index = parse_statement_in_if_block(block_parser, arena, stmt_tokens(1))
                    end if
                    
                    ! Add to body
                    if (stmt_index > 0) then
                        body_indices = [body_indices, stmt_index]
                    end if
                    
                    ! Advance parser position
                    parser%current_token = stmt_end + 1
                end if
            end block
        end do

        ! Merge parameter attributes from body declarations
        if (allocated(param_indices) .and. allocated(body_indices)) then
            if (size(param_indices) > 0 .and. size(body_indices) > 0) then
                call merge_parameter_attributes(arena, param_indices, body_indices)
            end if
        end if
        
        ! Create subroutine node
        sub_index = push_subroutine_def(arena, subroutine_name, param_indices, body_indices, &
                                        line, column)
    end function parse_subroutine_definition

    function parse_interface_block(parser, arena) result(interface_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: interface_index

        type(token_t) :: token
        character(len=:), allocatable :: interface_name
        integer :: line, column
        integer, allocatable :: body_indices(:)

        ! Consume interface keyword
        token = parser%consume()
        line = token%line
        column = token%column

        ! Get interface name (optional)
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            token = parser%consume()
            interface_name = token%text
        else
            interface_name = ""
        end if

        ! Simplified parsing for refactoring
        allocate(body_indices(0))

        ! Create interface node
        interface_index = push_interface_block(arena, interface_name, body_indices, &
                                               line, column)
    end function parse_interface_block

end module parser_definition_statements_module