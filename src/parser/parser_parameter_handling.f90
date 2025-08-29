module parser_parameter_handling_module
    ! Parser module for parameter handling and typed parameter parsing
    use lexer_core
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_declarations, only: parse_declaration
    use parser_expressions_module, only: parse_comparison
    use ast_core
    use ast_factory
    use ast_types, only: LITERAL_STRING, LITERAL_INTEGER
    use ast_nodes_data, only: declaration_node, parameter_declaration_node, &
                              INTENT_IN, INTENT_OUT, INTENT_INOUT
    implicit none
    private

    public :: merge_parameter_attributes, parse_typed_parameters

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

end module parser_parameter_handling_module