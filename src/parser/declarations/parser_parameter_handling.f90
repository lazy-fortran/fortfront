module parser_parameter_handling_module
    ! Parser module for parameter handling and typed parameter parsing
    use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, TK_NUMBER, TK_STRING, &
                          TK_OPERATOR, TK_KEYWORD, TK_NEWLINE, TK_COMMENT, &
                          TK_WHITESPACE
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_declarations, only: parse_declaration
    use parser_expressions_module, only: parse_comparison
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_parameter_declaration, push_identifier, push_literal
    use ast_factory
    use ast_types, only: LITERAL_INTEGER
    use ast_nodes_data, only: declaration_node, parameter_declaration_node, &
                              INTENT_NONE, INTENT_IN, INTENT_OUT, INTENT_INOUT
    use parser_utilities, only: consume_token
    use string_utils_mod, only: to_lower
    implicit none
    private

    type :: parameter_type_info_t
        character(len=:), allocatable :: type_name
        integer :: kind_value = 0
        character(len=:), allocatable :: character_length_expr
        integer :: intent_value = INTENT_NONE
        logical :: is_optional = .false.
        logical :: is_target = .false.
        logical :: is_unsigned = .false.
        integer :: line = 0
        integer :: column = 0
    end type parameter_type_info_t

    public :: merge_parameter_attributes, parse_typed_parameters

contains

    subroutine update_parameter_from_declaration(param_node, body_node)
        type(parameter_declaration_node), intent(inout) :: param_node
        type(declaration_node), intent(in) :: body_node

        ! Update intent
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

        ! Update optional flag
        param_node%is_optional = body_node%is_optional

        ! Update target flag
        param_node%is_target = body_node%is_target
        param_node%is_unsigned = body_node%is_unsigned

        ! Update type if not already set
        if (param_node%type_name == "" .and. allocated(body_node%type_name)) then
            param_node%type_name = body_node%type_name
            param_node%kind_value = body_node%kind_value
            param_node%has_kind = body_node%has_kind
        end if
    end subroutine update_parameter_from_declaration

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

            ! CRITICAL FIX for Issue #1121: Check if node is allocated before accessing
            if (.not. allocated(arena%entries(param_indices(i))%node)) cycle

            select type (param_node => arena%entries(param_indices(i))%node)
            type is (parameter_declaration_node)
                param_name = param_node%name

                ! Look for corresponding declaration in body
                do j = 1, size(body_indices)
                    if (body_indices(j) <= 0 .or. body_indices(j) > arena%size) cycle

             ! CRITICAL FIX for Issue #1121: Check if node is allocated before accessing
                    if (.not. allocated(arena%entries(body_indices(j))%node)) cycle

                    select type (body_node => arena%entries(body_indices(j))%node)
                    type is (declaration_node)
                        ! Check if this declaration is for the parameter
                        if (body_node%is_multi_declaration) then
                            ! Check multi-declaration var_names
                            if (allocated(body_node%var_names)) then
                                if (any(body_node%var_names == param_name)) then
                                    call &
                                        update_parameter_from_declaration(param_node, &
                                                                          body_node)
                                end if
                            end if
                        else
                            ! Single declaration
                            if (allocated(body_node%var_name) .and. &
                                body_node%var_name == param_name) then
                                call &
                                    update_parameter_from_declaration(param_node, &
                                                                      body_node)
                            end if
                        end if
                    end select
                end do
            end select
        end do
    end subroutine merge_parameter_attributes

    ! Parse typed parameters from a procedure header
    subroutine parse_typed_parameters(parser, arena, param_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(out) :: param_indices(:)
        type(token_t) :: token

        allocate (param_indices(0))

        do while (.not. parser%is_at_end())
            token = parser%peek()

            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                exit
            end if

            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                call consume_token(parser)
                cycle
            end if

            if (token%kind == TK_KEYWORD .and. is_type_keyword_token(token)) then
                call parse_parameter_group(parser, arena, param_indices)
                cycle
            end if

            if (token%kind == TK_IDENTIFIER) then
                call append_untyped_parameter(parser, arena, param_indices)
                cycle
            end if

            if (token%kind == TK_KEYWORD .and. can_keyword_be_identifier(token)) then
                call append_untyped_parameter(parser, arena, param_indices)
                cycle
            end if

            call consume_token(parser)
        end do
    end subroutine parse_typed_parameters

    logical function is_type_keyword_token(token) result(is_type_keyword)
        type(token_t), intent(in) :: token
        is_type_keyword = token%text == "real" .or. token%text == "integer" .or. &
                          token%text == "logical" .or. token%text == "character" .or. &
                          token%text == "type" .or. token%text == "complex" .or. &
                          token%text == "class" .or. token%text == "procedure"
    end function is_type_keyword_token

    logical function can_keyword_be_identifier(token) result(can_be_id)
        type(token_t), intent(in) :: token
        character(len=:), allocatable :: lower_text

        lower_text = to_lower(token%text)
        can_be_id = lower_text == "stop" .or. lower_text == "pause" .or. &
                    lower_text == "cycle" .or. lower_text == "exit" .or. &
                    lower_text == "return" .or. lower_text == "continue"
    end function can_keyword_be_identifier

    subroutine parse_parameter_group(parser, arena, param_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(inout) :: param_indices(:)
        type(parameter_type_info_t) :: info

        call initialize_type_info(parser, info)
        call parse_type_suffix(parser, info)
        call parse_type_attributes(parser, info)
        call consume_double_colon(parser)
        call collect_parameter_group(parser, arena, info, param_indices)
    end subroutine parse_parameter_group

    subroutine initialize_type_info(parser, info)
        type(parser_state_t), intent(inout) :: parser
        type(parameter_type_info_t), intent(out) :: info
        type(token_t) :: type_token

        type_token = parser%consume()
        info%type_name = type_token%text
        info%kind_value = 0
        info%intent_value = INTENT_NONE
        info%is_optional = .false.
        info%is_target = .false.
        info%is_unsigned = .false.
        info%line = type_token%line
        info%column = type_token%column
    end subroutine initialize_type_info

    subroutine parse_type_suffix(parser, info)
        type(parser_state_t), intent(inout) :: parser
        type(parameter_type_info_t), intent(inout) :: info
        type(token_t) :: token
        character(len=:), allocatable :: type_expr
        integer :: paren_count
        integer :: io_status

        token = parser%peek()
        if (token%kind /= TK_OPERATOR .or. token%text /= "(") then
            return
        end if

        call consume_token(parser)
        if (to_lower(info%type_name) == "type" .or. to_lower(info%type_name) == &
            "class") then
            type_expr = ""
            paren_count = 1
            do while (.not. parser%is_at_end() .and. paren_count > 0)
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == "(") then
                    paren_count = paren_count + 1
                    type_expr = type_expr // token%text
                    call consume_token(parser)
                else if (token%kind == TK_OPERATOR .and. token%text == ")") then
                    paren_count = paren_count - 1
                    if (paren_count > 0) type_expr = type_expr // token%text
                    call consume_token(parser)
                else
                    type_expr = type_expr // token%text
                    call consume_token(parser)
                end if
            end do
            info%type_name = info%type_name // "(" // type_expr // ")"
        else if (to_lower(info%type_name) == "character") then
            type_expr = ""
            paren_count = 1
            do while (.not. parser%is_at_end() .and. paren_count > 0)
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == "(") then
                    paren_count = paren_count + 1
                    type_expr = type_expr // token%text
                    call consume_token(parser)
                else if (token%kind == TK_OPERATOR .and. token%text == ")") then
                    paren_count = paren_count - 1
                    if (paren_count > 0) type_expr = type_expr // token%text
                    call consume_token(parser)
                else
                    type_expr = type_expr // token%text
                    call consume_token(parser)
                end if
            end do
            info%character_length_expr = type_expr
            info%type_name = info%type_name // "(" // type_expr // ")"
        else
            token = parser%peek()
            if (token%kind == TK_NUMBER) then
                read (token%text, *, iostat=io_status) info%kind_value
                if (io_status /= 0) info%kind_value = 0
                call consume_token(parser)
            end if
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                call consume_token(parser)
            end if
        end if
    end subroutine parse_type_suffix

    subroutine parse_type_attributes(parser, info)
        type(parser_state_t), intent(inout) :: parser
        type(parameter_type_info_t), intent(inout) :: info
        type(token_t) :: token

        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind /= TK_OPERATOR .or. token%text /= ",") exit
            call consume_token(parser)

            token = parser%peek()
            if (token%kind == TK_KEYWORD .or. token%kind == TK_IDENTIFIER) then
                select case (token%text)
                case ("intent")
                    call consume_token(parser)
                    call parse_intent_attribute(parser, info)
                case ("dimension")
                    call consume_token(parser)
                    call skip_balanced_parentheses(parser)
                case ("optional")
                    info%is_optional = .true.
                    call consume_token(parser)
                case ("target")
                    info%is_target = .true.
                    call consume_token(parser)
                case ("unsigned")
                    info%is_unsigned = .true.
                    call consume_token(parser)
                case default
                    call consume_token(parser)
                end select
            else
                call consume_token(parser)
            end if
        end do
    end subroutine parse_type_attributes

    subroutine parse_intent_attribute(parser, info)
        type(parser_state_t), intent(inout) :: parser
        type(parameter_type_info_t), intent(inout) :: info
        type(token_t) :: token

        token = parser%peek()
        if (token%kind /= TK_OPERATOR .or. token%text /= "(") then
            return
        end if
        call consume_token(parser)

        token = parser%peek()
        if (token%kind == TK_KEYWORD .or. token%kind == TK_IDENTIFIER) then
            select case (token%text)
            case ("in")
                info%intent_value = INTENT_IN
            case ("out")
                info%intent_value = INTENT_OUT
            case ("inout")
                info%intent_value = INTENT_INOUT
            end select
            call consume_token(parser)
        end if

        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == ")") then
            call consume_token(parser)
        end if
    end subroutine parse_intent_attribute

    subroutine skip_balanced_parentheses(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token
        integer :: depth

        token = parser%peek()
        if (token%kind /= TK_OPERATOR .or. token%text /= "(") then
            return
        end if

        depth = 0
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_OPERATOR) then
                if (token%text == "(") depth = depth + 1
                if (token%text == ")") then
                    depth = depth - 1
                    call consume_token(parser)
                    if (depth <= 0) exit
                    cycle
                end if
            end if
            call consume_token(parser)
            if (depth <= 0) exit
        end do
    end subroutine skip_balanced_parentheses

    subroutine consume_double_colon(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "::") then
            call consume_token(parser)
        end if
    end subroutine consume_double_colon

    subroutine collect_parameter_group(parser, arena, info, param_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(parameter_type_info_t), intent(in) :: info
        integer, allocatable, intent(inout) :: param_indices(:)
        integer, allocatable :: group_indices(:)

        allocate (group_indices(0))

        do while (.not. parser%is_at_end())
            if (.not. next_token_is_identifier(parser)) exit
            group_indices = [group_indices, parse_single_parameter(parser, &
                                                                   arena, info)]

            if (.not. comma_continues_group(parser)) exit
        end do

        if (size(group_indices) > 0) then
            param_indices = [param_indices, group_indices]
        end if
    end subroutine collect_parameter_group

    logical function next_token_is_identifier(parser) result(is_identifier)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token
        token = parser%peek()
        is_identifier = token%kind == TK_IDENTIFIER
    end function next_token_is_identifier

    logical function comma_continues_group(parser) result(continues)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token
        token = parser%peek()
        if (token%kind /= TK_OPERATOR .or. token%text /= ",") then
            continues = .false.
            return
        end if

        if (parser%current_token + 1 > size(parser%tokens)) then
            call consume_token(parser)
            continues = .true.
            return
        end if

        select case (parser%tokens(parser%current_token + 1)%text)
        case ("real", "integer", "logical", "character", "type", "class", &
              "complex", "procedure")
            continues = .false.
        case default
            call consume_token(parser)
            continues = .true.
        end select
    end function comma_continues_group

    integer function parse_single_parameter(parser, arena, info) result(param_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(parameter_type_info_t), intent(in) :: info
        type(token_t) :: token
        character(len=:), allocatable :: param_name
        integer, allocatable :: dim_indices(:)
        character(len=:), allocatable :: length_expr

        token = parser%peek()
        param_name = token%text
        call consume_token(parser)

        allocate (dim_indices(0))
        call parse_dimension_list(parser, arena, dim_indices)

        if (allocated(info%character_length_expr)) then
            if (len_trim(info%character_length_expr) > 0) then
                length_expr = trim(info%character_length_expr)
            end if
        end if

        if (size(dim_indices) > 0) then
            if (allocated(length_expr)) then
                param_index = push_parameter_declaration( &
                              arena, param_name, &
                              info%type_name, &
                              info%kind_value, &
                              info%intent_value, &
                              info%is_optional, &
                              info%is_target, &
                              info%is_unsigned, &
                              dim_indices, &
                              line=info%line, &
                              column=info%column, &
                              character_length_expr=length_expr)
            else
                param_index = push_parameter_declaration( &
                              arena, param_name, &
                              info%type_name, &
                              info%kind_value, &
                              info%intent_value, &
                              info%is_optional, &
                              info%is_target, &
                              info%is_unsigned, &
                              dim_indices, &
                              line=info%line, &
                              column=info%column)
            end if
        else
            if (allocated(length_expr)) then
                param_index = push_parameter_declaration( &
                              arena, name=param_name, &
                              type_name=info%type_name, &
                              kind_value=info%kind_value, &
                              intent_value=info%intent_value, &
                              is_optional=info%is_optional, &
                              is_target=info%is_target, &
                              is_unsigned=info%is_unsigned, &
                              line=info%line, &
                              column=info%column, &
                              character_length_expr=length_expr)
            else
                param_index = push_parameter_declaration( &
                              arena, name=param_name, &
                              type_name=info%type_name, &
                              kind_value=info%kind_value, &
                              intent_value=info%intent_value, &
                              is_optional=info%is_optional, &
                              is_target=info%is_target, &
                              is_unsigned=info%is_unsigned, &
                              line=info%line, &
                              column=info%column)
            end if
        end if
    end function parse_single_parameter

    subroutine parse_dimension_list(parser, arena, dim_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(inout) :: dim_indices(:)
        type(token_t) :: token

        token = parser%peek()
        if (token%kind /= TK_OPERATOR .or. token%text /= "(") then
            return
        end if

        call consume_token(parser)
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                call consume_token(parser)
                exit
            end if

            call append_dimension_entry(parser, arena, dim_indices)

            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                call consume_token(parser)
            end if
        end do
    end subroutine parse_dimension_list

    subroutine append_dimension_entry(parser, arena, dim_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(inout) :: dim_indices(:)
        type(token_t) :: token
        integer :: dim_index

        token = parser%peek()
        select case (token%kind)
        case (TK_OPERATOR)
            select case (token%text)
            case (":", "*")
                dim_index = push_identifier(arena, token%text, token%line, &
                                            token%column)
                dim_indices = [dim_indices, dim_index]
                call consume_token(parser)
                return
            end select
        case (TK_NUMBER)
            dim_index = push_literal(arena, token%text, LITERAL_INTEGER, token%line, &
                                     token%column)
            dim_indices = [dim_indices, dim_index]
            call consume_token(parser)
            return
        case (TK_IDENTIFIER)
            dim_index = push_identifier(arena, token%text, token%line, token%column)
            dim_indices = [dim_indices, dim_index]
            call consume_token(parser)
            return
        end select

        call consume_token(parser)
    end subroutine append_dimension_entry

    subroutine append_untyped_parameter(parser, arena, param_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(inout) :: param_indices(:)
        type(token_t) :: token
        integer :: param_index

        token = parser%consume()
        param_index = push_parameter_declaration(arena, name=token%text, &
                                                 type_name="", &
                                                 kind_value=0, &
                                                 intent_value=INTENT_NONE, &
                                                 is_optional=.false., &
                                                 is_target=.false., &
                                                 line=token%line, &
                                                 column=token%column)
        param_indices = [param_indices, param_index]
    end subroutine append_untyped_parameter

end module parser_parameter_handling_module
