module parser_parameter_statements_module
    use lexer_core, only: token_t, TK_IDENTIFIER, TK_KEYWORD, TK_OPERATOR, &
        & TK_WHITESPACE, TK_COMMENT, TK_NEWLINE, TK_NUMBER, TK_STRING, to_lower
    use parser_state_module, only: parser_state_t
    use parser_expressions_module, only: parse_expression
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: declaration_node
    use ast_factory, only: push_declaration
    implicit none
    private

    public :: parse_parameter_statement

contains

    logical function parse_parameter_statement(parser, arena) result(success)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t) :: token
        character(len=:), allocatable :: lowered_keyword

        success = .false.
        if (parser%is_at_end()) return

        call skip_trivia(parser)

        if (.not. parser%is_at_end()) then
            token = parser%peek()
            if (token%kind == TK_KEYWORD) then
                lowered_keyword = to_lower(token%text)
                if (trim(lowered_keyword) == "parameter") then
                    token = parser%consume()
                    call skip_trivia(parser)
                end if
            end if
        end if

        if (parser%is_at_end()) return

        token = parser%peek()
        if (token%kind /= TK_OPERATOR .or. token%text /= "(") return
        token = parser%consume()
        call skip_trivia(parser)

        do
            if (parser%is_at_end()) exit

            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()
                exit
            end if

            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()
                call skip_trivia(parser)
                cycle
            end if

            if (.not. token_is_identifier(token)) exit

            if (parse_single_parameter_def(parser, arena)) then
                success = .true.
            end if

            call skip_trivia(parser)
        end do
    end function parse_parameter_statement

    logical function parse_single_parameter_def(parser, arena) result(applied)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t) :: token
        type(token_t), allocatable :: expr_tokens(:)
        character(len=:), allocatable :: var_name
        integer :: value_index, paren_depth, start_pos, end_pos, tok_count

        applied = .false.
        if (parser%is_at_end()) return

        token = parser%peek()
        if (.not. token_is_identifier(token)) return
        token = parser%consume()
        var_name = adjustl(trim(token%text))

        call skip_trivia(parser)
        if (parser%is_at_end()) return

        token = parser%peek()
        if (token%kind /= TK_OPERATOR .or. token%text /= "=") return
        token = parser%consume()
        call skip_trivia(parser)

        if (parser%is_at_end()) return

        start_pos = parser%current_token
        paren_depth = 0
        do while (parser%current_token <= size(parser%tokens))
            token = parser%tokens(parser%current_token)
            if (token%kind == TK_OPERATOR) then
                select case (trim(token%text))
                case ("(", "[", "(/")
                    paren_depth = paren_depth + 1
                case (")", "]", "/)")
                    if (paren_depth == 0) then
                        exit
                    else
                        paren_depth = paren_depth - 1
                    end if
                case (",")
                    if (paren_depth == 0) exit
                end select
            else if (token%kind == TK_NEWLINE) then
                exit
            end if
            parser%current_token = parser%current_token + 1
        end do
        end_pos = parser%current_token - 1

        if (end_pos < start_pos) return

        tok_count = end_pos - start_pos + 1
        allocate (expr_tokens(tok_count + 1))
        expr_tokens(1:tok_count) = parser%tokens(start_pos:end_pos)
        expr_tokens(tok_count + 1)%kind = 0
        expr_tokens(tok_count + 1)%text = ""

        value_index = parse_expression(expr_tokens, arena)
        if (value_index <= 0) return

        applied = apply_parameter_to_variable(arena, var_name, value_index)

        if (.not. applied) then
            applied = create_parameter_declaration(arena, var_name, value_index)
        end if
    end function parse_single_parameter_def

    subroutine skip_trivia(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        do while (.not. parser%is_at_end())
            token = parser%peek()
            select case (token%kind)
            case (TK_WHITESPACE, TK_COMMENT, TK_NEWLINE)
                token = parser%consume()
            case default
                exit
            end select
        end do
    end subroutine skip_trivia

    logical function token_is_identifier(token) result(is_ident)
        type(token_t), intent(in) :: token
        is_ident = (token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD)
    end function token_is_identifier

    logical function apply_parameter_to_variable(arena, name, value_index) &
        result(applied)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in) :: value_index
        character(len=:), allocatable :: target, decl_name
        integer :: idx

        applied = .false.
        target = to_lower(adjustl(trim(name)))

        do idx = arena%size, 1, -1
            if (.not. allocated(arena%entries(idx)%node)) cycle
            select type (decl => arena%entries(idx)%node)
            type is (declaration_node)
                if (decl%is_multi_declaration .and. allocated(decl%var_names)) then
                    if (apply_parameter_multi(arena, idx, decl, target, &
                                              value_index)) then
                        applied = .true.
                        return
                    end if
                end if
                if (allocated(decl%var_name)) then
                    decl_name = to_lower(trim(decl%var_name))
                    if (decl_name == target) then
                        decl%is_parameter = .true.
                        decl%has_initializer = .true.
                        decl%initializer_index = value_index
                        arena%entries(idx)%node = decl
                        applied = .true.
                        return
                    end if
                end if
            end select
        end do
    end function apply_parameter_to_variable

    logical function apply_parameter_multi(arena, decl_index, decl, target, &
                                           value_index) result(updated)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: decl_index
        type(declaration_node), intent(inout) :: decl
        character(len=*), intent(in) :: target
        integer, intent(in) :: value_index
        integer :: i
        character(len=:), allocatable :: decl_name

        updated = .false.
        if (.not. allocated(decl%var_names)) return

        do i = 1, size(decl%var_names)
            decl_name = to_lower(trim(decl%var_names(i)))
            if (decl_name == target) then
                decl%is_parameter = .true.
                decl%has_initializer = .true.
                decl%initializer_index = value_index
                arena%entries(decl_index)%node = decl
                updated = .true.
                return
            end if
        end do
    end function apply_parameter_multi

    logical function create_parameter_declaration(arena, name, value_index) &
        result(created)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in) :: value_index
        integer :: decl_index
        character(len=len(name)) :: names(1)

        created = .false.
        names(1) = name
        decl_index = push_declaration(arena, type_name="", names=names, &
                                      is_parameter=.true., &
                                      initializer_index=value_index)
        if (decl_index > 0) created = .true.
    end function create_parameter_declaration

end module parser_parameter_statements_module
