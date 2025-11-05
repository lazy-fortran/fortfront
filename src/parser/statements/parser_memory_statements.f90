module parser_memory_statements_module
    ! Parser module for memory management statement types (allocate, deallocate)
    use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, TK_NUMBER, TK_STRING, &
                          TK_OPERATOR, TK_KEYWORD, TK_NEWLINE, TK_COMMENT, &
                          TK_WHITESPACE
    use parser_state_module
    use parser_expressions_module, only: parse_comparison
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_allocate, push_deallocate
    use ast_factory
    implicit none
    private

    public :: parse_allocate_statement, parse_deallocate_statement

    abstract interface
        subroutine list_item_handler_t(parser, arena, token, in_variable_list)
            import :: parser_state_t, ast_arena_t, token_t
            type(parser_state_t), intent(inout) :: parser
            type(ast_arena_t), intent(inout) :: arena
            type(token_t), intent(in) :: token
            logical, intent(inout) :: in_variable_list
        end subroutine list_item_handler_t
    end interface

contains

    subroutine parse_parenthesized_list(parser, arena, closing_text, &
                                        in_variable_list, handler)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: closing_text
        logical, intent(inout) :: in_variable_list
        procedure(list_item_handler_t) :: handler
        type(token_t) :: token

        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == closing_text) then
            token = parser%consume()
            return
        end if

        do
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == &
                closing_text) exit
            call handler(parser, arena, token, in_variable_list)
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()
            else
                exit
            end if
        end do

        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == closing_text) then
            token = parser%consume()
        end if
    end subroutine parse_parenthesized_list

    ! Helper subroutine to parse allocate variable with optional dimensions
    subroutine parse_allocate_variable(parser, arena, var_indices, &
                                       shape_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(inout) :: var_indices(:)
        integer, allocatable, intent(inout) :: shape_indices(:)

        type(token_t) :: token
        integer :: var_index

        ! Parse variable identifier
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            var_index = parse_comparison(parser, arena)
            var_indices = [var_indices, var_index]

            ! Check for array dimensions: var(size1, size2, ...)
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "(") then
                token = parser%consume()  ! consume '('

                ! Parse dimension expressions
                do
                    shape_indices = [shape_indices, &
                                     parse_comparison(parser, arena)]

                    token = parser%peek()
                    if (token%kind == TK_OPERATOR .and. token%text == ",") then
                        token = parser%consume()  ! consume ','
                    else
                        exit
                    end if
                end do

                ! Expect closing parenthesis
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ")") then
                    token = parser%consume()  ! consume ')'
                end if
            end if
        end if
    end subroutine parse_allocate_variable

    ! Parse allocate parameters (stat, errmsg, source, mold)
    subroutine parse_allocate_params(parser, arena, param_name, &
                                     stat_var_index, errmsg_var_index, &
                                     source_expr_index, mold_expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: param_name
        integer, intent(inout) :: stat_var_index, errmsg_var_index
        integer, intent(inout) :: source_expr_index, mold_expr_index
        type(token_t) :: token

        token = parser%consume()  ! consume parameter name
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "=") then
            token = parser%consume()  ! consume '='
            select case (param_name)
            case ("stat")
                stat_var_index = parse_comparison(parser, arena)
            case ("errmsg")
                errmsg_var_index = parse_comparison(parser, arena)
            case ("source")
                source_expr_index = parse_comparison(parser, arena)
            case ("mold")
                mold_expr_index = parse_comparison(parser, arena)
            end select
        end if
    end subroutine parse_allocate_params

    subroutine skip_to_matching_paren(parser, type_spec_str, type_token, &
                                      saved_pos)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: type_spec_str
        type(token_t), intent(in) :: type_token
        integer, intent(in) :: saved_pos
        type(token_t) :: token
        integer :: paren_depth
        integer :: spec_start_pos, spec_end_pos

        paren_depth = 1
        spec_start_pos = parser%current_token

        do while (paren_depth > 0)
            token = parser%peek()
            if (token%kind == TK_EOF) then
                parser%current_token = saved_pos
                return
            end if

            if (token%kind == TK_OPERATOR) then
                if (token%text == "(") then
                    paren_depth = paren_depth + 1
                else if (token%text == ")") then
                    paren_depth = paren_depth - 1
                    if (paren_depth == 0) then
                        spec_end_pos = parser%current_token
                        token = parser%consume()
                        token = parser%peek()
                        if (token%kind == TK_OPERATOR .and. &
                            token%text == "::") then
                            token = parser%consume()
                            call build_type_spec(parser, type_token, &
                                                 spec_start_pos, &
                                                 spec_end_pos, &
                                                 type_spec_str)
                            return
                        else
                            parser%current_token = saved_pos
                            return
                        end if
                    end if
                end if
            end if
            token = parser%consume()
        end do
    end subroutine skip_to_matching_paren

    subroutine build_type_spec(parser, type_token, start_pos, end_pos, &
                               type_spec_str)
        type(parser_state_t), intent(inout) :: parser
        type(token_t), intent(in) :: type_token
        integer, intent(in) :: start_pos, end_pos
        character(len=:), allocatable, intent(out) :: type_spec_str
        integer :: i, saved_current
        type(token_t) :: token
        character(len=1024) :: buffer

        buffer = ""
        saved_current = parser%current_token

        parser%current_token = start_pos
        do i = start_pos, end_pos - 1
            token = parser%peek()
            if (len_trim(buffer) > 0 .and. token%kind /= TK_OPERATOR) then
                buffer = trim(buffer) // " "
            end if
            buffer = trim(buffer) // trim(token%text)
            token = parser%consume()
        end do

        type_spec_str = trim(type_token%text) // "(" // trim(buffer) // ")"
        parser%current_token = saved_current
    end subroutine build_type_spec

    function parse_allocate_statement(parser, arena) result(allocate_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: allocate_index

        type(token_t) :: token, lookahead, type_token
        integer, allocatable :: var_indices(:)
        integer, allocatable :: shape_indices(:)
        integer :: stat_var_index, errmsg_var_index, source_expr_index, &
                   mold_expr_index
        integer :: line, column, saved_pos
        logical :: in_variable_list
        character(len=:), allocatable :: type_spec_str

        ! Initialize
        stat_var_index = 0
        errmsg_var_index = 0
        source_expr_index = 0
        mold_expr_index = 0
        line = 0
        column = 0
        in_variable_list = .true.
        ! Initialize with proper empty arrays that can grow
        allocate (var_indices(0))
        allocate (shape_indices(0))

        ! Consume 'allocate' keyword
        token = parser%consume()
        line = token%line
        column = token%column

        ! Expect opening parenthesis
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            token = parser%consume()  ! consume '('

            ! Check for type-spec: TypeName :: or TypeName(param) ::
            token = parser%peek()
            if (token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) then
                ! Save current position
                saved_pos = parser%current_token
                type_token = parser%consume()
                lookahead = parser%peek()
                if (lookahead%kind == TK_OPERATOR .and. lookahead%text &
                    == "(") then
                    ! TypeName(params) :: pattern - skip parameter list
                    token = parser%consume()
                    call skip_to_matching_paren(parser, type_spec_str, &
                                                type_token, saved_pos)
                else if (lookahead%kind == TK_OPERATOR .and. &
                         lookahead%text == "::") then
                    ! TypeName :: pattern - consume ::
                    token = parser%consume()
                    type_spec_str = trim(type_token%text)
                else
                    ! Not type-spec, restore position
                    parser%current_token = saved_pos
                end if
            end if

            call parse_parenthesized_list(parser, arena, ")", &
                                          in_variable_list, &
                                          process_allocate_item)
        end if

        ! Create allocate statement node
        if (allocated(type_spec_str)) then
            allocate_index = push_allocate(arena, var_indices, shape_indices, &
                                           stat_var_index, &
                                           errmsg_var_index, &
                                           source_expr_index, &
                                           mold_expr_index, &
                                           line, column, &
                                           type_spec=type_spec_str)
        else
            allocate_index = push_allocate(arena, var_indices, shape_indices, &
                                           stat_var_index, &
                                           errmsg_var_index, &
                                           source_expr_index, &
                                           mold_expr_index, &
                                           line, column)
        end if
    contains
        subroutine process_allocate_item(local_parser, local_arena, token, &
                                         in_variable_list)
            type(parser_state_t), intent(inout) :: local_parser
            type(ast_arena_t), intent(inout) :: local_arena
            type(token_t), intent(in) :: token
            logical, intent(inout) :: in_variable_list

            type(token_t) :: discard

            select case (token%kind)
            case (TK_IDENTIFIER)
                select case (token%text)
                case ("stat", "errmsg", "source", "mold")
                    call parse_allocate_params(local_parser, local_arena, &
                                               token%text, &
                                               stat_var_index, &
                                               errmsg_var_index, &
                                               source_expr_index, &
                                               mold_expr_index)
                    in_variable_list = .false.
                case default
                    if (in_variable_list) then
                        call parse_allocate_variable(local_parser, &
                                                     local_arena, &
                                                     var_indices, &
                                                     shape_indices)
                    else
                        discard = local_parser%consume()
                    end if
                end select
            case default
                if (in_variable_list) then
                    call parse_allocate_variable(local_parser, local_arena, &
                                                 var_indices, shape_indices)
                else
                    discard = local_parser%consume()
                end if
            end select
        end subroutine process_allocate_item
    end function parse_allocate_statement

    function parse_deallocate_statement(parser, arena) result(deallocate_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: deallocate_index

        type(token_t) :: token
        integer, allocatable :: var_indices(:)
        integer :: stat_var_index, errmsg_var_index, line, column
        logical :: in_variable_list

        ! Initialize
        stat_var_index = 0
        errmsg_var_index = 0
        line = 0
        column = 0
        in_variable_list = .true.
        allocate (var_indices(0))

        ! Consume 'deallocate' keyword
        token = parser%consume()
        line = token%line
        column = token%column

        ! Expect opening parenthesis
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            token = parser%consume()  ! consume '('

            call parse_parenthesized_list(parser, arena, ")", &
                                          in_variable_list, &
                                          process_deallocate_item)
        end if

        ! Create deallocate statement node
        deallocate_index = push_deallocate(arena, var_indices, &
                                           stat_var_index, &
                                           errmsg_var_index, &
                                           line, column)
    contains
        subroutine process_deallocate_item(local_parser, local_arena, token, &
                                           in_variable_list)
            type(parser_state_t), intent(inout) :: local_parser
            type(ast_arena_t), intent(inout) :: local_arena
            type(token_t), intent(in) :: token
            logical, intent(inout) :: in_variable_list

            type(token_t) :: discard

            select case (token%kind)
            case (TK_IDENTIFIER)
                select case (token%text)
                case ("stat")
                    call consume_assignment(local_parser, local_arena, &
                                            stat_var_index)
                    in_variable_list = .false.
                case ("errmsg")
                    call consume_assignment(local_parser, local_arena, &
                                            errmsg_var_index)
                    in_variable_list = .false.
                case default
                    if (in_variable_list) then
                        var_indices = [var_indices, &
                                       parse_comparison(local_parser, &
                                                        local_arena)]
                    else
                        discard = local_parser%consume()
                    end if
                end select
            case default
                if (in_variable_list) then
                    var_indices = [var_indices, &
                                   parse_comparison(local_parser, &
                                                    local_arena)]
                else
                    discard = local_parser%consume()
                end if
            end select
        end subroutine process_deallocate_item

        subroutine consume_assignment(local_parser, local_arena, target_index)
            type(parser_state_t), intent(inout) :: local_parser
            type(ast_arena_t), intent(inout) :: local_arena
            integer, intent(inout) :: target_index
            type(token_t) :: lookahead

            lookahead = local_parser%consume()
            lookahead = local_parser%peek()
            if (lookahead%kind == TK_OPERATOR .and. lookahead%text == "=") then
                lookahead = local_parser%consume()
                target_index = parse_comparison(local_parser, local_arena)
            end if
        end subroutine consume_assignment
    end function parse_deallocate_statement

end module parser_memory_statements_module
