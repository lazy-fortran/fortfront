module parser_control_flow_router_module
    use lexer_core, only: token_t, TK_KEYWORD, TK_WHITESPACE, TK_COMMENT, &
        TK_NEWLINE, to_lower
    use parser_state_module, only: parser_state_t
    use parser_statement_callbacks_module, only: statement_callbacks_t, &
        null_statement_callbacks, &
        parse_without_parent_interface
    use ast_arena_modern, only: ast_arena_t
    use parser_if_constructs_module, only: parse_if
    use parser_do_constructs_module, only: parse_do_loop
    use parser_select_constructs_module, only: parse_select_case, parse_select_type, &
        parse_select_rank
    use parser_array_constructs_module, only: parse_where_construct, parse_associate, &
        parse_block_construct
    use parser_forall_module, only: parse_forall
    implicit none
    private

    public :: route_control_flow
    public :: is_control_flow_keyword
    public :: default_control_flow_callbacks

contains

    pure function default_control_flow_callbacks() result(callbacks)
        type(statement_callbacks_t) :: callbacks
        callbacks = null_statement_callbacks()
        callbacks%parse_if => parse_if
        callbacks%parse_do_loop => parse_do_loop
        callbacks%parse_select_case => parse_select_case
        callbacks%parse_select_type => parse_select_type
        callbacks%parse_select_rank => parse_select_rank
        callbacks%parse_where => parse_where_construct
        callbacks%parse_forall => parse_forall
        callbacks%parse_associate => parse_associate
        callbacks%parse_block => parse_block_construct
    end function default_control_flow_callbacks

    logical function is_control_flow_keyword(text) result(is_control)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: lowered

        lowered = to_lower(trim(text))
        select case (lowered)
        case ("if", "do", "select", "where", "forall", "associate", "block")
            is_control = .true.
        case default
            is_control = .false.
        end select
    end function is_control_flow_keyword

    function route_control_flow(parser, arena, callbacks, parent_index) &
            result(node_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(statement_callbacks_t), intent(in), optional :: callbacks
        integer, intent(in), optional :: parent_index
        integer :: node_index
        type(statement_callbacks_t) :: local_callbacks
        type(token_t) :: token
        character(len=:), allocatable :: lowered

        node_index = 0
        if (present(callbacks)) then
            local_callbacks = callbacks
        else
            local_callbacks = default_control_flow_callbacks()
        end if

        token = parser%peek()
        if (token%kind /= TK_KEYWORD) return
        lowered = to_lower(trim(token%text))

        select case (lowered)
        case ("if")
            node_index = invoke_if(local_callbacks, parser, arena, parent_index)
        case ("do")
            node_index = invoke_no_parent(local_callbacks%parse_do_loop, parser, arena)
        case ("select")
            ! Look ahead to distinguish SELECT CASE / SELECT TYPE / SELECT RANK
            ! Skip whitespace and comments to find the next keyword
            block
                integer :: lookahead_idx
                type(token_t) :: lookahead_token
                logical :: found_keyword

                found_keyword = .false.
                lookahead_idx = parser%current_token + 1

                do while (lookahead_idx <= size(parser%tokens))
                    lookahead_token = parser%tokens(lookahead_idx)
                    if (lookahead_token%kind == TK_WHITESPACE .or. &
                        lookahead_token%kind == TK_COMMENT .or. &
                        lookahead_token%kind == TK_NEWLINE) then
                        lookahead_idx = lookahead_idx + 1
                        cycle
                    else if (lookahead_token%kind == TK_KEYWORD) then
                        found_keyword = .true.
                        exit
                    else
                        exit
                    end if
                end do

                if (found_keyword) then
                    if (lookahead_token%text == "type") then
                        node_index = &
                            invoke_no_parent(local_callbacks%parse_select_type, &
                            parser, arena)
                    else if (lookahead_token%text == "case") then
                        node_index = &
                            invoke_no_parent(local_callbacks%parse_select_case, &
                            parser, arena)
                    else if (lookahead_token%text == "rank") then
                        node_index = &
                            invoke_no_parent(local_callbacks%parse_select_rank, &
                            parser, arena)
                    end if
                end if
            end block
        case ("where")
            node_index = invoke_no_parent(local_callbacks%parse_where, parser, arena)
        case ("forall")
            node_index = invoke_no_parent(local_callbacks%parse_forall, parser, arena)
        case ("associate")
            node_index = invoke_no_parent(local_callbacks%parse_associate, &
                parser, arena)
        case ("block")
            node_index = invoke_no_parent(local_callbacks%parse_block, parser, arena)
        case default
            node_index = 0
        end select
    end function route_control_flow

    function invoke_if(callbacks, parser, arena, parent_index) result(node_index)
        type(statement_callbacks_t), intent(in) :: callbacks
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        integer :: node_index

        node_index = 0
        if (.not. associated(callbacks%parse_if)) return
        if (present(parent_index)) then
            node_index = callbacks%parse_if(parser, arena, parent_index)
        else
            node_index = callbacks%parse_if(parser, arena)
        end if
    end function invoke_if

    function invoke_no_parent(proc, parser, arena) result(node_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        procedure(parse_without_parent_interface), pointer, intent(in) :: proc
        integer :: node_index

        if (.not. associated(proc)) then
            node_index = 0
        else
            node_index = proc(parser, arena)
        end if
    end function invoke_no_parent

end module parser_control_flow_router_module
