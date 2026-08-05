subroutine ensure_if_do_registration_bridge()
    ! Keep this as an external subroutine to avoid a module dependency cycle
    ! between IF parsing and DO construct parsing.
    use parser_do_constructs_module, only: ensure_if_do_registration
    implicit none

    call ensure_if_do_registration()

end subroutine ensure_if_do_registration_bridge

recursive function parse_block_construct_bridge(parser, arena) result(block_index)
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use parser_array_constructs_module, only: parse_block_construct
    implicit none

    type(parser_state_t), intent(inout) :: parser
    type(ast_arena_t), intent(inout) :: arena
    integer :: block_index

    block_index = parse_block_construct(parser, arena)
end function parse_block_construct_bridge
