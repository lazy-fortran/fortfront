module parser_statement_callbacks_module
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    implicit none
    private

    abstract interface
        recursive function parse_with_parent_interface(parser, arena, parent_index) &
                result(node_index)
            import :: parser_state_t, ast_arena_t
            type(parser_state_t), intent(inout) :: parser
            type(ast_arena_t), intent(inout) :: arena
            integer, intent(in), optional :: parent_index
            integer :: node_index
        end function parse_with_parent_interface

        recursive function parse_without_parent_interface(parser, arena) &
                result(node_index)
            import :: parser_state_t, ast_arena_t
            type(parser_state_t), intent(inout) :: parser
            type(ast_arena_t), intent(inout) :: arena
            integer :: node_index
        end function parse_without_parent_interface
    end interface

    type, public :: statement_callbacks_t
        procedure(parse_with_parent_interface), pointer, nopass :: parse_if => null()
        procedure(parse_without_parent_interface), pointer, nopass :: &
            parse_do_loop => null()
        procedure(parse_without_parent_interface), pointer, nopass :: &
            parse_select_case => null()
        procedure(parse_without_parent_interface), pointer, nopass :: &
            parse_select_type => null()
        procedure(parse_without_parent_interface), pointer, nopass :: &
            parse_select_rank => null()
        procedure(parse_without_parent_interface), pointer, nopass :: &
            parse_where => null()
        procedure(parse_without_parent_interface), pointer, nopass :: &
            parse_forall => null()
        procedure(parse_without_parent_interface), pointer, nopass :: &
            parse_associate => null()
        procedure(parse_without_parent_interface), pointer, nopass :: &
            parse_block => null()
    end type statement_callbacks_t

    public :: null_statement_callbacks
    public :: parse_with_parent_interface
    public :: parse_without_parent_interface

contains

    pure function null_statement_callbacks() result(callbacks)
        type(statement_callbacks_t) :: callbacks
    end function null_statement_callbacks

end module parser_statement_callbacks_module
