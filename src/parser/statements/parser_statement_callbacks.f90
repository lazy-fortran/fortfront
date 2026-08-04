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
        !! Every component explicitly nullified, rather than left to the
        !! type's default initialisation.
        !!
        !! A function result of a derived type whose components are procedure
        !! pointers is not reliably default-initialised by gfortran 13.3 - the
        !! compiler on Ubuntu 24.04, and so on every GitHub runner. The
        !! components came back holding whatever was on the stack, `associated`
        !! on one of them answered true, and the parser called into it. That is
        !! the segfault on an `if` block inside a `case` arm: `parse_select_case`
        !! builds its callbacks from this function.
        type(statement_callbacks_t) :: callbacks

        callbacks%parse_if => null()
        callbacks%parse_do_loop => null()
        callbacks%parse_select_case => null()
        callbacks%parse_select_type => null()
        callbacks%parse_select_rank => null()
        callbacks%parse_where => null()
        callbacks%parse_forall => null()
        callbacks%parse_associate => null()
        callbacks%parse_block => null()
    end function null_statement_callbacks

end module parser_statement_callbacks_module
