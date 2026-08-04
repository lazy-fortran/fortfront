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
    public :: register_fallback_do_parser, fallback_do_parser_is_set, &
              call_fallback_do_parser
    public :: register_fallback_if_parser, call_fallback_if_parser

    !! The DO parser, registered rather than imported.
    !!
    !! handle_control_keyword needs a parser for `do` when its caller supplied
    !! no callback - `select case` populates only its own entry, so a loop
    !! inside a case arm had none. Importing the do parser closes a module
    !! cycle, which is why `if` reaches its own through a forwarder. This is
    !! the same indirection, done once here where the callback type already
    !! lives.
    procedure(parse_without_parent_interface), pointer :: do_parser => null()
    !! The full IF parser, for the same reason. The definition-level fallback
    !! reached otherwise handles a single if, not one containing another, and
    !! a nested if inside a case arm is exactly that.
    procedure(parse_with_parent_interface), pointer :: if_parser => null()
    public :: parse_with_parent_interface
    public :: parse_without_parent_interface

contains

    subroutine register_fallback_do_parser(proc)
        !! Called once by the do parser's own initialisation.
        procedure(parse_without_parent_interface) :: proc

        do_parser => proc
    end subroutine register_fallback_do_parser

    logical function fallback_do_parser_is_set() result(yes)
        yes = associated(do_parser)
    end function fallback_do_parser_is_set

    integer function call_fallback_do_parser(parser, arena) result(node_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena

        node_index = 0
        if (associated(do_parser)) node_index = do_parser(parser, arena)
    end function call_fallback_do_parser

    subroutine register_fallback_if_parser(proc)
        !! Called once by the if parser's own initialisation.
        procedure(parse_with_parent_interface) :: proc

        if_parser => proc
    end subroutine register_fallback_if_parser

    integer function call_fallback_if_parser(parser, arena, parent_index) &
        result(node_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index

        node_index = 0
        if (.not. associated(if_parser)) return
        if (present(parent_index)) then
            node_index = if_parser(parser, arena, parent_index)
        else
            node_index = if_parser(parser, arena)
        end if
    end function call_fallback_if_parser

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
