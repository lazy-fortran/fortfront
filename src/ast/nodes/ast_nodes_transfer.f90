module ast_nodes_transfer
    use uid_generator, only: generate_uid
    use ast_base, only: ast_node, &
                        ast_node_wrapper, ast_visitor_base_t
    implicit none
    private

    ! Public types
    public :: cycle_node, exit_node, stop_node, return_node, entry_node
    public :: goto_node, error_stop_node, continue_node, pause_node, nullify_node
    ! Constructors migrated from ast_core
    public :: create_cycle, create_exit, create_stop, create_return, create_entry
    public :: create_goto, create_error_stop, create_continue, create_pause
    public :: create_nullify

    ! Cycle statement node
    type, extends(ast_node) :: cycle_node
        character(len=:), allocatable :: label ! Optional label to cycle to
    contains
        procedure :: accept => cycle_accept
        procedure :: assign => cycle_assign
        generic :: assignment(=) => assign
    end type cycle_node

    ! Exit statement node
    type, extends(ast_node) :: exit_node
        character(len=:), allocatable :: label ! Optional label to exit from
    contains
        procedure :: accept => exit_accept
        procedure :: assign => exit_assign
        generic :: assignment(=) => assign
    end type exit_node

    ! Stop statement node
    type, extends(ast_node) :: stop_node
        integer :: stop_code_index = 0 ! Optional stop code &
        ! expression index
        character(len=:), allocatable :: stop_message ! Optional stop message string
    contains
        procedure :: accept => stop_accept
        procedure :: assign => stop_assign
        generic :: assignment(=) => assign
    end type stop_node

    ! Return statement node
    type, extends(ast_node) :: return_node
        ! RETURN statement has no additional data
    contains
        procedure :: accept => return_accept
        procedure :: assign => return_assign
        generic :: assignment(=) => assign
    end type return_node

    ! Entry statement node
    type, extends(ast_node) :: entry_node
        character(len=:), allocatable :: name
        character(len=:), allocatable :: params_text
        integer, allocatable :: param_indices(:)
    contains
        procedure :: accept => entry_accept
        procedure :: assign => entry_assign
        generic :: assignment(=) => assign
    end type entry_node

    ! Continue statement node
    type, extends(ast_node) :: continue_node
    contains
        procedure :: accept => continue_accept
        procedure :: assign => continue_assign
        generic :: assignment(=) => assign
    end type continue_node

    ! Goto statement node
    type, extends(ast_node) :: goto_node
        character(len=:), allocatable :: label ! Target label (simple goto)
        character(len=:), allocatable :: label_list ! Comma-separated labels
        integer :: selector_index = 0 ! Expression index for computed goto selector
    contains
        procedure :: accept => goto_accept
        procedure :: assign => goto_assign
        generic :: assignment(=) => assign
    end type goto_node

    ! Error stop statement node
    type, extends(ast_node) :: error_stop_node
        integer :: error_code_index = 0 ! Optional error code expression index
        character(len=:), allocatable :: error_message ! Optional error message string
    contains
        procedure :: accept => error_stop_accept
        procedure :: assign => error_stop_assign
        generic :: assignment(=) => assign
    end type error_stop_node

    ! Pause statement node
    type, extends(ast_node) :: pause_node
        integer :: pause_code_index = 0 ! Optional pause code expression index
        character(len=:), allocatable :: pause_message ! Optional pause message string
    contains
        procedure :: accept => pause_accept
        procedure :: assign => pause_assign
        generic :: assignment(=) => assign
    end type pause_node

    ! Nullify statement node
    type, extends(ast_node) :: nullify_node
        integer, allocatable :: pointer_indices(:) ! Indices of pointers to nullify
    contains
        procedure :: accept => nullify_accept
        procedure :: assign => nullify_assign
        generic :: assignment(=) => assign
    end type nullify_node

contains

    ! Cycle statement implementations
    subroutine cycle_accept(this, visitor)
        class(cycle_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine cycle_accept

    subroutine cycle_assign(lhs, rhs)
        class(cycle_node), intent(inout) :: lhs
        class(cycle_node), intent(in) :: rhs
        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        if (allocated(rhs%label)) lhs%label = rhs%label
    end subroutine cycle_assign

    ! Constructors
    function create_cycle(loop_label, line, column) result(node)
        character(len=*), intent(in), optional :: loop_label
        integer, intent(in), optional :: line, column
        type(cycle_node) :: node

        node%uid = generate_uid()
        if (present(loop_label)) node%label = loop_label
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_cycle

    ! Exit statement implementations
    subroutine exit_accept(this, visitor)
        class(exit_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine exit_accept

    subroutine exit_assign(lhs, rhs)
        class(exit_node), intent(inout) :: lhs
        class(exit_node), intent(in) :: rhs
        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        if (allocated(rhs%label)) lhs%label = rhs%label
    end subroutine exit_assign

    function create_exit(loop_label, line, column) result(node)
        character(len=*), intent(in), optional :: loop_label
        integer, intent(in), optional :: line, column
        type(exit_node) :: node

        node%uid = generate_uid()
        if (present(loop_label)) node%label = loop_label
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_exit

    ! Stop statement implementations
    subroutine stop_accept(this, visitor)
        class(stop_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine stop_accept

    subroutine stop_assign(lhs, rhs)
        class(stop_node), intent(inout) :: lhs
        class(stop_node), intent(in) :: rhs
        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        lhs%stop_code_index = rhs%stop_code_index
        if (allocated(rhs%stop_message)) lhs%stop_message = rhs%stop_message
    end subroutine stop_assign

    function create_stop(stop_code_index, stop_message, line, column) result(node)
        integer, intent(in), optional :: stop_code_index
        character(len=*), intent(in), optional :: stop_message
        integer, intent(in), optional :: line, column
        type(stop_node) :: node

        node%uid = generate_uid()
        if (present(stop_code_index)) node%stop_code_index = stop_code_index
        if (present(stop_message)) node%stop_message = stop_message
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_stop

    ! Return statement implementations
    subroutine return_accept(this, visitor)
        class(return_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine return_accept

    subroutine return_assign(lhs, rhs)
        class(return_node), intent(inout) :: lhs
        class(return_node), intent(in) :: rhs
        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
    end subroutine return_assign

    function create_return(line, column) result(node)
        integer, intent(in), optional :: line, column
        type(return_node) :: node

        node%uid = generate_uid()
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_return

    ! Entry statement implementations
    subroutine entry_accept(this, visitor)
        class(entry_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine entry_accept

    subroutine entry_assign(lhs, rhs)
        class(entry_node), intent(inout) :: lhs
        class(entry_node), intent(in) :: rhs
        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        if (allocated(rhs%name)) lhs%name = rhs%name
        if (allocated(rhs%params_text)) lhs%params_text = rhs%params_text
        if (allocated(rhs%param_indices)) lhs%param_indices = rhs%param_indices
    end subroutine entry_assign

    function create_entry(name, param_indices, line, column) result(node)
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: param_indices(:)
        integer, intent(in), optional :: line, column
        type(entry_node) :: node

        node%uid = generate_uid()
        node%name = name
        if (present(param_indices)) node%param_indices = param_indices
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_entry

    subroutine continue_accept(this, visitor)
        class(continue_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine continue_accept

    subroutine continue_assign(lhs, rhs)
        class(continue_node), intent(inout) :: lhs
        class(continue_node), intent(in) :: rhs
        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
    end subroutine continue_assign

    function create_continue(line, column) result(node)
        integer, intent(in), optional :: line, column
        type(continue_node) :: node

        node%uid = generate_uid()
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_continue

    ! Goto statement implementations
    subroutine goto_accept(this, visitor)
        class(goto_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine goto_accept

    subroutine goto_assign(lhs, rhs)
        class(goto_node), intent(inout) :: lhs
        class(goto_node), intent(in) :: rhs
        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        if (allocated(rhs%label)) lhs%label = rhs%label
        if (allocated(rhs%label_list)) lhs%label_list = rhs%label_list
        lhs%selector_index = rhs%selector_index
    end subroutine goto_assign

    function create_goto(label, line, column) result(node)
        character(len=*), intent(in), optional :: label
        integer, intent(in), optional :: line, column
        type(goto_node) :: node

        node%uid = generate_uid()
        if (present(label)) node%label = label
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_goto

    ! Error stop statement implementations
    subroutine error_stop_accept(this, visitor)
        class(error_stop_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine error_stop_accept

    subroutine error_stop_assign(lhs, rhs)
        class(error_stop_node), intent(inout) :: lhs
        class(error_stop_node), intent(in) :: rhs
        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        lhs%error_code_index = rhs%error_code_index
        if (allocated(rhs%error_message)) lhs%error_message = rhs%error_message
    end subroutine error_stop_assign

    function create_error_stop(error_code_index, error_message, line, column) &
        result(node)
        integer, intent(in), optional :: error_code_index
        character(len=*), intent(in), optional :: error_message
        integer, intent(in), optional :: line, column
        type(error_stop_node) :: node

        node%uid = generate_uid()
        if (present(error_code_index)) node%error_code_index = error_code_index
        if (present(error_message)) node%error_message = error_message
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_error_stop

    ! Pause statement implementations
    subroutine pause_accept(this, visitor)
        class(pause_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine pause_accept

    subroutine pause_assign(lhs, rhs)
        class(pause_node), intent(inout) :: lhs
        class(pause_node), intent(in) :: rhs
        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        lhs%pause_code_index = rhs%pause_code_index
        if (allocated(rhs%pause_message)) lhs%pause_message = rhs%pause_message
    end subroutine pause_assign

    function create_pause(pause_code_index, pause_message, line, column) &
        result(node)
        integer, intent(in), optional :: pause_code_index
        character(len=*), intent(in), optional :: pause_message
        integer, intent(in), optional :: line, column
        type(pause_node) :: node

        node%uid = generate_uid()
        if (present(pause_code_index)) node%pause_code_index = pause_code_index
        if (present(pause_message)) node%pause_message = pause_message
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_pause

    ! Nullify statement implementations
    subroutine nullify_accept(this, visitor)
        class(nullify_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine nullify_accept

    subroutine nullify_assign(lhs, rhs)
        class(nullify_node), intent(inout) :: lhs
        class(nullify_node), intent(in) :: rhs
        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        if (allocated(rhs%pointer_indices)) then
            lhs%pointer_indices = rhs%pointer_indices
        end if
    end subroutine nullify_assign

    function create_nullify(pointer_indices, line, column) result(node)
        integer, intent(in), optional :: pointer_indices(:)
        integer, intent(in), optional :: line, column
        type(nullify_node) :: node

        node%uid = generate_uid()
        if (present(pointer_indices)) then
            if (size(pointer_indices) > 0) then
                node%pointer_indices = pointer_indices
            end if
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_nullify

end module ast_nodes_transfer
