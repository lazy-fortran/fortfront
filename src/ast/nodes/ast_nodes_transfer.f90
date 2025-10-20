module ast_nodes_transfer
    use json_module
    use uid_generator, only: generate_uid
    use ast_base, only: ast_node, visit_interface, to_json_interface, &
                        ast_node_wrapper, ast_visitor_base_t
    implicit none
    private

    ! Public types
    public :: cycle_node, exit_node, stop_node, return_node
    public :: goto_node, error_stop_node
    ! Constructors migrated from ast_core
    public :: create_cycle, create_exit, create_stop, create_return
    public :: create_goto, create_error_stop

    ! Cycle statement node
    type, extends(ast_node) :: cycle_node
        character(len=:), allocatable :: label  ! Optional label to cycle to
    contains
        procedure :: accept => cycle_accept
        procedure :: to_json => cycle_to_json
        procedure :: assign => cycle_assign
        generic :: assignment(=) => assign
    end type cycle_node

    ! Exit statement node
    type, extends(ast_node) :: exit_node
        character(len=:), allocatable :: label  ! Optional label to exit from
    contains
        procedure :: accept => exit_accept
        procedure :: to_json => exit_to_json
        procedure :: assign => exit_assign
        generic :: assignment(=) => assign
    end type exit_node

    ! Stop statement node
    type, extends(ast_node) :: stop_node
        integer :: stop_code_index = 0  ! Optional stop code &
        ! expression index
        character(len=:), allocatable :: stop_message  ! Optional stop message string
    contains
        procedure :: accept => stop_accept
        procedure :: to_json => stop_to_json
        procedure :: assign => stop_assign
        generic :: assignment(=) => assign
    end type stop_node

    ! Return statement node
    type, extends(ast_node) :: return_node
        ! RETURN statement has no additional data
    contains
        procedure :: accept => return_accept
        procedure :: to_json => return_to_json
        procedure :: assign => return_assign
        generic :: assignment(=) => assign
    end type return_node

    ! Goto statement node
    type, extends(ast_node) :: goto_node
        character(len=:), allocatable :: label  ! Target label (simple goto)
        character(len=:), allocatable :: label_list  ! Comma-separated labels (computed goto)
        integer :: selector_index = 0  ! Expression index for computed goto selector
    contains
        procedure :: accept => goto_accept
        procedure :: to_json => goto_to_json
        procedure :: assign => goto_assign
        generic :: assignment(=) => assign
    end type goto_node

    ! Error stop statement node
    type, extends(ast_node) :: error_stop_node
        integer :: error_code_index = 0  ! Optional error code expression index
        character(len=:), allocatable :: error_message  ! Optional error message string
    contains
        procedure :: accept => error_stop_accept
        procedure :: to_json => error_stop_to_json
        procedure :: assign => error_stop_assign
        generic :: assignment(=) => assign
    end type error_stop_node

contains

    ! Cycle statement implementations
    subroutine cycle_accept(this, visitor)
        class(cycle_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine cycle_accept

    subroutine cycle_to_json(this, json, parent)
        class(cycle_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'cycle')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        if (allocated(this%label)) call json%add(obj, 'label', this%label)
        call json%add(parent, obj)
    end subroutine cycle_to_json

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

    subroutine exit_to_json(this, json, parent)
        class(exit_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'exit')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        if (allocated(this%label)) call json%add(obj, 'label', this%label)
        call json%add(parent, obj)
    end subroutine exit_to_json

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

    subroutine stop_to_json(this, json, parent)
        class(stop_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'stop')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        if (this%stop_code_index > 0) call json%add(obj, 'stop_code_index', &
                                                    this%stop_code_index)
        if (allocated(this%stop_message)) call json%add(obj, 'stop_message', &
                                                        this%stop_message)
        call json%add(parent, obj)
    end subroutine stop_to_json

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

    subroutine return_to_json(this, json, parent)
        class(return_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'return')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(parent, obj)
    end subroutine return_to_json

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

    ! Goto statement implementations
    subroutine goto_accept(this, visitor)
        class(goto_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine goto_accept

    subroutine goto_to_json(this, json, parent)
        class(goto_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'goto')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        if (allocated(this%label)) call json%add(obj, 'label', this%label)
        if (allocated(this%label_list)) call json%add(obj, 'label_list', this%label_list)
        if (this%selector_index > 0) call json%add(obj, 'selector_index', this%selector_index)
        call json%add(parent, obj)
    end subroutine goto_to_json

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

    subroutine error_stop_to_json(this, json, parent)
        class(error_stop_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'error_stop')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        if (this%error_code_index > 0) call json%add(obj, 'error_code_index', &
                                                     this%error_code_index)
        if (allocated(this%error_message)) call json%add(obj, 'error_message', &
                                                         this%error_message)
        call json%add(parent, obj)
    end subroutine error_stop_to_json

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

    function create_error_stop(error_code_index, error_message, line, column) result(node)
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

end module ast_nodes_transfer
