module ast_nodes_control
    use json_module
    use ast_base, only: ast_node, visit_interface, to_json_interface, ast_node_wrapper
    implicit none
    private

    ! Public factory functions
    public :: create_do_loop, create_do_while, create_if, create_select_case

    ! Control flow AST nodes

    ! Elseif wrapper (not an AST node itself)
    type, public :: elseif_wrapper
        integer :: condition_index = 0                ! Elseif condition arena index
        integer, allocatable :: body_indices(:)       ! Elseif body arena indices
    end type elseif_wrapper

    ! Case statement wrapper (temporary for parser compatibility)
    type, public :: case_wrapper
        character(len=:), allocatable :: case_type    ! "case", "case_default"
        class(ast_node), allocatable :: value         ! Case value (optional for default)
        type(ast_node_wrapper), allocatable :: body(:) ! Case body
    end type case_wrapper

    ! If statement node
    type, extends(ast_node), public :: if_node
        integer :: condition_index = 0                ! If condition arena index
        integer, allocatable :: then_body_indices(:) ! Then body arena indices
        type(elseif_wrapper), allocatable :: elseif_blocks(:) ! Elseif blocks (optional)
        integer, allocatable :: else_body_indices(:) ! Else body arena indices (optional)
    contains
        procedure :: accept => if_accept
        procedure :: to_json => if_to_json
        procedure :: assign => if_assign
        generic :: assignment(=) => assign
    end type if_node

    ! Do loop node
    type, extends(ast_node), public :: do_loop_node
        character(len=:), allocatable :: var_name     ! Loop variable
        character(len=:), allocatable :: label        ! Loop label (optional)
        integer :: start_expr_index = 0               ! Start expression arena index
        integer :: end_expr_index = 0                 ! End expression arena index
        integer :: step_expr_index = 0                ! Step expression arena index (optional)
        integer, allocatable :: body_indices(:)       ! Loop body arena indices
    contains
        procedure :: accept => do_loop_accept
        procedure :: to_json => do_loop_to_json
        procedure :: assign => do_loop_assign
        generic :: assignment(=) => assign
    end type do_loop_node

    ! Do while loop node
    type, extends(ast_node), public :: do_while_node
        integer :: condition_index       ! Index to condition expression
        integer, allocatable :: body_indices(:)  ! Indices to body statements
    contains
        procedure :: accept => do_while_accept
        procedure :: to_json => do_while_to_json
        procedure :: assign => do_while_assign
        generic :: assignment(=) => assign
    end type do_while_node

    ! Forall construct node
    type, extends(ast_node), public :: forall_node
        character(len=:), allocatable :: index_var        ! Index variable name
        integer :: start_index = 0                        ! Start expression arena index
        integer :: end_index = 0                          ! End expression arena index
        integer :: step_index = 0                         ! Step expression arena index (optional)
        integer :: mask_index = 0                         ! Mask condition arena index (optional)
        integer, allocatable :: body_indices(:)           ! Forall body arena indices
    contains
        procedure :: accept => forall_accept
        procedure :: to_json => forall_to_json
        procedure :: assign => forall_assign
        generic :: assignment(=) => assign
    end type forall_node

    ! Select case construct node
    type, extends(ast_node), public :: select_case_node
        integer :: selector_index = 0                 ! Selector expression arena index
        integer, allocatable :: case_indices(:)       ! Case block arena indices
        integer :: default_index = 0                  ! Default case arena index (optional)
    contains
        procedure :: accept => select_case_accept
        procedure :: to_json => select_case_to_json
        procedure :: assign => select_case_assign
        generic :: assignment(=) => assign
    end type select_case_node

    ! Case block node (case (values) body)
    type, extends(ast_node), public :: case_block_node
        integer, allocatable :: value_indices(:)      ! Case value arena indices
        integer, allocatable :: body_indices(:)       ! Case body arena indices
    contains
        procedure :: accept => case_block_accept
        procedure :: to_json => case_block_to_json
        procedure :: assign => case_block_assign
        generic :: assignment(=) => assign
    end type case_block_node

    ! Case range node (for ranges like 1:5)
    type, extends(ast_node), public :: case_range_node
        integer :: start_value = 0                    ! Start value
        integer :: end_value = 0                      ! End value
    contains
        procedure :: accept => case_range_accept
        procedure :: to_json => case_range_to_json
        procedure :: assign => case_range_assign
        generic :: assignment(=) => assign
    end type case_range_node

    ! Case default node
    type, extends(ast_node), public :: case_default_node
        integer, allocatable :: body_indices(:)       ! Default case body arena indices
    contains
        procedure :: accept => case_default_accept
        procedure :: to_json => case_default_to_json
        procedure :: assign => case_default_assign
        generic :: assignment(=) => assign
    end type case_default_node

    ! Where construct node
    type, extends(ast_node), public :: where_node
        integer :: mask_index = 0                     ! Mask expression arena index
        integer, allocatable :: body_indices(:)       ! Where body arena indices
        integer, allocatable :: elsewhere_indices(:)  ! Elsewhere body arena indices (optional)
    contains
        procedure :: accept => where_accept
        procedure :: to_json => where_to_json
        procedure :: assign => where_assign
        generic :: assignment(=) => assign
    end type where_node

    ! Cycle statement node
    type, extends(ast_node), public :: cycle_node
        character(len=:), allocatable :: label        ! Optional label to cycle to
    contains
        procedure :: accept => cycle_accept
        procedure :: to_json => cycle_to_json
        procedure :: assign => cycle_assign
        generic :: assignment(=) => assign
    end type cycle_node

    ! Exit statement node
    type, extends(ast_node), public :: exit_node
        character(len=:), allocatable :: label        ! Optional label to exit from
    contains
        procedure :: accept => exit_accept
        procedure :: to_json => exit_to_json
        procedure :: assign => exit_assign
        generic :: assignment(=) => assign
    end type exit_node

    ! Stop statement node
    type, extends(ast_node), public :: stop_node
        integer :: stop_code_index = 0                ! Optional stop code expression index
        character(len=:), allocatable :: stop_message ! Optional stop message string
    contains
        procedure :: accept => stop_accept
        procedure :: to_json => stop_to_json
        procedure :: assign => stop_assign
        generic :: assignment(=) => assign
    end type stop_node

    ! Return statement node
    type, extends(ast_node), public :: return_node
        ! RETURN statement has no additional data
    contains
        procedure :: accept => return_accept
        procedure :: to_json => return_to_json
        procedure :: assign => return_assign
        generic :: assignment(=) => assign
    end type return_node

contains

    ! Stub implementations for if_node
    subroutine if_accept(this, visitor)
        class(if_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Stub implementation
    end subroutine if_accept

    subroutine if_to_json(this, json, parent)
        class(if_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        ! Stub implementation
    end subroutine if_to_json

    subroutine if_assign(lhs, rhs)
        class(if_node), intent(inout) :: lhs
        class(if_node), intent(in) :: rhs
        integer :: i
        ! Copy base fields
        lhs%line = rhs%line
        lhs%column = rhs%column
        if (allocated(rhs%inferred_type)) then
            allocate(lhs%inferred_type)
            lhs%inferred_type = rhs%inferred_type
        end if
        ! Copy specific fields
        lhs%condition_index = rhs%condition_index
        if (allocated(rhs%then_body_indices)) then
            if (allocated(lhs%then_body_indices)) deallocate(lhs%then_body_indices)
            allocate(lhs%then_body_indices(size(rhs%then_body_indices)))
            lhs%then_body_indices = rhs%then_body_indices
        end if
        if (allocated(rhs%else_body_indices)) then
            if (allocated(lhs%else_body_indices)) deallocate(lhs%else_body_indices)
            allocate(lhs%else_body_indices(size(rhs%else_body_indices)))
            lhs%else_body_indices = rhs%else_body_indices
        end if
        if (allocated(rhs%elseif_blocks)) then
            if (allocated(lhs%elseif_blocks)) deallocate(lhs%elseif_blocks)
            allocate(lhs%elseif_blocks(size(rhs%elseif_blocks)))
            lhs%elseif_blocks = rhs%elseif_blocks
        end if
    end subroutine if_assign

    ! Stub implementations for do_loop_node
    subroutine do_loop_accept(this, visitor)
        class(do_loop_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Stub implementation
    end subroutine do_loop_accept

    subroutine do_loop_to_json(this, json, parent)
        class(do_loop_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        ! Stub implementation
    end subroutine do_loop_to_json

    subroutine do_loop_assign(lhs, rhs)
        class(do_loop_node), intent(inout) :: lhs
        class(do_loop_node), intent(in) :: rhs
        ! Copy base class components
        lhs%line = rhs%line
        lhs%column = rhs%column
        if (allocated(rhs%inferred_type)) then
            allocate(lhs%inferred_type)
            lhs%inferred_type = rhs%inferred_type
        end if
        ! Copy specific components
        lhs%var_name = rhs%var_name
        if (allocated(rhs%label)) lhs%label = rhs%label
        lhs%start_expr_index = rhs%start_expr_index
        lhs%end_expr_index = rhs%end_expr_index
        lhs%step_expr_index = rhs%step_expr_index
        if (allocated(rhs%body_indices)) then
            if (allocated(lhs%body_indices)) deallocate(lhs%body_indices)
            allocate(lhs%body_indices(size(rhs%body_indices)))
            lhs%body_indices = rhs%body_indices
        end if
    end subroutine do_loop_assign

    ! Stub implementations for do_while_node
    subroutine do_while_accept(this, visitor)
        class(do_while_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Stub implementation
    end subroutine do_while_accept

    subroutine do_while_to_json(this, json, parent)
        class(do_while_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        ! Stub implementation
    end subroutine do_while_to_json

    subroutine do_while_assign(lhs, rhs)
        class(do_while_node), intent(inout) :: lhs
        class(do_while_node), intent(in) :: rhs
        lhs%condition_index = rhs%condition_index
        if (allocated(rhs%body_indices)) lhs%body_indices = rhs%body_indices
    end subroutine do_while_assign

    ! Stub implementations for forall_node
    subroutine forall_accept(this, visitor)
        class(forall_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Stub implementation
    end subroutine forall_accept

    subroutine forall_to_json(this, json, parent)
        class(forall_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        ! Stub implementation
    end subroutine forall_to_json

    subroutine forall_assign(lhs, rhs)
        class(forall_node), intent(inout) :: lhs
        class(forall_node), intent(in) :: rhs
        ! Copy base class components
        lhs%line = rhs%line
        lhs%column = rhs%column
        if (allocated(rhs%inferred_type)) then
            allocate(lhs%inferred_type)
            lhs%inferred_type = rhs%inferred_type
        end if
        ! Copy specific components
        lhs%index_var = rhs%index_var
        lhs%start_index = rhs%start_index
        lhs%end_index = rhs%end_index
        lhs%step_index = rhs%step_index
        lhs%mask_index = rhs%mask_index
        if (allocated(rhs%body_indices)) then
            if (allocated(lhs%body_indices)) deallocate(lhs%body_indices)
            allocate(lhs%body_indices(size(rhs%body_indices)))
            lhs%body_indices = rhs%body_indices
        end if
    end subroutine forall_assign

    ! Select case implementations
    subroutine select_case_accept(this, visitor)
        class(select_case_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Visitor pattern implementation
    end subroutine select_case_accept

    subroutine select_case_to_json(this, json, parent)
        class(select_case_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj
        
        call json%create_object(obj, '')
        call json%add(obj, 'type', 'select_case')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'selector_index', this%selector_index)
        if (this%default_index > 0) call json%add(obj, 'default_index', this%default_index)
        call json%add(parent, obj)
    end subroutine select_case_to_json

    subroutine select_case_assign(lhs, rhs)
        class(select_case_node), intent(inout) :: lhs
        class(select_case_node), intent(in) :: rhs
        ! Copy base class components
        lhs%line = rhs%line
        lhs%column = rhs%column
        if (allocated(rhs%inferred_type)) then
            allocate(lhs%inferred_type)
            lhs%inferred_type = rhs%inferred_type
        end if
        ! Copy specific components
        lhs%selector_index = rhs%selector_index
        lhs%default_index = rhs%default_index
        ! Deep copy allocatable array
        if (allocated(rhs%case_indices)) then
            if (allocated(lhs%case_indices)) deallocate(lhs%case_indices)
            allocate(lhs%case_indices(size(rhs%case_indices)))
            lhs%case_indices = rhs%case_indices
        end if
    end subroutine select_case_assign

    ! Case block implementations
    subroutine case_block_accept(this, visitor)
        class(case_block_node), intent(in) :: this
        class(*), intent(inout) :: visitor
    end subroutine case_block_accept

    subroutine case_block_to_json(this, json, parent)
        class(case_block_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'case_block')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(parent, obj)
    end subroutine case_block_to_json

    subroutine case_block_assign(lhs, rhs)
        class(case_block_node), intent(inout) :: lhs
        class(case_block_node), intent(in) :: rhs
        ! Copy base class components
        lhs%line = rhs%line
        lhs%column = rhs%column
        if (allocated(rhs%inferred_type)) then
            allocate(lhs%inferred_type)
            lhs%inferred_type = rhs%inferred_type
        end if
        ! Deep copy allocatable arrays
        if (allocated(rhs%value_indices)) then
            if (allocated(lhs%value_indices)) deallocate(lhs%value_indices)
            allocate(lhs%value_indices(size(rhs%value_indices)))
            lhs%value_indices = rhs%value_indices
        end if
        if (allocated(rhs%body_indices)) then
            if (allocated(lhs%body_indices)) deallocate(lhs%body_indices)
            allocate(lhs%body_indices(size(rhs%body_indices)))
            lhs%body_indices = rhs%body_indices
        end if
    end subroutine case_block_assign

    ! Case range implementations
    subroutine case_range_accept(this, visitor)
        class(case_range_node), intent(in) :: this
        class(*), intent(inout) :: visitor
    end subroutine case_range_accept

    subroutine case_range_to_json(this, json, parent)
        class(case_range_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'case_range')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'start_value', this%start_value)
        call json%add(obj, 'end_value', this%end_value)
        call json%add(parent, obj)
    end subroutine case_range_to_json

    subroutine case_range_assign(lhs, rhs)
        class(case_range_node), intent(inout) :: lhs
        class(case_range_node), intent(in) :: rhs
        ! Copy base class components
        lhs%line = rhs%line
        lhs%column = rhs%column
        if (allocated(rhs%inferred_type)) then
            allocate(lhs%inferred_type)
            lhs%inferred_type = rhs%inferred_type
        end if
        ! Copy specific components
        lhs%start_value = rhs%start_value
        lhs%end_value = rhs%end_value
    end subroutine case_range_assign

    ! Case default implementations
    subroutine case_default_accept(this, visitor)
        class(case_default_node), intent(in) :: this
        class(*), intent(inout) :: visitor
    end subroutine case_default_accept

    subroutine case_default_to_json(this, json, parent)
        class(case_default_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'case_default')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(parent, obj)
    end subroutine case_default_to_json

    subroutine case_default_assign(lhs, rhs)
        class(case_default_node), intent(inout) :: lhs
        class(case_default_node), intent(in) :: rhs
        ! Copy base class components
        lhs%line = rhs%line
        lhs%column = rhs%column
        if (allocated(rhs%inferred_type)) then
            allocate(lhs%inferred_type)
            lhs%inferred_type = rhs%inferred_type
        end if
        ! Deep copy allocatable array
        if (allocated(rhs%body_indices)) then
            if (allocated(lhs%body_indices)) deallocate(lhs%body_indices)
            allocate(lhs%body_indices(size(rhs%body_indices)))
            lhs%body_indices = rhs%body_indices
        end if
    end subroutine case_default_assign

    ! Where construct implementations
    subroutine where_accept(this, visitor)
        class(where_node), intent(in) :: this
        class(*), intent(inout) :: visitor
    end subroutine where_accept

    subroutine where_to_json(this, json, parent)
        class(where_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'where')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'mask_index', this%mask_index)
        call json%add(parent, obj)
    end subroutine where_to_json

    subroutine where_assign(lhs, rhs)
        class(where_node), intent(inout) :: lhs
        class(where_node), intent(in) :: rhs
        lhs%line = rhs%line
        lhs%column = rhs%column
        if (allocated(rhs%inferred_type)) then
            allocate(lhs%inferred_type)
            lhs%inferred_type = rhs%inferred_type
        end if
        lhs%mask_index = rhs%mask_index
        if (allocated(rhs%body_indices)) then
            if (allocated(lhs%body_indices)) deallocate(lhs%body_indices)
            allocate(lhs%body_indices(size(rhs%body_indices)))
            lhs%body_indices = rhs%body_indices
        end if
        if (allocated(rhs%elsewhere_indices)) then
            if (allocated(lhs%elsewhere_indices)) deallocate(lhs%elsewhere_indices)
            allocate(lhs%elsewhere_indices(size(rhs%elsewhere_indices)))
            lhs%elsewhere_indices = rhs%elsewhere_indices
        end if
    end subroutine where_assign

    ! Cycle statement implementations
    subroutine cycle_accept(this, visitor)
        class(cycle_node), intent(in) :: this
        class(*), intent(inout) :: visitor
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
        if (allocated(rhs%inferred_type)) then
            allocate(lhs%inferred_type)
            lhs%inferred_type = rhs%inferred_type
        end if
        if (allocated(rhs%label)) lhs%label = rhs%label
    end subroutine cycle_assign

    ! Exit statement implementations
    subroutine exit_accept(this, visitor)
        class(exit_node), intent(in) :: this
        class(*), intent(inout) :: visitor
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
        if (allocated(rhs%inferred_type)) then
            allocate(lhs%inferred_type)
            lhs%inferred_type = rhs%inferred_type
        end if
        if (allocated(rhs%label)) lhs%label = rhs%label
    end subroutine exit_assign

    ! Stop statement implementations
    subroutine stop_accept(this, visitor)
        class(stop_node), intent(in) :: this
        class(*), intent(inout) :: visitor
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
        if (this%stop_code_index > 0) call json%add(obj, 'stop_code_index', this%stop_code_index)
        if (allocated(this%stop_message)) call json%add(obj, 'stop_message', this%stop_message)
        call json%add(parent, obj)
    end subroutine stop_to_json

    subroutine stop_assign(lhs, rhs)
        class(stop_node), intent(inout) :: lhs
        class(stop_node), intent(in) :: rhs
        lhs%line = rhs%line
        lhs%column = rhs%column
        if (allocated(rhs%inferred_type)) then
            allocate(lhs%inferred_type)
            lhs%inferred_type = rhs%inferred_type
        end if
        lhs%stop_code_index = rhs%stop_code_index
        if (allocated(rhs%stop_message)) lhs%stop_message = rhs%stop_message
    end subroutine stop_assign

    ! Return statement implementations
    subroutine return_accept(this, visitor)
        class(return_node), intent(in) :: this
        class(*), intent(inout) :: visitor
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
        if (allocated(rhs%inferred_type)) then
            allocate(lhs%inferred_type)
            lhs%inferred_type = rhs%inferred_type
        end if
    end subroutine return_assign

    ! Factory functions
    function create_do_loop(var_name, start_expr_index, end_expr_index, step_expr_index, body_indices, line, column) result(node)
        character(len=*), intent(in) :: var_name
        integer, intent(in) :: start_expr_index, end_expr_index
        integer, intent(in), optional :: step_expr_index
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column
        type(do_loop_node) :: node

        node%var_name = var_name
        node%start_expr_index = start_expr_index
        node%end_expr_index = end_expr_index
        if (present(step_expr_index)) node%step_expr_index = step_expr_index

        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                node%body_indices = body_indices
            end if
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_do_loop

    function create_do_while(condition_index, body_indices, line, column) result(node)
        integer, intent(in) :: condition_index
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column
        type(do_while_node) :: node

        node%condition_index = condition_index

        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                node%body_indices = body_indices
            end if
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_do_while

    function create_if(condition_index, then_body_indices, elseif_blocks, else_body_indices, line, column) result(node)
        integer, intent(in) :: condition_index
        integer, intent(in), optional :: then_body_indices(:)
        type(elseif_wrapper), intent(in), optional :: elseif_blocks(:)
        integer, intent(in), optional :: else_body_indices(:)
        integer, intent(in), optional :: line, column
        type(if_node) :: node

        node%condition_index = condition_index

        if (present(then_body_indices)) then
            if (size(then_body_indices) > 0) then
                node%then_body_indices = then_body_indices
            end if
        end if

        if (present(elseif_blocks)) then
            if (size(elseif_blocks) > 0) then
                node%elseif_blocks = elseif_blocks
            end if
        end if

        if (present(else_body_indices)) then
            if (size(else_body_indices) > 0) then
                node%else_body_indices = else_body_indices
            end if
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_if

    function create_select_case(expr_index, case_indices, default_index, line, column) result(node)
        integer, intent(in) :: expr_index
        integer, intent(in), optional :: case_indices(:)
        integer, intent(in), optional :: default_index
        integer, intent(in), optional :: line, column
        type(select_case_node) :: node

        node%selector_index = expr_index
        if (present(case_indices)) then
            if (size(case_indices) > 0) then
                node%case_indices = case_indices
            end if
        end if
        if (present(default_index)) node%default_index = default_index

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_select_case

end module ast_nodes_control