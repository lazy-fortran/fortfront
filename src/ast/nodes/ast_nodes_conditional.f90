module ast_nodes_conditional
    use json_module
    use uid_generator, only: generate_uid
    use ast_base, only: ast_node, visit_interface, to_json_interface, &
                         ast_node_wrapper, ast_visitor_base_t
    implicit none
    private

    ! Public types
    public :: elseif_wrapper, case_wrapper
    public :: if_node, select_case_node, case_block_node
    public :: case_range_node, case_default_node

    ! Public factory functions
    public :: create_if, create_select_case

    ! Elseif wrapper (not an AST node itself)
    type :: elseif_wrapper
        integer :: condition_index = 0                ! Elseif condition arena index
        integer, allocatable :: body_indices(:)       ! Elseif body arena indices
    end type elseif_wrapper

    ! Case statement wrapper (temporary for parser compatibility)
    type :: case_wrapper
        character(len=:), allocatable :: case_type    ! "case", "case_default"
        class(ast_node), allocatable :: value         ! Case value (optional &
                                                        ! for default)
        type(ast_node_wrapper), allocatable :: body(:) ! Case body
    end type case_wrapper

    ! If statement node
    type, extends(ast_node) :: if_node
        integer :: condition_index = 0                ! If condition arena index
        integer, allocatable :: then_body_indices(:) ! Then body arena indices
        type(elseif_wrapper), allocatable :: elseif_blocks(:) ! Elseif blocks (optional)
        integer, allocatable :: else_body_indices(:) ! Else body arena indices &
                                                      ! (optional)
    contains
        procedure :: accept => if_accept
        procedure :: to_json => if_to_json
        procedure :: assign => if_assign
        generic :: assignment(=) => assign
    end type if_node

    ! Select case construct node
    type, extends(ast_node) :: select_case_node
        integer :: selector_index = 0                 ! Selector expression arena index
        integer, allocatable :: case_indices(:)       ! Case block arena indices
        integer :: default_index = 0                  ! Default case arena index &
                                                        ! (optional)
    contains
        procedure :: accept => select_case_accept
        procedure :: to_json => select_case_to_json
        procedure :: assign => select_case_assign
        generic :: assignment(=) => assign
    end type select_case_node

    ! Case block node (case (values) body)
    type, extends(ast_node) :: case_block_node
        integer, allocatable :: value_indices(:)      ! Case value arena indices
        integer, allocatable :: body_indices(:)       ! Case body arena indices
    contains
        procedure :: accept => case_block_accept
        procedure :: to_json => case_block_to_json
        procedure :: assign => case_block_assign
        generic :: assignment(=) => assign
    end type case_block_node

    ! Case range node (for ranges like 1:5)
    type, extends(ast_node) :: case_range_node
        integer :: start_value = 0                    ! Start value
        integer :: end_value = 0                      ! End value
    contains
        procedure :: accept => case_range_accept
        procedure :: to_json => case_range_to_json
        procedure :: assign => case_range_assign
        generic :: assignment(=) => assign
    end type case_range_node

    ! Case default node
    type, extends(ast_node) :: case_default_node
        integer, allocatable :: body_indices(:)       ! Default case body arena indices
    contains
        procedure :: accept => case_default_accept
        procedure :: to_json => case_default_to_json
        procedure :: assign => case_default_assign
        generic :: assignment(=) => assign
    end type case_default_node

contains

    ! If node implementations
    subroutine if_accept(this, visitor)
        class(if_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
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
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
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

    ! Select case implementations
    subroutine select_case_accept(this, visitor)
        class(select_case_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
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
        if (this%default_index > 0) call json%add(obj, 'default_index', &
                                                   this%default_index)
        call json%add(parent, obj)
    end subroutine select_case_to_json

    subroutine select_case_assign(lhs, rhs)
        class(select_case_node), intent(inout) :: lhs
        class(select_case_node), intent(in) :: rhs
        ! Copy base class components
        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
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
        class(ast_visitor_base_t), intent(inout) :: visitor
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
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
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
        class(ast_visitor_base_t), intent(inout) :: visitor
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
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        ! Copy specific components
        lhs%start_value = rhs%start_value
        lhs%end_value = rhs%end_value
    end subroutine case_range_assign

    ! Case default implementations
    subroutine case_default_accept(this, visitor)
        class(case_default_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
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
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        ! Deep copy allocatable array
        if (allocated(rhs%body_indices)) then
            if (allocated(lhs%body_indices)) deallocate(lhs%body_indices)
            allocate(lhs%body_indices(size(rhs%body_indices)))
            lhs%body_indices = rhs%body_indices
        end if
    end subroutine case_default_assign

    ! Factory functions
    function create_if(condition_index, then_body_indices, elseif_blocks, &
                       else_body_indices, line, column) result(node)
        integer, intent(in) :: condition_index
        integer, intent(in), optional :: then_body_indices(:)
        type(elseif_wrapper), intent(in), optional :: elseif_blocks(:)
        integer, intent(in), optional :: else_body_indices(:)
        integer, intent(in), optional :: line, column
        type(if_node) :: node

        node%uid = generate_uid()
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

    function create_select_case(expr_index, case_indices, default_index, &
                                line, column) result(node)
        integer, intent(in) :: expr_index
        integer, intent(in), optional :: case_indices(:)
        integer, intent(in), optional :: default_index
        integer, intent(in), optional :: line, column
        type(select_case_node) :: node

        node%uid = generate_uid()
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

end module ast_nodes_conditional