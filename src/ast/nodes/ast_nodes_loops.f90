module ast_nodes_loops
    use json_module
    use uid_generator, only: generate_uid
    use ast_base, only: ast_node, visit_interface, to_json_interface, &
                         ast_node_wrapper, ast_visitor_base_t
    implicit none
    private

    ! Constants
    integer, parameter, public :: MAX_INDEX_NAME_LENGTH = 32

    ! Public types
    public :: do_loop_node, do_while_node, forall_node, forall_triplet_t

    ! Public factory functions
    public :: create_do_loop, create_do_while

    ! Do loop node
    type, extends(ast_node) :: do_loop_node
        character(len=:), allocatable :: var_name     ! Loop variable
        character(len=:), allocatable :: label        ! Loop label (optional)
        integer :: start_expr_index = 0               ! Start expression arena index
        integer :: end_expr_index = 0                 ! End expression arena index
        integer :: step_expr_index = 0                ! Step expression arena &
                                                        ! index (optional)
        integer, allocatable :: body_indices(:)       ! Loop body arena indices
    contains
        procedure :: accept => do_loop_accept
        procedure :: to_json => do_loop_to_json
        procedure :: assign => do_loop_assign
        generic :: assignment(=) => assign
    end type do_loop_node

    ! Do while loop node
    type, extends(ast_node) :: do_while_node
        integer :: condition_index       ! Index to condition expression
        integer, allocatable :: body_indices(:)  ! Indices to body statements
    contains
        procedure :: accept => do_while_accept
        procedure :: to_json => do_while_to_json
        procedure :: assign => do_while_assign
        generic :: assignment(=) => assign
    end type do_while_node

    ! Enhanced FORALL construct node
    type, extends(ast_node) :: forall_node
        ! Iteration specifications
        integer :: num_indices = 0
        character(len=:), allocatable :: index_names(:)
        integer, allocatable :: lower_bound_indices(:)
        integer, allocatable :: upper_bound_indices(:)
        integer, allocatable :: stride_indices(:)  ! Optional strides (0 = no stride)
        
        ! Optional mask
        logical :: has_mask = .false.
        integer :: mask_expr_index = 0
        
        ! Body statements
        integer, allocatable :: body_indices(:)
        
        ! Dependency analysis results
        logical :: has_dependencies = .false.
        logical :: is_parallel_safe = .false.
        integer, allocatable :: dependency_pairs(:,:)  ! Pairs of dependent statements
    contains
        procedure :: accept => forall_accept
        procedure :: to_json => forall_to_json
        procedure :: assign => forall_assign
        generic :: assignment(=) => assign
    end type forall_node
    
    ! FORALL triplet type for index specifications
    type :: forall_triplet_t
        character(len=:), allocatable :: index_name
        integer :: lower_expr_index = 0
        integer :: upper_expr_index = 0
        integer :: stride_expr_index = 0  ! 0 if no stride
    end type forall_triplet_t

contains

    ! Do loop node implementations
    subroutine do_loop_accept(this, visitor)
        class(do_loop_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
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
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
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

    ! Do while node implementations
    subroutine do_while_accept(this, visitor)
        class(do_while_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
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
        lhs%condition_index = rhs%condition_index
        if (allocated(rhs%body_indices)) then
            if (allocated(lhs%body_indices)) deallocate(lhs%body_indices)
            allocate(lhs%body_indices(size(rhs%body_indices)))
            lhs%body_indices = rhs%body_indices
        end if
    end subroutine do_while_assign

    ! Forall node implementations
    subroutine forall_accept(this, visitor)
        class(forall_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
        ! Stub implementation
    end subroutine forall_accept

    subroutine forall_to_json(this, json, parent)
        class(forall_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj
        
        call json%create_object(obj, '')
        call json%add(obj, 'type', 'forall')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'num_indices', this%num_indices)
        if (allocated(this%lower_bound_indices)) then
            call json%add(obj, 'lower_bound_indices', this%lower_bound_indices)
        end if
        if (allocated(this%upper_bound_indices)) then
            call json%add(obj, 'upper_bound_indices', this%upper_bound_indices)
        end if
        if (allocated(this%stride_indices)) then
            call json%add(obj, 'stride_indices', this%stride_indices)
        end if
        call json%add(obj, 'has_mask', this%has_mask)
        if (this%has_mask) then
            call json%add(obj, 'mask_expr_index', this%mask_expr_index)
        end if
        if (allocated(this%body_indices)) then
            call json%add(obj, 'body_indices', this%body_indices)
        end if
        call json%add(obj, 'has_dependencies', this%has_dependencies)
        call json%add(obj, 'is_parallel_safe', this%is_parallel_safe)
        call json%add(parent, obj)
    end subroutine forall_to_json

    subroutine forall_assign(lhs, rhs)
        class(forall_node), intent(inout) :: lhs
        class(forall_node), intent(in) :: rhs
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
        lhs%num_indices = rhs%num_indices
        if (allocated(rhs%index_names)) then
            if (allocated(lhs%index_names)) deallocate(lhs%index_names)
            allocate(lhs%index_names, source=rhs%index_names)
        end if
        if (allocated(rhs%lower_bound_indices)) then
            if (allocated(lhs%lower_bound_indices)) deallocate(lhs%lower_bound_indices)
            allocate(lhs%lower_bound_indices(size(rhs%lower_bound_indices)))
            lhs%lower_bound_indices = rhs%lower_bound_indices
        end if
        if (allocated(rhs%upper_bound_indices)) then
            if (allocated(lhs%upper_bound_indices)) deallocate(lhs%upper_bound_indices)
            allocate(lhs%upper_bound_indices(size(rhs%upper_bound_indices)))
            lhs%upper_bound_indices = rhs%upper_bound_indices
        end if
        if (allocated(rhs%stride_indices)) then
            if (allocated(lhs%stride_indices)) deallocate(lhs%stride_indices)
            allocate(lhs%stride_indices(size(rhs%stride_indices)))
            lhs%stride_indices = rhs%stride_indices
        end if
        lhs%has_mask = rhs%has_mask
        lhs%mask_expr_index = rhs%mask_expr_index
        if (allocated(rhs%body_indices)) then
            if (allocated(lhs%body_indices)) deallocate(lhs%body_indices)
            allocate(lhs%body_indices(size(rhs%body_indices)))
            lhs%body_indices = rhs%body_indices
        end if
        lhs%has_dependencies = rhs%has_dependencies
        lhs%is_parallel_safe = rhs%is_parallel_safe
        if (allocated(rhs%dependency_pairs)) then
            if (allocated(lhs%dependency_pairs)) deallocate(lhs%dependency_pairs)
            allocate(lhs%dependency_pairs, source=rhs%dependency_pairs)
        end if
    end subroutine forall_assign

    ! Factory functions
    function create_do_loop(var_name, start_expr_index, end_expr_index, &
                            step_expr_index, body_indices, line, column) &
                            result(node)
        character(len=*), intent(in) :: var_name
        integer, intent(in) :: start_expr_index, end_expr_index
        integer, intent(in), optional :: step_expr_index
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column
        type(do_loop_node) :: node

        node%uid = generate_uid()
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

        node%uid = generate_uid()
        node%condition_index = condition_index

        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                node%body_indices = body_indices
            end if
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_do_while

end module ast_nodes_loops