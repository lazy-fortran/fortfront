module ast_nodes_bounds
    use ast_base
    use ast_arena_modern, only: ast_arena_t
    implicit none
    private

    public :: array_bounds_node, array_slice_node, range_expression_node, &
        array_operation_node
    public :: get_array_bounds_node, get_array_slice_node, get_range_expression_node, &
        get_array_operation_node
    public :: array_bounds_t, array_spec_t
    ! Constructors migrated from ast_core
    public :: create_array_bounds, create_array_slice, create_range_expression
    public :: create_array_operation

    ! Node type constants
    integer, parameter, public :: NODE_ARRAY_BOUNDS = 50
    integer, parameter, public :: NODE_ARRAY_SLICE = 51
    integer, parameter, public :: NODE_RANGE_EXPRESSION = 52
    integer, parameter, public :: NODE_ARRAY_OPERATION = 53

    ! Type to represent bounds for a single dimension
    type :: array_bounds_t
        integer :: lower_bound_expr = -1 ! Expression index for lower bound
        integer :: upper_bound_expr = -1 ! Expression index for upper bound
        integer :: stride_expr = -1 ! Expression index for stride
        logical :: is_constant_lower = .false.
        logical :: is_constant_upper = .false.
        logical :: is_constant_stride = .false.
        integer :: const_lower = 1 ! Constant value if is_constant_lower
        integer :: const_upper = 1 ! Constant value if is_constant_upper
        integer :: const_stride = 1 ! Constant value if is_constant_stride
        logical :: is_assumed = .false. ! True for assumed shape (:)
        logical :: is_deferred = .false. ! True for deferred shape (:)
        logical :: is_assumed_size = .false. ! True for assumed size (*)
    end type array_bounds_t

    ! Type to represent complete array specification
    type :: array_spec_t
        integer :: rank = 0 ! Number of dimensions
        type(array_bounds_t), allocatable :: bounds(:) ! Bounds for each dimension
        logical :: is_allocatable = .false.
        logical :: is_pointer = .false.
        logical :: is_fixed_size = .true. ! True if all bounds are constant
    end type array_spec_t

    ! Represents array bounds information (lower:upper:stride)
    type, extends(ast_node) :: array_bounds_node
        integer :: node_type = NODE_ARRAY_BOUNDS
        integer :: lower_bound_index = -1 ! Index to lower bound expression
        ! (-1 if implicit)
        integer :: upper_bound_index = -1 ! Index to upper bound expression
        ! (-1 if implicit)
        integer :: stride_index = -1 ! Index to stride expression
        ! (-1 if no stride)
        logical :: is_assumed_shape = .false. ! True for (:) bounds
        logical :: is_deferred_shape = .false. ! True for allocatable/pointer arrays
        logical :: is_assumed_size = .false. ! True for (*) last dimension
        logical :: is_assumed_rank = .false. ! True for (..) assumed-rank arrays
    contains
        procedure :: accept => array_bounds_accept
    end type array_bounds_node

    ! Represents array slice operation arr(bounds1, bounds2, ...)
    ! Note: For single-dimension slices on character types, this could represent
    ! a character substring operation rather than array slicing
    type, extends(ast_node) :: array_slice_node
        integer :: node_type = NODE_ARRAY_SLICE
        integer :: array_index ! Index to array expression being sliced
        integer :: bounds_indices(10) ! Indices to array_bounds_node
        ! for each dimension
        integer :: num_dimensions = 0 ! Number of dimensions in slice
        ! Resolution flag (set during semantic analysis)
        logical :: is_character_substring = .false. ! true if substring
    contains
        procedure :: accept => array_slice_accept
    end type array_slice_node

    ! Represents range expression start:end or start:end:stride
    type, extends(ast_node) :: range_expression_node
        integer :: node_type = NODE_RANGE_EXPRESSION
        integer :: start_index = -1 ! Index to start expression (-1 if implicit)
        integer :: end_index = -1 ! Index to end expression (-1 if implicit)
        integer :: stride_index = -1 ! Index to stride expression (-1 if no stride)
    contains
        procedure :: accept => range_expression_accept
    end type range_expression_node

    ! Represents array operations (assignment, arithmetic, etc.) with bounds info
    type, extends(ast_node) :: array_operation_node
        integer :: node_type = NODE_ARRAY_OPERATION
        character(len=32) :: operation = "" ! Operation type (=, +, -, *, etc.)
        integer :: left_operand_index = -1 ! Left operand (array)
        integer :: right_operand_index = -1 ! Right operand (array or scalar)
        type(array_spec_t) :: array_spec ! Array specification with bounds
        type(array_spec_t) :: result_spec ! Result specification
        logical :: bounds_checked = .false. ! Whether bounds have been validated
        logical :: shape_conformant = .false. ! Whether operands are conformant
    contains
        procedure :: accept => array_operation_accept
    end type array_operation_node

contains

    function get_array_bounds_node(arena, index) result(node)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        type(array_bounds_node), pointer :: node

        node => null()
        if (index > 0 .and. index <= arena%compat_size) then
            select type (p => arena%entries(index)%node)
                type is (array_bounds_node)
                node => p
            end select
        end if
    end function get_array_bounds_node

    function get_array_slice_node(arena, index) result(node)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        type(array_slice_node), pointer :: node

        node => null()
        if (index > 0 .and. index <= arena%compat_size) then
            select type (p => arena%entries(index)%node)
                type is (array_slice_node)
                node => p
            end select
        end if
    end function get_array_slice_node

    function get_range_expression_node(arena, index) result(node)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        type(range_expression_node), pointer :: node

        node => null()
        if (index > 0 .and. index <= arena%compat_size) then
            select type (p => arena%entries(index)%node)
                type is (range_expression_node)
                node => p
            end select
        end if
    end function get_range_expression_node

    function get_array_operation_node(arena, index) result(node)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        type(array_operation_node), pointer :: node

        node => null()
        if (index > 0 .and. index <= arena%compat_size) then
            select type (p => arena%entries(index)%node)
                type is (array_operation_node)
                node => p
            end select
        end if
    end function get_array_operation_node

    ! Constructors
    function create_array_bounds(lower_index, upper_index, stride_index) result(node)
        use uid_generator, only: generate_uid
        integer, intent(in) :: lower_index, upper_index
        integer, intent(in), optional :: stride_index
        type(array_bounds_node) :: node

        node%uid = generate_uid()
        node%lower_bound_index = lower_index
        node%upper_bound_index = upper_index
        if (present(stride_index)) then
            node%stride_index = stride_index
        else
            node%stride_index = -1
        end if
    end function create_array_bounds

    function create_array_slice(array_index, bounds_indices, num_dims) result(node)
        use uid_generator, only: generate_uid
        integer, intent(in) :: array_index
        integer, intent(in) :: bounds_indices(:)
        integer, intent(in) :: num_dims
        type(array_slice_node) :: node
        integer :: i

        node%uid = generate_uid()
        node%array_index = array_index
        if (num_dims < 0) then
            node%num_dimensions = 0
        else
            node%num_dimensions = min(num_dims, size(node%bounds_indices))
        end if
        do i = 1, node%num_dimensions
            if (i <= size(bounds_indices)) then
                node%bounds_indices(i) = bounds_indices(i)
            else
                node%bounds_indices(i) = -1
            end if
        end do
    end function create_array_slice

    function create_range_expression(start_index, end_index, stride_index) result(node)
        use uid_generator, only: generate_uid
        integer, intent(in) :: start_index, end_index
        integer, intent(in), optional :: stride_index
        type(range_expression_node) :: node

        node%uid = generate_uid()
        node%start_index = start_index
        node%end_index = end_index
        if (present(stride_index)) then
            node%stride_index = stride_index
        else
            node%stride_index = -1
        end if
    end function create_range_expression

    function create_array_operation(operation, left_index, right_index, &
            array_spec, result_spec) result(node)
        use uid_generator, only: generate_uid
        character(len=*), intent(in) :: operation
        integer, intent(in) :: left_index, right_index
        type(array_spec_t), intent(in), optional :: array_spec, result_spec
        type(array_operation_node) :: node

        node%uid = generate_uid()
        node%operation = operation
        node%left_operand_index = left_index
        node%right_operand_index = right_index
        if (present(array_spec)) node%array_spec = array_spec
        if (present(result_spec)) node%result_spec = result_spec
        node%bounds_checked = .false.
        node%shape_conformant = .false.
    end function create_array_operation

    ! Visitor pattern implementations
    subroutine array_bounds_accept(this, visitor)
        class(array_bounds_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor

        ! Bounds nodes do not have a visit hook on ast_visitor_t; the
        ! deferred accept on ast_node is satisfied by this no-op.
    end subroutine array_bounds_accept

    subroutine array_slice_accept(this, visitor)
        class(array_slice_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor

        ! Bounds nodes do not have a visit hook on ast_visitor_t; the
        ! deferred accept on ast_node is satisfied by this no-op.
    end subroutine array_slice_accept

    subroutine range_expression_accept(this, visitor)
        class(range_expression_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor

        ! Bounds nodes do not have a visit hook on ast_visitor_t; the
        ! deferred accept on ast_node is satisfied by this no-op.
    end subroutine range_expression_accept

    subroutine array_operation_accept(this, visitor)
        class(array_operation_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor

        ! Bounds nodes do not have a visit hook on ast_visitor_t; the
        ! deferred accept on ast_node is satisfied by this no-op.
    end subroutine array_operation_accept

end module ast_nodes_bounds
