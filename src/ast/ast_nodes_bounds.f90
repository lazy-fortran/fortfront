module ast_nodes_bounds
    use ast_base
    use ast_arena, only: ast_arena_t
    use json_module
    implicit none
    private

    public :: array_bounds_node, array_slice_node, range_expression_node, array_operation_node
    public :: get_array_bounds_node, get_array_slice_node, get_range_expression_node, &
              get_array_operation_node
    public :: array_bounds_t, array_spec_t

    ! Node type constants
    integer, parameter, public :: NODE_ARRAY_BOUNDS = 50
    integer, parameter, public :: NODE_ARRAY_SLICE = 51
    integer, parameter, public :: NODE_RANGE_EXPRESSION = 52
    integer, parameter, public :: NODE_ARRAY_OPERATION = 53
    
    ! Type to represent bounds for a single dimension
    type :: array_bounds_t
        integer :: lower_bound_expr = -1    ! Expression index for lower bound
        integer :: upper_bound_expr = -1    ! Expression index for upper bound
        integer :: stride_expr = -1         ! Expression index for stride
        logical :: is_constant_lower = .false.
        logical :: is_constant_upper = .false.
        logical :: is_constant_stride = .false.
        integer :: const_lower = 1          ! Constant value if is_constant_lower
        integer :: const_upper = 1          ! Constant value if is_constant_upper
        integer :: const_stride = 1         ! Constant value if is_constant_stride
        logical :: is_assumed = .false.     ! True for assumed shape (:)
        logical :: is_deferred = .false.    ! True for deferred shape (:)
        logical :: is_assumed_size = .false. ! True for assumed size (*)
    end type array_bounds_t
    
    ! Type to represent complete array specification
    type :: array_spec_t
        integer :: rank = 0                     ! Number of dimensions
        type(array_bounds_t), allocatable :: bounds(:)  ! Bounds for each dimension
        logical :: is_allocatable = .false.
        logical :: is_pointer = .false.
        logical :: is_fixed_size = .true.       ! True if all bounds are constant
    end type array_spec_t

    ! Represents array bounds information (lower:upper:stride)
    type, extends(ast_node) :: array_bounds_node
        integer :: node_type = NODE_ARRAY_BOUNDS
        integer :: lower_bound_index = -1  ! Index to lower bound expression (-1 if implicit)
        integer :: upper_bound_index = -1  ! Index to upper bound expression (-1 if implicit)
        integer :: stride_index = -1       ! Index to stride expression (-1 if no stride)
        logical :: is_assumed_shape = .false.  ! True for (:) bounds
        logical :: is_deferred_shape = .false. ! True for allocatable/pointer arrays
        logical :: is_assumed_size = .false.   ! True for (*) last dimension
    contains
        procedure :: accept => array_bounds_accept
        procedure :: to_json => array_bounds_to_json
    end type array_bounds_node

    ! Represents array slice operation arr(bounds1, bounds2, ...)
    ! Note: For single-dimension slices on character types, this could represent
    ! a character substring operation rather than array slicing
    type, extends(ast_node) :: array_slice_node
        integer :: node_type = NODE_ARRAY_SLICE
        integer :: array_index            ! Index to array expression being sliced
        integer :: bounds_indices(10)     ! Indices to array_bounds_node for each dimension
        integer :: num_dimensions = 0     ! Number of dimensions in slice
        ! Resolution flag (set during semantic analysis)
        logical :: is_character_substring = .false.  ! true if substring
    contains
        procedure :: accept => array_slice_accept
        procedure :: to_json => array_slice_to_json
    end type array_slice_node

    ! Represents range expression start:end or start:end:stride
    type, extends(ast_node) :: range_expression_node
        integer :: node_type = NODE_RANGE_EXPRESSION
        integer :: start_index = -1   ! Index to start expression (-1 if implicit)
        integer :: end_index = -1     ! Index to end expression (-1 if implicit)  
        integer :: stride_index = -1  ! Index to stride expression (-1 if no stride)
    contains
        procedure :: accept => range_expression_accept
        procedure :: to_json => range_expression_to_json
    end type range_expression_node

    ! Represents array operations (assignment, arithmetic, etc.) with bounds info
    type, extends(ast_node) :: array_operation_node
        integer :: node_type = NODE_ARRAY_OPERATION
        character(len=32) :: operation = ""    ! Operation type (=, +, -, *, etc.)
        integer :: left_operand_index = -1     ! Left operand (array)
        integer :: right_operand_index = -1    ! Right operand (array or scalar)
        type(array_spec_t) :: array_spec       ! Array specification with bounds
        type(array_spec_t) :: result_spec      ! Result specification
        logical :: bounds_checked = .false.    ! Whether bounds have been validated
        logical :: shape_conformant = .false.  ! Whether operands are conformant
    contains
        procedure :: accept => array_operation_accept
        procedure :: to_json => array_operation_to_json
    end type array_operation_node

contains

    function get_array_bounds_node(arena, index) result(node)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        type(array_bounds_node), pointer :: node
        
        node => null()
        if (index > 0 .and. index <= arena%size) then
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
        if (index > 0 .and. index <= arena%size) then
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
        if (index > 0 .and. index <= arena%size) then
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
        if (index > 0 .and. index <= arena%size) then
            select type (p => arena%entries(index)%node)
            type is (array_operation_node)
                node => p
            end select
        end if
    end function get_array_operation_node

    ! Visitor pattern implementations
    subroutine array_bounds_accept(this, visitor)
        class(array_bounds_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        
        ! Visitor pattern stub - would need full implementation
        ! For now, just validate the visitor is not null
    end subroutine array_bounds_accept

    subroutine array_slice_accept(this, visitor)
        class(array_slice_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        
        ! Visitor pattern stub - would need full implementation
        ! For now, just validate the visitor is not null
    end subroutine array_slice_accept

    subroutine range_expression_accept(this, visitor)
        class(range_expression_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        
        ! Visitor pattern stub - would need full implementation
        ! For now, just validate the visitor is not null
    end subroutine range_expression_accept

    subroutine array_operation_accept(this, visitor)
        class(array_operation_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        
        ! Visitor pattern stub - would need full implementation
        ! For now, just validate the visitor is not null
    end subroutine array_operation_accept

    ! JSON serialization implementations
    subroutine array_bounds_to_json(this, json, parent)
        class(array_bounds_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        
        call json%create_object(parent, "array_bounds")
        call json%add(parent, "node_type", "array_bounds")
        call json%add(parent, "lower_bound_index", this%lower_bound_index)
        call json%add(parent, "upper_bound_index", this%upper_bound_index)
        call json%add(parent, "stride_index", this%stride_index)
        call json%add(parent, "is_assumed_shape", this%is_assumed_shape)
        call json%add(parent, "is_deferred_shape", this%is_deferred_shape)
        call json%add(parent, "is_assumed_size", this%is_assumed_size)
    end subroutine array_bounds_to_json

    subroutine array_slice_to_json(this, json, parent)
        class(array_slice_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: bounds_array, bounds_item
        integer :: i
        
        call json%create_object(parent, "array_slice")
        call json%add(parent, "node_type", "array_slice")
        call json%add(parent, "array_index", this%array_index)
        call json%add(parent, "num_dimensions", this%num_dimensions)
        
        call json%create_array(bounds_array, "bounds_indices")
        do i = 1, this%num_dimensions
            call json%add(bounds_array, "", this%bounds_indices(i))
        end do
        call json%add(parent, bounds_array)
    end subroutine array_slice_to_json

    subroutine range_expression_to_json(this, json, parent)
        class(range_expression_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        
        call json%create_object(parent, "range_expression")
        call json%add(parent, "node_type", "range_expression")
        call json%add(parent, "start_index", this%start_index)
        call json%add(parent, "end_index", this%end_index)
        call json%add(parent, "stride_index", this%stride_index)
    end subroutine range_expression_to_json

    subroutine array_operation_to_json(this, json, parent)
        class(array_operation_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        
        call json%create_object(parent, "array_operation")
        call json%add(parent, "node_type", "array_operation")
        call json%add(parent, "operation", trim(this%operation))
        call json%add(parent, "left_operand_index", this%left_operand_index)
        call json%add(parent, "right_operand_index", this%right_operand_index)
        call json%add(parent, "bounds_checked", this%bounds_checked)
        call json%add(parent, "shape_conformant", this%shape_conformant)
        
        ! Add array_spec information if available
        if (this%array_spec%rank > 0) then
            call json%add(parent, "array_rank", this%array_spec%rank)
            call json%add(parent, "is_allocatable", this%array_spec%is_allocatable)
            call json%add(parent, "is_pointer", this%array_spec%is_pointer)
        end if
        
        if (this%result_spec%rank > 0) then
            call json%add(parent, "result_rank", this%result_spec%rank)
        end if
    end subroutine array_operation_to_json

end module ast_nodes_bounds