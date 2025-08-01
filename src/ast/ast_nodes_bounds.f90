module ast_nodes_bounds
    use ast_base
    use ast_arena, only: ast_arena_t
    use json_module
    implicit none
    private

    public :: array_bounds_node, array_slice_node, range_expression_node
    public :: get_array_bounds_node, get_array_slice_node, get_range_expression_node

    ! Node type constants
    integer, parameter, public :: NODE_ARRAY_BOUNDS = 50
    integer, parameter, public :: NODE_ARRAY_SLICE = 51
    integer, parameter, public :: NODE_RANGE_EXPRESSION = 52

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
    type, extends(ast_node) :: array_slice_node
        integer :: node_type = NODE_ARRAY_SLICE
        integer :: array_index            ! Index to array expression being sliced
        integer :: bounds_indices(10)     ! Indices to array_bounds_node for each dimension
        integer :: num_dimensions = 0     ! Number of dimensions in slice
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

    ! Visitor pattern implementations
    subroutine array_bounds_accept(this, visitor)
        class(array_bounds_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        
        ! Stub implementation for now
    end subroutine array_bounds_accept

    subroutine array_slice_accept(this, visitor)
        class(array_slice_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        
        ! Stub implementation for now
    end subroutine array_slice_accept

    subroutine range_expression_accept(this, visitor)
        class(range_expression_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        
        ! Stub implementation for now
    end subroutine range_expression_accept

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

end module ast_nodes_bounds