module ast_arena
    use ast_base, only: ast_node
    implicit none
    private

    ! Stack entry for AST nodes  
    type :: ast_entry_t
        class(ast_node), allocatable :: node    ! The AST node itself
        integer :: parent_index = 0             ! Index of parent node in stack (0 for root)
        integer :: depth = 0                    ! Depth in tree (0 for root)
        character(len=:), allocatable :: node_type  ! Type name for debugging
        integer, allocatable :: child_indices(:)    ! Indices of child nodes
        integer :: child_count = 0              ! Number of children
    contains
        procedure :: deep_copy => ast_entry_deep_copy
        procedure :: assign => ast_entry_assign
        generic :: assignment(=) => assign
    end type ast_entry_t

    ! High-performance arena-based AST storage system
    type, public :: ast_arena_t
        type(ast_entry_t), allocatable :: entries(:)  ! Contiguous array of entries
        integer :: size = 0                           ! Current number of entries
        integer :: capacity = 0                       ! Array capacity
        integer :: current_index = 0                  ! Current position in arena
        integer :: max_depth = 0                      ! Maximum depth reached
        integer :: chunk_size = 1024                  ! Default chunk size for growth
        integer :: initial_capacity = 256             ! Starting capacity
    contains
        procedure :: push => ast_arena_push
        procedure :: pop => ast_arena_pop
        procedure :: current => ast_arena_current
        procedure :: get_parent => ast_arena_get_parent
        procedure :: get_depth => ast_arena_get_depth
        procedure :: traverse_depth => ast_arena_traverse_depth
        procedure :: find_by_type => ast_arena_find_by_type
        procedure :: get_children => ast_arena_get_children
        procedure :: get_stats => ast_arena_get_stats
        procedure :: clear => ast_arena_clear
        procedure :: add_child => ast_arena_add_child
        procedure :: shrink_arena
        procedure :: deep_copy => ast_arena_deep_copy
        procedure :: assign => ast_arena_assign
        generic :: assignment(=) => assign
    end type ast_arena_t

    ! Statistics for performance monitoring
    type, public :: ast_arena_stats_t
        integer :: total_nodes = 0
        integer :: max_depth = 0
        integer :: capacity = 0
        integer :: memory_usage = 0  ! Approximate memory usage in bytes
    end type ast_arena_stats_t

    ! Public interface
    public :: create_ast_arena, ast_entry_t

contains

    ! Create new AST arena
    function create_ast_arena(initial_capacity) result(arena)
        integer, intent(in), optional :: initial_capacity
        type(ast_arena_t) :: arena
        integer :: cap

        ! Set defaults first
        arena%chunk_size = 1024  ! High-performance chunk size
        arena%initial_capacity = 256

        ! Use chunk-aligned initial capacity for optimal performance
        if (present(initial_capacity)) then
            cap = max(initial_capacity, 256)  ! Use literal to avoid dependency
        else
            cap = 256  ! Use literal default
        end if

        arena%capacity = cap
        allocate (arena%entries(cap))
        arena%size = 0
        arena%current_index = 0
        arena%max_depth = 0
    end function create_ast_arena

    ! Push a new AST node onto the stack
    subroutine ast_arena_push(this, node, node_type, parent_index)
        class(ast_arena_t), intent(inout) :: this
        class(ast_node), intent(in) :: node
        character(len=*), intent(in), optional :: node_type
        integer, intent(in), optional :: parent_index
        type(ast_entry_t), allocatable :: temp_entries(:)
        integer :: new_capacity, parent_depth, parent_idx, i

        ! Grow array using buffered chunk allocation for high performance
        if (this%size >= this%capacity) then
            if (this%capacity == 0) then
                new_capacity = this%initial_capacity
            else
                ! Grow by chunk_size to minimize allocations
                new_capacity = this%capacity + this%chunk_size
            end if

            ! Preserve existing entries during growth
            allocate (temp_entries(new_capacity))
            if (this%size > 0) then
                do i = 1, this%size
                    temp_entries(i) = this%entries(i)
                end do
            end if
            call move_alloc(temp_entries, this%entries)
            this%capacity = new_capacity
        end if

        ! Add new entry
        this%size = this%size + 1

        ! Ensure the node field is not allocated before allocating
        if (allocated(this%entries(this%size)%node)) then
            deallocate(this%entries(this%size)%node)
        end if
        
        ! Allocate and copy the node
        allocate (this%entries(this%size)%node, source=node)

        ! Set metadata
        if (present(node_type)) then
            this%entries(this%size)%node_type = node_type
        else
            this%entries(this%size)%node_type = "unknown"
        end if

        ! Set parent relationship
        if (present(parent_index)) then
            parent_idx = parent_index
            if (parent_idx > 0 .and. parent_idx <= this%size) then
                this%entries(this%size)%parent_index = parent_idx
                parent_depth = this%entries(parent_idx)%depth
                this%entries(this%size)%depth = parent_depth + 1

                ! Add to parent's children
                call this%add_child(parent_idx, this%size)
            else
                this%entries(this%size)%parent_index = 0
                this%entries(this%size)%depth = 0
            end if
        else
            this%entries(this%size)%parent_index = 0
            this%entries(this%size)%depth = 0
        end if

        ! Update max depth
        this%max_depth = max(this%max_depth, this%entries(this%size)%depth)
        this%current_index = this%size
    end subroutine ast_arena_push

    ! Add child relationship
    subroutine ast_arena_add_child(this, parent_index, child_index)
        class(ast_arena_t), intent(inout) :: this
        integer, intent(in) :: parent_index, child_index
        integer, allocatable :: temp_children(:)
        integer :: i, new_size

        if (parent_index <= 0 .or. parent_index > this%size) return
        if (child_index <= 0 .or. child_index > this%size) return

        ! Grow children array
        if (.not. allocated(this%entries(parent_index)%child_indices)) then
            allocate (this%entries(parent_index)%child_indices(1))
            this%entries(parent_index)%child_indices(1) = child_index
            this%entries(parent_index)%child_count = 1
        else
            new_size = this%entries(parent_index)%child_count + 1
            allocate (temp_children(new_size))
            do i = 1, this%entries(parent_index)%child_count
                temp_children(i) = this%entries(parent_index)%child_indices(i)
            end do
            temp_children(new_size) = child_index
            call move_alloc(temp_children, this%entries(parent_index)%child_indices)
            this%entries(parent_index)%child_count = new_size
        end if
    end subroutine ast_arena_add_child

    ! Stub implementations for other procedures (to be implemented incrementally)
    subroutine ast_arena_pop(this)
        class(ast_arena_t), intent(inout) :: this
        ! Stub implementation
    end subroutine ast_arena_pop

    function ast_arena_current(this) result(node)
        class(ast_arena_t), intent(in) :: this
        class(ast_node), allocatable :: node
        ! Stub implementation
    end function ast_arena_current

    function ast_arena_get_parent(this, index) result(parent_node)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in) :: index
        class(ast_node), allocatable :: parent_node
        ! Stub implementation
    end function ast_arena_get_parent

    function ast_arena_get_depth(this, index) result(depth)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in) :: index
        integer :: depth
        depth = 0
        ! Stub implementation
    end function ast_arena_get_depth

    subroutine ast_arena_traverse_depth(this, target_depth, visitor)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in) :: target_depth
        class(*), intent(inout) :: visitor
        ! Stub implementation
    end subroutine ast_arena_traverse_depth

    function ast_arena_find_by_type(this, node_type) result(indices)
        class(ast_arena_t), intent(in) :: this
        character(len=*), intent(in) :: node_type
        integer, allocatable :: indices(:)
        ! Stub implementation
        allocate (indices(0))
    end function ast_arena_find_by_type

    function ast_arena_get_children(this, parent_index) result(child_indices)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in) :: parent_index
        integer, allocatable :: child_indices(:)
        ! Stub implementation
        allocate (child_indices(0))
    end function ast_arena_get_children

    function ast_arena_get_stats(this) result(stats)
        class(ast_arena_t), intent(in) :: this
        type(ast_arena_stats_t) :: stats
        stats%total_nodes = this%size
        stats%max_depth = this%max_depth
        stats%capacity = this%capacity
        stats%memory_usage = this%capacity * 64  ! Rough estimate
    end function ast_arena_get_stats

    subroutine ast_arena_clear(this)
        class(ast_arena_t), intent(inout) :: this
        this%size = 0
        this%current_index = 0
        this%max_depth = 0
    end subroutine ast_arena_clear

    subroutine shrink_arena(this)
        class(ast_arena_t), intent(inout) :: this
        ! Stub implementation
    end subroutine shrink_arena

    function ast_arena_deep_copy(this) result(copy)
        class(ast_arena_t), intent(in) :: this
        type(ast_arena_t) :: copy
        ! Stub implementation
        copy = create_ast_arena()
    end function ast_arena_deep_copy

    subroutine ast_arena_assign(lhs, rhs)
        class(ast_arena_t), intent(inout) :: lhs
        class(ast_arena_t), intent(in) :: rhs
        
        ! Copy all scalar components
        lhs%size = rhs%size
        lhs%capacity = rhs%capacity
        lhs%current_index = rhs%current_index
        lhs%max_depth = rhs%max_depth
        lhs%chunk_size = rhs%chunk_size
        lhs%initial_capacity = rhs%initial_capacity
        
        ! Copy allocatable array
        if (allocated(rhs%entries)) then
            if (allocated(lhs%entries)) deallocate(lhs%entries)
            allocate(lhs%entries(size(rhs%entries)))
            lhs%entries = rhs%entries
        end if
    end subroutine ast_arena_assign

    function ast_entry_deep_copy(this) result(copy)
        class(ast_entry_t), intent(in) :: this
        type(ast_entry_t) :: copy
        ! Stub implementation
    end function ast_entry_deep_copy

    subroutine ast_entry_assign(lhs, rhs)
        class(ast_entry_t), intent(inout) :: lhs
        class(ast_entry_t), intent(in) :: rhs
        
        ! Deep copy the node if allocated
        if (allocated(lhs%node)) deallocate(lhs%node)
        if (allocated(rhs%node)) then
            allocate(lhs%node, source=rhs%node)
        end if
        
        ! Copy scalar fields
        lhs%parent_index = rhs%parent_index
        lhs%depth = rhs%depth
        lhs%child_count = rhs%child_count
        
        ! Copy allocatable strings
        if (allocated(rhs%node_type)) then
            lhs%node_type = rhs%node_type
        end if
        
        ! Deep copy child indices array
        if (allocated(lhs%child_indices)) deallocate(lhs%child_indices)
        if (allocated(rhs%child_indices)) then
            allocate(lhs%child_indices(size(rhs%child_indices)))
            lhs%child_indices = rhs%child_indices
        end if
    end subroutine ast_entry_assign

end module ast_arena