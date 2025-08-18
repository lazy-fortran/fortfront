module ast_arena
    use ast_base, only: ast_node
    implicit none
    private

    ! DEPRECATED: This module implements a monolithic arena design with architectural issues
    ! WARNING: Monolithic arena creates tight coupling, memory pressure, and scalability bottlenecks
    ! MIGRATE TO: ast_hierarchical_factory for distributed node management with reference counting
    ! DEPRECATION TIMELINE: Will be removed in favor of hierarchical architecture
    ! 
    ! ARCHITECTURAL ISSUES:
    ! 1. Monolithic storage forces O(n) memory growth and tight coupling
    ! 2. Index-based references create fragile dependencies without automatic cleanup  
    ! 3. Large contiguous allocations cause memory pressure and poor cache locality
    ! 4. No component isolation prevents parallel processing and testing
    !
    ! MIGRATION: See DOCS/AST_ARENA_ARCHITECTURE_ANALYSIS.md for migration guide

    ! Stack entry for AST nodes  
    type :: ast_entry_t
        class(ast_node), allocatable :: node    ! The AST node itself
        integer :: parent_index = 0    ! Index of parent node in stack (0 for root)
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
        procedure :: get_next_sibling => ast_arena_get_next_sibling
        procedure :: get_previous_sibling => ast_arena_get_previous_sibling
        procedure :: get_block_statements => ast_arena_get_block_statements
        procedure :: is_last_in_block => ast_arena_is_last_in_block
        procedure :: is_block_node => ast_arena_is_block_node
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
    public :: create_ast_arena, ast_entry_t, init_ast_arena

contains

    ! Create new AST arena
    function create_ast_arena(initial_capacity) result(arena)
        integer, intent(in), optional :: initial_capacity
        type(ast_arena_t) :: arena
        integer :: cap

        ! DEPRECATION WARNING: Arena pattern has architectural issues
        ! Consider migrating to ast_hierarchical_factory for better memory management
        
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

    ! Initialize AST arena (subroutine version to avoid assignment)
    subroutine init_ast_arena(arena, initial_capacity)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: initial_capacity
        integer :: cap
        
        ! Set defaults first
        arena%chunk_size = 1024  ! High-performance chunk size
        arena%initial_capacity = 256
        
        ! Use chunk-aligned initial capacity for optimal performance
        if (present(initial_capacity)) then
            cap = max(initial_capacity, 256)
        else
            cap = 256
        end if
        
        arena%capacity = cap
        allocate (arena%entries(cap))
        arena%size = 0
        arena%current_index = 0
        arena%max_depth = 0
    end subroutine init_ast_arena

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
                    ! Explicitly copy each field to avoid assignment operator issues
                    temp_entries(i)%parent_index = this%entries(i)%parent_index
                    temp_entries(i)%depth = this%entries(i)%depth
                    temp_entries(i)%child_count = this%entries(i)%child_count
                    
                    ! Copy node_type string
                    if (allocated(this%entries(i)%node_type)) then
                        temp_entries(i)%node_type = this%entries(i)%node_type
                    end if
                    
                    ! Copy child indices array
                    if (allocated(this%entries(i)%child_indices)) then
                        temp_entries(i)%child_indices = this%entries(i)%child_indices
                    end if
                    
                    ! Explicitly deep copy the polymorphic node
                    if (allocated(this%entries(i)%node)) then
                        allocate(temp_entries(i)%node, source=this%entries(i)%node)
                    end if
                end do
            end if
            call move_alloc(temp_entries, this%entries)
            this%capacity = new_capacity
        end if

        ! Add new entry
        this%size = this%size + 1

        ! Perform safe deep copy for polymorphic AST nodes
        if (allocated(this%entries(this%size)%node)) then
            ! This should not happen for new entries, but handle it safely
            deallocate(this%entries(this%size)%node)
        end if
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
            if (parent_idx > 0) then
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

        ! Grow children array using Fortran array extension syntax
        if (.not. allocated(this%entries(parent_index)%child_indices)) then
            allocate (this%entries(parent_index)%child_indices(1))
            this%entries(parent_index)%child_indices(1) = child_index
            this%entries(parent_index)%child_count = 1
        else
            ! Use Fortran array extension syntax as per CLAUDE.md policy
            this%entries(parent_index)%child_indices = &
                [this%entries(parent_index)%child_indices, child_index]
            this%entries(parent_index)%child_count = &
                this%entries(parent_index)%child_count + 1
        end if
    end subroutine ast_arena_add_child

    ! Pop the last node from the arena
    subroutine ast_arena_pop(this)
        class(ast_arena_t), intent(inout) :: this
        integer :: parent_idx, i, j
        
        ! Get parent of the node being removed
        parent_idx = this%entries(this%size)%parent_index
        
        ! Remove this node from parent's children list if it has a parent
        if (parent_idx > 0) then
            if (allocated(this%entries(parent_idx)%child_indices)) then
                ! Find and remove this node from parent's children
                do i = 1, this%entries(parent_idx)%child_count
                    if (this%entries(parent_idx)%child_indices(i) == this%size) then
                        ! Shift remaining children left
                        do j = i, this%entries(parent_idx)%child_count - 1
                            this%entries(parent_idx)%child_indices(j) = &
                                this%entries(parent_idx)%child_indices(j + 1)
                        end do
                        this%entries(parent_idx)%child_count = &
                            this%entries(parent_idx)%child_count - 1
                        exit
                    end if
                end do
            end if
        end if
        
        ! Reset the entry fields
        this%entries(this%size)%parent_index = 0
        this%entries(this%size)%depth = 0
        this%entries(this%size)%child_count = 0
        
        ! Decrement size
        this%size = this%size - 1
        
        ! Update current index
        if (this%current_index > this%size) then
            this%current_index = this%size
        end if
    end subroutine ast_arena_pop

    function ast_arena_current(this) result(node)
        class(ast_arena_t), intent(in) :: this
        class(ast_node), allocatable :: node
        
        ! Return deep copy of the node at current_index
        if (this%current_index > 0 .and. this%current_index <= this%size) then
            if (allocated(this%entries(this%current_index)%node)) then
                allocate(node, source=this%entries(this%current_index)%node)
            end if
        end if
    end function ast_arena_current

    function ast_arena_get_parent(this, index) result(parent_node)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in) :: index
        class(ast_node), allocatable :: parent_node
        integer :: parent_index
        
        ! Get parent node with bounds checking
        if (index > 0 .and. index <= this%size) then
            parent_index = this%entries(index)%parent_index
            if (parent_index > 0 .and. parent_index <= this%size) then
                if (allocated(this%entries(parent_index)%node)) then
                    allocate(parent_node, source=this%entries(parent_index)%node)
                end if
            end if
        end if
    end function ast_arena_get_parent

    function ast_arena_get_depth(this, index) result(depth)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in) :: index
        integer :: depth
        
        depth = this%entries(index)%depth
    end function ast_arena_get_depth

    subroutine ast_arena_traverse_depth(this, target_depth, visitor)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in) :: target_depth
        class(*), intent(inout) :: visitor
        integer :: i
        
        ! Visit all nodes at the target depth
        do i = 1, this%size
            if (this%entries(i)%depth == target_depth) then
                if (allocated(this%entries(i)%node)) then
                    ! Call visitor on node - simplified interface
                    ! In a full implementation, would use visitor pattern properly
                end if
            end if
        end do
    end subroutine ast_arena_traverse_depth

    function ast_arena_find_by_type(this, node_type) result(indices)
        class(ast_arena_t), intent(in) :: this
        character(len=*), intent(in) :: node_type
        integer, allocatable :: indices(:)
        integer, allocatable :: temp(:)
        integer :: i, count
        
        ! Count matching nodes
        count = 0
        do i = 1, this%size
            if (allocated(this%entries(i)%node_type)) then
                if (this%entries(i)%node_type == node_type) then
                    count = count + 1
                end if
            end if
        end do
        
        ! Allocate result array
        allocate(indices(count))
        
        ! Fill with matching indices
        if (count > 0) then
            count = 0
            do i = 1, this%size
                if (allocated(this%entries(i)%node_type)) then
                    if (this%entries(i)%node_type == node_type) then
                        count = count + 1
                        indices(count) = i
                    end if
                end if
            end do
        end if
    end function ast_arena_find_by_type

    function ast_arena_get_children(this, parent_index) result(child_indices)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in) :: parent_index
        integer, allocatable :: child_indices(:)
        
        ! Return children indices for the parent node
        if (allocated(this%entries(parent_index)%child_indices)) then
            allocate(child_indices(this%entries(parent_index)%child_count))
            child_indices = &
                this%entries(parent_index)%child_indices( &
                    1:this%entries(parent_index)%child_count)
        else
            allocate(child_indices(0))
        end if
    end function ast_arena_get_children

    ! Get next sibling node in the same parent
    function ast_arena_get_next_sibling(this, node_index) result(next_sibling)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in) :: node_index
        integer :: next_sibling
        integer :: parent_idx, i
        integer, allocatable :: siblings(:)
        
        next_sibling = 0
        
        parent_idx = this%entries(node_index)%parent_index
        if (parent_idx <= 0) return  ! No parent = no siblings
        
        siblings = this%get_children(parent_idx)
        
        ! Find current node in parent's children and return next one
        do i = 1, size(siblings)
            if (siblings(i) == node_index .and. i < size(siblings)) then
                next_sibling = siblings(i + 1)
                exit
            end if
        end do
    end function ast_arena_get_next_sibling

    ! Get previous sibling node in the same parent
    function ast_arena_get_previous_sibling(this, node_index) result(prev_sibling)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in) :: node_index
        integer :: prev_sibling
        integer :: parent_idx, i
        integer, allocatable :: siblings(:)
        
        prev_sibling = 0
        
        parent_idx = this%entries(node_index)%parent_index
        if (parent_idx <= 0) return  ! No parent = no siblings
        
        siblings = this%get_children(parent_idx)
        
        ! Find current node in parent's children and return previous one
        do i = 1, size(siblings)
            if (siblings(i) == node_index .and. i > 1) then
                prev_sibling = siblings(i - 1)
                exit
            end if
        end do
    end function ast_arena_get_previous_sibling

    ! Get all statements in a block (for block nodes like if, do, etc.)
    function ast_arena_get_block_statements(this, block_index) result(stmt_indices)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in) :: block_index
        integer, allocatable :: stmt_indices(:)
        
        ! Default: return children of the block node
        stmt_indices = this%get_children(block_index)
    end function ast_arena_get_block_statements

    ! Check if a statement is the last executable statement in its block
    function ast_arena_is_last_in_block(this, node_index) result(is_last)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in) :: node_index
        logical :: is_last
        integer :: next_idx
        
        is_last = .false.
        next_idx = this%get_next_sibling(node_index)
        
        ! If no next sibling, this is the last statement in the block
        is_last = (next_idx == 0)
    end function ast_arena_is_last_in_block

    ! Check if a node represents a block (contains statements)
    function ast_arena_is_block_node(this, node_index) result(is_block)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in) :: node_index
        logical :: is_block
        character(len=:), allocatable :: node_type
        
        is_block = .false.
        
        if (allocated(this%entries(node_index)%node_type)) then
            node_type = this%entries(node_index)%node_type
            
            ! Check for known block types (using actual arena node type strings)
            is_block = (node_type == "if_statement" .or. &
                       node_type == "do_loop" .or. &
                       node_type == "do_while" .or. &
                       node_type == "forall" .or. &
                       node_type == "where" .or. &
                       node_type == "select_case" .or. &
                       node_type == "function_def" .or. &
                       node_type == "subroutine_def" .or. &
                       node_type == "program" .or. &
                       node_type == "module")
        end if
    end function ast_arena_is_block_node

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
        type(ast_entry_t), allocatable :: temp_entries(:)
        integer :: i, new_capacity
        
        ! Only shrink if we have more capacity than needed plus some buffer
        if (this%capacity > this%size + this%chunk_size) then
            ! Calculate new capacity with some buffer, check for overflow
            if (this%size > huge(this%size) - this%chunk_size / 2) then
                new_capacity = this%initial_capacity
            else
                new_capacity = max(this%size + this%chunk_size / 2, &
                                   this%initial_capacity)
            end if
            
            if (new_capacity < this%capacity) then
                ! Allocate smaller array
                allocate(temp_entries(new_capacity))
                
                ! Copy existing entries
                if (this%size > 0) then
                    do i = 1, this%size
                        temp_entries(i) = this%entries(i)
                    end do
                end if
                
                ! Replace entries array
                call move_alloc(temp_entries, this%entries)
                this%capacity = new_capacity
            end if
        end if
    end subroutine shrink_arena

    function ast_arena_deep_copy(this) result(copy)
        class(ast_arena_t), intent(in) :: this
        type(ast_arena_t) :: copy
        integer :: i
        
        ! Create new arena with same capacity
        copy = create_ast_arena(this%capacity)
        
        ! Copy all scalar fields
        copy%size = this%size
        copy%current_index = this%current_index
        copy%max_depth = this%max_depth
        copy%chunk_size = this%chunk_size
        copy%initial_capacity = this%initial_capacity
        
        ! Deep copy all entries
        if (this%size > 0) then
            do i = 1, this%size
                copy%entries(i) = this%entries(i)%deep_copy()
            end do
        end if
    end function ast_arena_deep_copy

    subroutine ast_arena_assign(lhs, rhs)
        class(ast_arena_t), intent(inout) :: lhs
        class(ast_arena_t), intent(in) :: rhs
        integer :: i
        
        ! Copy all scalar components
        lhs%size = rhs%size
        lhs%capacity = rhs%capacity
        lhs%current_index = rhs%current_index
        lhs%max_depth = rhs%max_depth
        lhs%chunk_size = rhs%chunk_size
        lhs%initial_capacity = rhs%initial_capacity
        
        ! Deep copy allocatable array using Fortran automatic reallocation
        if (allocated(rhs%entries)) then
            ! Use slice assignment to trigger automatic reallocation
            lhs%entries = rhs%entries(1:rhs%capacity)
            do i = 1, rhs%size
                lhs%entries(i) = rhs%entries(i)  ! Uses ast_entry_assign
            end do
        end if
    end subroutine ast_arena_assign

    function ast_entry_deep_copy(this) result(copy)
        class(ast_entry_t), intent(in) :: this
        type(ast_entry_t) :: copy
        
        ! Copy scalar fields
        copy%parent_index = this%parent_index
        copy%depth = this%depth
        copy%child_count = this%child_count
        
        ! Copy node_type string
        if (allocated(this%node_type)) then
            copy%node_type = this%node_type
        end if
        
        ! Deep copy child indices array
        if (allocated(this%child_indices)) then
            allocate(copy%child_indices(size(this%child_indices)))
            copy%child_indices = this%child_indices
        end if
        
        ! Deep copy AST node
        if (allocated(this%node)) then
            allocate(copy%node, source=this%node)
        end if
    end function ast_entry_deep_copy

    subroutine ast_entry_assign(lhs, rhs)
        class(ast_entry_t), intent(inout) :: lhs
        class(ast_entry_t), intent(in) :: rhs
        
        ! Copy scalar fields
        lhs%parent_index = rhs%parent_index
        lhs%depth = rhs%depth
        lhs%child_count = rhs%child_count
        
        ! Copy allocatable strings
        if (allocated(rhs%node_type)) then
            lhs%node_type = rhs%node_type
        end if
        
        ! Copy child indices using automatic reallocation
        if (allocated(rhs%child_indices)) then
            lhs%child_indices = rhs%child_indices  ! Automatic allocation for regular arrays
        end if
        
        ! Deep copy the polymorphic node using explicit allocation
        if (allocated(lhs%node)) deallocate(lhs%node)
        if (allocated(rhs%node)) then
            allocate(lhs%node, source=rhs%node)
        end if
    end subroutine ast_entry_assign

end module ast_arena