module ast_arena_compat
    ! Compatibility layer for bridging modern AST arena to old arena API
    ! Provides backward compatibility while using modern arena internally
    
    use ast_base, only: ast_node
    use ast_arena_core, only: ast_arena_core_t, ast_handle_t, ast_node_arena_t, &
                              ast_arena_stats_t, create_ast_arena_core
    implicit none
    private
    
    public :: ast_arena_compat_t, ast_entry_t
    public :: create_ast_arena_compat
    
    ! Extended arena with compatibility layer
    type, extends(ast_arena_core_t) :: ast_arena_compat_t
        ! Compatibility layer for old arena API (public for compatibility)
        type(ast_entry_t), allocatable :: entries(:)    ! Compatibility entry access
        integer :: compat_size = 0                      ! Compatibility: current size
        integer :: max_depth = 0                        ! Compatibility: maximum tree depth
    contains
        ! Compatibility methods for old arena API
        procedure :: push => ast_arena_push_compat
        procedure :: ensure_capacity => ast_arena_ensure_capacity
        procedure :: reset => ast_arena_compat_reset
        procedure :: get_stats => ast_arena_compat_get_stats
        procedure :: get_children => ast_arena_get_children_compat
        procedure :: get_parent => ast_arena_get_parent_compat
        procedure :: get_depth => ast_arena_get_depth_compat
        procedure :: get_next_sibling => ast_arena_get_next_sibling_compat
        procedure :: get_previous_sibling => ast_arena_get_previous_sibling_compat
        procedure :: get_block_statements => ast_arena_get_block_statements_compat
        procedure :: is_last_in_block => ast_arena_is_last_in_block_compat
        procedure :: is_block_node => ast_arena_is_block_node_compat
        procedure :: add_child => ast_arena_add_child_compat
        procedure :: find_by_type => ast_arena_find_by_type_compat
    end type ast_arena_compat_t
    
    ! Compatibility type for bridging to old arena API
    type :: ast_entry_t
        class(ast_node), allocatable :: node    ! The AST node itself
        integer :: parent_index = 0             ! Index of parent node (0 for root)
        integer :: depth = 0                    ! Depth in tree (0 for root)
        character(len=:), allocatable :: node_type  ! Type name for debugging
        integer, allocatable :: child_indices(:)    ! Indices of child nodes
        integer :: child_count = 0              ! Number of children
    contains
        procedure :: deep_copy => ast_entry_deep_copy
        procedure :: assign => ast_entry_assign
        generic :: assignment(=) => assign
    end type ast_entry_t
    
contains

    ! Create arena with compatibility layer
    function create_ast_arena_compat(initial_capacity) result(arena)
        integer, intent(in), optional :: initial_capacity
        type(ast_arena_compat_t) :: arena
        integer :: capacity
        
        capacity = 1024
        if (present(initial_capacity)) capacity = initial_capacity
        
        ! Initialize core arena
        arena%ast_arena_core_t = create_ast_arena_core(capacity)
        
        ! Initialize compatibility layer
        allocate(arena%entries(capacity))
        arena%compat_size = 0
        arena%max_depth = 0
        
        ! CRITICAL FIX: Synchronize base arena capacity field with actual capacity
        arena%capacity = capacity
    end function create_ast_arena_compat
    
    ! Compatibility method: get children indices for parent node
    function ast_arena_get_children_compat(this, parent_index) result(child_indices)
        class(ast_arena_compat_t), intent(in) :: this
        integer, intent(in) :: parent_index
        integer, allocatable :: child_indices(:)
        
        ! Return children indices from compatibility layer
        if (parent_index > 0 .and. parent_index <= this%compat_size) then
            if (allocated(this%entries(parent_index)%child_indices)) then
                allocate(child_indices(this%entries(parent_index)%child_count))
                child_indices = this%entries(parent_index)%child_indices( &
                    1:this%entries(parent_index)%child_count)
            else
                allocate(child_indices(0))
            end if
        else
            allocate(child_indices(0))
        end if
    end function ast_arena_get_children_compat
    
    ! Compatibility method: get parent node (polymorphic return)
    function ast_arena_get_parent_compat(this, index) result(parent_node)
        class(ast_arena_compat_t), intent(in) :: this
        integer, intent(in) :: index
        class(ast_node), allocatable :: parent_node
        integer :: parent_index
        
        if (index > 0 .and. index <= this%compat_size) then
            parent_index = this%entries(index)%parent_index
            if (parent_index > 0 .and. parent_index <= this%compat_size) then
                if (allocated(this%entries(parent_index)%node)) then
                    allocate(parent_node, source=this%entries(parent_index)%node)
                end if
            end if
        end if
    end function ast_arena_get_parent_compat
    
    ! Compatibility method: get node depth
    function ast_arena_get_depth_compat(this, index) result(depth)
        class(ast_arena_compat_t), intent(in) :: this
        integer, intent(in) :: index
        integer :: depth
        
        if (index > 0 .and. index <= this%compat_size) then
            depth = this%entries(index)%depth
        else
            depth = 0
        end if
    end function ast_arena_get_depth_compat
    
    ! Compatibility method: get next sibling
    function ast_arena_get_next_sibling_compat(this, node_index) result(next_sibling)
        class(ast_arena_compat_t), intent(in) :: this
        integer, intent(in) :: node_index
        integer :: next_sibling
        integer :: parent_idx, i
        integer, allocatable :: siblings(:)
        
        next_sibling = 0
        
        if (node_index > 0 .and. node_index <= this%compat_size) then
            parent_idx = this%entries(node_index)%parent_index
            if (parent_idx > 0) then
                siblings = this%get_children(parent_idx)
                
                ! Find current node in parent's children and return next one
                do i = 1, size(siblings)
                    if (siblings(i) == node_index .and. i < size(siblings)) then
                        next_sibling = siblings(i + 1)
                        exit
                    end if
                end do
            end if
        end if
    end function ast_arena_get_next_sibling_compat
    
    ! Compatibility method: get previous sibling  
    function ast_arena_get_previous_sibling_compat(this, node_index) result(prev_sibling)
        class(ast_arena_compat_t), intent(in) :: this
        integer, intent(in) :: node_index
        integer :: prev_sibling
        integer :: parent_idx, i
        integer, allocatable :: siblings(:)
        
        prev_sibling = 0
        
        if (node_index > 0 .and. node_index <= this%compat_size) then
            parent_idx = this%entries(node_index)%parent_index
            if (parent_idx > 0) then
                siblings = this%get_children(parent_idx)
                
                ! Find current node in parent's children and return previous one
                do i = 1, size(siblings)
                    if (siblings(i) == node_index .and. i > 1) then
                        prev_sibling = siblings(i - 1)
                        exit
                    end if
                end do
            end if
        end if
    end function ast_arena_get_previous_sibling_compat
    
    ! Compatibility method: get block statements
    function ast_arena_get_block_statements_compat(this, block_index) result(stmt_indices)
        class(ast_arena_compat_t), intent(in) :: this
        integer, intent(in) :: block_index
        integer, allocatable :: stmt_indices(:)
        
        ! Default: return children of the block node
        stmt_indices = this%get_children(block_index)
    end function ast_arena_get_block_statements_compat
    
    ! Compatibility method: check if node is last in block
    function ast_arena_is_last_in_block_compat(this, node_index) result(is_last)
        class(ast_arena_compat_t), intent(in) :: this
        integer, intent(in) :: node_index
        logical :: is_last
        integer :: next_idx
        
        is_last = .false.
        next_idx = this%get_next_sibling(node_index)
        
        ! If no next sibling, this is the last statement in the block
        is_last = (next_idx == 0)
    end function ast_arena_is_last_in_block_compat
    
    ! Compatibility method: check if node is block type
    function ast_arena_is_block_node_compat(this, node_index) result(is_block)
        class(ast_arena_compat_t), intent(in) :: this
        integer, intent(in) :: node_index
        logical :: is_block
        character(len=:), allocatable :: node_type
        
        is_block = .false.
        
        if (node_index > 0 .and. node_index <= this%compat_size) then
            if (allocated(this%entries(node_index)%node_type)) then
                node_type = this%entries(node_index)%node_type
                
                ! Check for known block types
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
        end if
    end function ast_arena_is_block_node_compat
    
    ! Compatibility method: add child relationship
    subroutine ast_arena_add_child_compat(this, parent_index, child_index)
        class(ast_arena_compat_t), intent(inout) :: this
        integer, intent(in) :: parent_index, child_index
        
        call add_child_compat(this, parent_index, child_index)
    end subroutine ast_arena_add_child_compat
    
    ! Compatibility method: find nodes by type
    function ast_arena_find_by_type_compat(this, type_name) result(indices)
        class(ast_arena_compat_t), intent(in) :: this
        character(len=*), intent(in) :: type_name
        integer, allocatable :: indices(:)
        
        integer :: i, count
        integer, allocatable :: temp_indices(:)
        
        ! Count matching nodes first
        count = 0
        do i = 1, this%compat_size
            if (allocated(this%entries(i)%node_type)) then
                if (this%entries(i)%node_type == type_name) then
                    count = count + 1
                end if
            end if
        end do
        
        ! Allocate result array
        allocate(indices(count))
        if (count == 0) return
        
        ! Fill result array
        count = 0
        do i = 1, this%compat_size
            if (allocated(this%entries(i)%node_type)) then
                if (this%entries(i)%node_type == type_name) then
                    count = count + 1
                    indices(count) = i
                end if
            end if
        end do
    end function ast_arena_find_by_type_compat
    
    ! Compatibility push method for old AST factory code
    subroutine ast_arena_push_compat(this, node, node_type, parent_index)
        class(ast_arena_compat_t), intent(inout) :: this
        class(ast_node), intent(in) :: node
        character(len=*), intent(in), optional :: node_type
        integer, intent(in), optional :: parent_index
        
        ! Ensure compatibility array has capacity (triggers growth if needed)
        call this%ensure_capacity()
        
        ! Add to compatibility layer
        this%compat_size = this%compat_size + 1
        
        ! Store in compatibility entries array
        if (allocated(this%entries(this%compat_size)%node)) deallocate(this%entries(this%compat_size)%node)
        allocate(this%entries(this%compat_size)%node, source=node)
        
        ! Set metadata
        if (present(node_type)) then
            this%entries(this%compat_size)%node_type = node_type
        else
            this%entries(this%compat_size)%node_type = "unknown"
        end if
        
        ! Set parent relationship
        if (present(parent_index)) then
            this%entries(this%compat_size)%parent_index = parent_index
            if (parent_index > 0 .and. parent_index <= this%compat_size) then
                this%entries(this%compat_size)%depth = this%entries(parent_index)%depth + 1
                
                ! Add this child to parent's children list
                call add_child_compat(this, parent_index, this%compat_size)
            else
                this%entries(this%compat_size)%depth = 0
            end if
        else
            this%entries(this%compat_size)%parent_index = 0
            this%entries(this%compat_size)%depth = 0
        end if
        
        ! Update max depth tracking
        this%max_depth = max(this%max_depth, this%entries(this%compat_size)%depth)
        
        ! Update node count to stay in sync
        call this%increment_node_count()
    end subroutine ast_arena_push_compat
    
    ! Add child relationship in compatibility layer
    subroutine add_child_compat(arena, parent_index, child_index)
        type(ast_arena_compat_t), intent(inout) :: arena
        integer, intent(in) :: parent_index, child_index
        
        ! Grow children array using Fortran array extension syntax
        if (.not. allocated(arena%entries(parent_index)%child_indices)) then
            allocate(arena%entries(parent_index)%child_indices(1))
            arena%entries(parent_index)%child_indices(1) = child_index
            arena%entries(parent_index)%child_count = 1
        else
            ! Use Fortran array extension syntax 
            arena%entries(parent_index)%child_indices = &
                [arena%entries(parent_index)%child_indices, child_index]
            arena%entries(parent_index)%child_count = &
                arena%entries(parent_index)%child_count + 1
        end if
    end subroutine add_child_compat
    
    ! Ensure compatibility array has sufficient capacity
    subroutine ast_arena_ensure_capacity(this)
        class(ast_arena_compat_t), intent(inout) :: this
        type(ast_entry_t), allocatable :: temp_entries(:)
        type(ast_arena_stats_t) :: stats
        integer :: new_capacity, core_capacity
        
        if (.not. allocated(this%entries)) then
            stats = this%get_stats()
            core_capacity = stats%capacity  ! Use core arena capacity from stats
            new_capacity = max(core_capacity, 1024)
            allocate(this%entries(new_capacity))
            ! CRITICAL FIX: Synchronize base arena capacity field
            this%capacity = new_capacity
            return
        end if
        
        stats = this%get_stats()
        core_capacity = stats%capacity  ! Current core arena capacity from stats
        
        ! Grow compatibility array if needed (also ensure capacity growth)
        if (this%compat_size >= size(this%entries)) then
            new_capacity = max(size(this%entries) * 2, this%compat_size + 1024)
            
            allocate(temp_entries(new_capacity))
            if (this%compat_size > 0) then
                temp_entries(1:this%compat_size) = this%entries(1:this%compat_size)
            end if
            
            call move_alloc(temp_entries, this%entries)
            
            ! CRITICAL FIX: Synchronize base arena capacity field with new capacity
            this%capacity = new_capacity
        end if
    end subroutine ast_arena_ensure_capacity
    
    ! Override get_stats to return compatibility layer statistics
    function ast_arena_compat_get_stats(this) result(stats)
        class(ast_arena_compat_t), intent(in) :: this
        type(ast_arena_stats_t) :: stats
        
        ! Get base stats from core arena
        stats = this%ast_arena_core_t%get_stats()
        
        ! Override with compatibility layer information
        stats%total_nodes = this%compat_size
        stats%max_depth = this%max_depth
        
        ! Use compatibility array size as capacity
        if (allocated(this%entries)) then
            stats%capacity = size(this%entries)
        else
            stats%capacity = 0
        end if
        
        ! Update other relevant fields
        stats%node_count = this%compat_size
        stats%active_nodes = this%compat_size
    end function ast_arena_compat_get_stats
    
    ! Compatibility AST entry deep copy
    function ast_entry_deep_copy(this) result(copy)
        class(ast_entry_t), intent(in) :: this
        type(ast_entry_t) :: copy
        
        ! Copy scalar fields
        copy%parent_index = this%parent_index
        copy%depth = this%depth
        copy%child_count = this%child_count
        
        ! Copy allocatable fields
        if (allocated(this%node_type)) then
            copy%node_type = this%node_type
        end if
        
        if (allocated(this%child_indices)) then
            copy%child_indices = this%child_indices
        end if
        
        if (allocated(this%node)) then
            allocate(copy%node, source=this%node)
        end if
    end function ast_entry_deep_copy
    
    ! Compatibility AST entry assignment
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
        else
            if (allocated(lhs%node_type)) deallocate(lhs%node_type)
        end if
        
        ! Copy child indices
        if (allocated(rhs%child_indices)) then
            lhs%child_indices = rhs%child_indices
        else
            if (allocated(lhs%child_indices)) deallocate(lhs%child_indices)
        end if
        
        ! Copy polymorphic node
        if (allocated(lhs%node)) deallocate(lhs%node)
        if (allocated(rhs%node)) then
            allocate(lhs%node, source=rhs%node)
        end if
    end subroutine ast_entry_assign
    
    ! Override reset to also reset compatibility layer fields
    subroutine ast_arena_compat_reset(this)
        class(ast_arena_compat_t), intent(inout) :: this
        
        ! Call parent reset method to reset core arena
        call this%ast_arena_core_t%reset()
        
        ! Reset compatibility layer state
        this%compat_size = 0
        this%max_depth = 0
        
        ! Clear entries if allocated (but keep the array allocated for reuse)
        if (allocated(this%entries)) then
            ! Deallocate individual entry components for clean slate
            ! but keep the entries array allocated for efficiency
        end if
    end subroutine ast_arena_compat_reset
    
end module ast_arena_compat