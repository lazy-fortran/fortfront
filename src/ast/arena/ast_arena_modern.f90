module ast_arena_modern
    ! AST arena with indexed nodes and generation-based handles

    use ast_base, only: ast_node
    use ast_nodes_data, only: declaration_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node, &
        subroutine_call_node
    use fortfront_constants, only: AST_ARENA_GROWTH_MINIMUM
    use ast_arena_core, only: ast_arena_core_t, ast_handle_t, ast_node_arena_t, &
        ast_arena_stats_t, ast_free_result_t, &
        create_ast_arena_core, destroy_ast_arena_core, &
        is_valid_ast_handle, null_ast_handle
    implicit none

    ! Indexed AST entry
    type :: ast_entry_t
        class(ast_node), allocatable :: node ! The AST node itself
        integer :: parent_index = 0 ! Index of parent node (0 for root)
        integer :: depth = 0 ! Depth in tree (0 for root)
        character(len=:), allocatable :: node_type ! Type name for debugging
        integer, allocatable :: child_indices(:) ! Indices of child nodes
        integer :: child_count = 0 ! Number of children
    contains
        procedure :: deep_copy => ast_entry_deep_copy
        procedure :: assign => ast_entry_assign
        generic :: assignment(=) => assign
    end type ast_entry_t

    ! Re-export core types and functions
    public :: ast_arena_t, ast_handle_t, ast_node_arena_t, ast_entry_t
    public :: create_ast_arena, destroy_ast_arena
    public :: store_ast_node, get_ast_node, is_valid_ast_handle, null_ast_handle
    public :: ast_arena_stats_t, ast_free_result_t
    ! Safe indexed helpers
    public :: has_node_at, get_node_line, get_node_column
    public :: get_inferred_kind_at, get_inferred_details_at
    public :: free_ast_node, is_node_active, get_free_statistics
    ! Child linking helper for factory functions
    public :: link_children_to_parent

    type, extends(ast_arena_core_t) :: ast_arena_t
        type(ast_entry_t), allocatable :: entries(:)
        integer :: entry_count = 0
        integer :: current_index = 0
        integer :: max_depth = 0
        character(len=:), allocatable :: source_text
        integer, allocatable :: source_line_starts(:)
    contains
        procedure :: clear => clear_ast_arena
        procedure :: push => ast_arena_push_with_size_sync
        procedure :: ensure_capacity => ast_arena_ensure_capacity
        procedure :: reset => ast_arena_indexed_reset
        procedure :: get_stats => ast_arena_indexed_get_stats
        procedure :: get_children => ast_arena_get_children_indexed
        procedure :: get_parent => ast_arena_get_parent_indexed
        procedure :: get_depth => ast_arena_get_depth_indexed
        procedure :: get_next_sibling => ast_arena_get_next_sibling_indexed
        procedure :: get_previous_sibling => ast_arena_get_previous_sibling_indexed
        procedure :: get_block_statements => ast_arena_get_block_statements_indexed
        procedure :: is_last_in_block => ast_arena_is_last_in_block_indexed
        procedure :: is_block_node => ast_arena_is_block_node_indexed
        procedure :: add_child => ast_arena_add_child_indexed
        procedure :: find_by_type => ast_arena_find_by_type_indexed
        ! Safe, index-based helpers
        procedure :: has_node_at
        procedure :: get_node_line
        procedure :: get_node_column
        procedure :: get_inferred_kind_at
        procedure :: get_inferred_details_at
        procedure :: assign_modern => ast_arena_modern_assign
        generic :: assignment(=) => assign_modern
    end type ast_arena_t

    interface free_ast_node
        module procedure free_ast_node_modern
    end interface free_ast_node

    interface is_node_active
        module procedure is_node_active_modern
    end interface is_node_active

    interface get_free_statistics
        module procedure get_free_statistics_modern
    end interface get_free_statistics

    interface store_ast_node
        module procedure store_ast_node_modern
    end interface store_ast_node

    interface get_ast_node
        module procedure get_ast_node_modern
    end interface get_ast_node

    private :: free_ast_node_modern, is_node_active_modern, get_free_statistics_modern
    private :: store_ast_node_modern, get_ast_node_modern

contains

    ! gfortran 12 loses elements of allocatable deferred-length character
    ! arrays when an AST node is copied through a polymorphic SOURCE
    ! allocation.  Declarations use such an array for multi-name entities;
    ! copy those nodes through their defined assignment instead.
    subroutine copy_ast_node_indexed(destination, source)
        class(ast_node), allocatable, intent(out) :: destination
        class(ast_node), intent(in) :: source

        select type (source)
        type is (declaration_node)
            allocate (declaration_node :: destination)
            select type (destination)
            type is (declaration_node)
                destination = source
            end select
        type is (function_def_node)
            allocate (function_def_node :: destination)
            select type (destination)
            type is (function_def_node)
                destination = source
            end select
        type is (subroutine_def_node)
            allocate (subroutine_def_node :: destination)
            select type (destination)
            type is (subroutine_def_node)
                destination = source
            end select
        type is (subroutine_call_node)
            allocate (subroutine_call_node :: destination)
            select type (destination)
            type is (subroutine_call_node)
                destination = source
            end select
        class default
            allocate (destination, source=source)
        end select
    end subroutine copy_ast_node_indexed

    ! Indexed method: get children indices for parent node
    function ast_arena_get_children_indexed(this, parent_index) result(child_indices)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in) :: parent_index
        integer, allocatable :: child_indices(:)

        ! Return children indices from indexed entries
        if (parent_index > 0 .and. parent_index <= this%entry_count) then
            if (allocated(this%entries(parent_index)%child_indices)) then
                allocate (child_indices(this%entries(parent_index)%child_count))
                child_indices = this%entries(parent_index)%child_indices( &
                    1:this%entries(parent_index)%child_count)
            else
                allocate (child_indices(0))
            end if
        else
            allocate (child_indices(0))
        end if
    end function ast_arena_get_children_indexed

    ! Indexed method: get parent node (polymorphic return)
    function ast_arena_get_parent_indexed(this, index) result(parent_node)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in) :: index
        class(ast_node), allocatable :: parent_node
        integer :: parent_index

        if (index > 0 .and. index <= this%entry_count) then
            parent_index = this%entries(index)%parent_index
            if (parent_index > 0 .and. parent_index <= this%entry_count) then
                if (allocated(this%entries(parent_index)%node)) then
                    call copy_ast_node_indexed(parent_node, &
                        this%entries(parent_index)%node)
                end if
            end if
        end if
    end function ast_arena_get_parent_indexed

    ! Indexed method: get node depth
    function ast_arena_get_depth_indexed(this, index) result(depth)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in) :: index
        integer :: depth

        if (index > 0 .and. index <= this%entry_count) then
            depth = this%entries(index)%depth
        else
            depth = 0
        end if
    end function ast_arena_get_depth_indexed

    ! Indexed method: get next sibling
    function ast_arena_get_next_sibling_indexed(this, node_index) result(next_sibling)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in) :: node_index
        integer :: next_sibling
        integer :: parent_idx, i
        integer, allocatable :: siblings(:)

        next_sibling = 0

        if (node_index > 0 .and. node_index <= this%entry_count) then
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
    end function ast_arena_get_next_sibling_indexed

    ! Indexed method: get previous sibling
    function ast_arena_get_previous_sibling_indexed(this, node_index) &
            result(prev_sibling)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in) :: node_index
        integer :: prev_sibling
        integer :: parent_idx, i
        integer, allocatable :: siblings(:)

        prev_sibling = 0

        if (node_index > 0 .and. node_index <= this%entry_count) then
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
    end function ast_arena_get_previous_sibling_indexed

    ! Indexed method: get block statements
    function ast_arena_get_block_statements_indexed(this, block_index) &
            result(stmt_indices)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in) :: block_index
        integer, allocatable :: stmt_indices(:)

        ! Default: return children of the block node
        stmt_indices = this%get_children(block_index)
    end function ast_arena_get_block_statements_indexed

    ! Indexed method: check if node is last in block
    function ast_arena_is_last_in_block_indexed(this, node_index) result(is_last)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in) :: node_index
        logical :: is_last
        integer :: next_idx

        is_last = .false.
        next_idx = this%get_next_sibling(node_index)

        ! If no next sibling, this is the last statement in the block
        is_last = (next_idx == 0)
    end function ast_arena_is_last_in_block_indexed

    ! Indexed method: check if node is block type
    function ast_arena_is_block_node_indexed(this, node_index) result(is_block)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in) :: node_index
        logical :: is_block
        character(len=:), allocatable :: node_type

        is_block = .false.

        if (node_index > 0 .and. node_index <= this%entry_count) then
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
    end function ast_arena_is_block_node_indexed

    ! Indexed method: add child relationship
    subroutine ast_arena_add_child_indexed(this, parent_index, child_index)
        class(ast_arena_t), intent(inout) :: this
        integer, intent(in) :: parent_index, child_index

        call add_child_indexed(this, parent_index, child_index)
    end subroutine ast_arena_add_child_indexed

    ! Indexed method: find nodes by type
    function ast_arena_find_by_type_indexed(this, type_name) result(indices)
        class(ast_arena_t), intent(in) :: this
        character(len=*), intent(in) :: type_name
        integer, allocatable :: indices(:)

        integer :: i, count

        ! Count matching nodes first
        count = 0
        do i = 1, this%entry_count
            if (allocated(this%entries(i)%node_type)) then
                if (this%entries(i)%node_type == type_name) then
                    count = count + 1
                end if
            end if
        end do

        ! Allocate result array
        allocate (indices(count))
        if (count == 0) return

        ! Fill result array
        count = 0
        do i = 1, this%entry_count
            if (allocated(this%entries(i)%node_type)) then
                if (this%entries(i)%node_type == type_name) then
                    count = count + 1
                    indices(count) = i
                end if
            end if
        end do
    end function ast_arena_find_by_type_indexed

    ! Indexed push method for old AST factory code
    subroutine ast_arena_push_indexed(this, node, node_type, parent_index)
        class(ast_arena_t), intent(inout) :: this
        class(ast_node), intent(in) :: node
        character(len=*), intent(in), optional :: node_type
        integer, intent(in), optional :: parent_index

        ! Ensure indexed entry array has capacity (triggers growth if needed)
        call this%ensure_capacity()

        ! Add to indexed entries
        this%entry_count = this%entry_count + 1

        ! Store in indexed entries array
        ! Newly grown entries should be empty; check before replacing.
        if (allocated(this%entries(this%entry_count)%node)) then
            deallocate (this%entries(this%entry_count)%node)
        end if
        call copy_ast_node_indexed(this%entries(this%entry_count)%node, node)

        ! Set metadata
        if (present(node_type)) then
            this%entries(this%entry_count)%node_type = node_type
        else
            this%entries(this%entry_count)%node_type = "unknown"
        end if

        ! Set parent relationship
        if (present(parent_index)) then
            this%entries(this%entry_count)%parent_index = parent_index
            if (parent_index > 0 .and. parent_index <= this%entry_count) then
                this%entries(this%entry_count)%depth = &
                    this%entries(parent_index)%depth + 1

                ! Add this child to parent's children list
                call add_child_indexed(this, parent_index, this%entry_count)
            else
                this%entries(this%entry_count)%depth = 0
            end if
        else
            this%entries(this%entry_count)%parent_index = 0
            this%entries(this%entry_count)%depth = 0
        end if

        ! Update max depth tracking
        this%max_depth = max(this%max_depth, this%entries(this%entry_count)%depth)

        ! Update node count to stay in sync
        call this%increment_node_count()
    end subroutine ast_arena_push_indexed

    ! Add child relationship in indexed entries
    subroutine add_child_indexed(arena, parent_index, child_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: parent_index, child_index
        integer, allocatable :: temp_children(:)
        integer :: new_count

        ! Optimized children array growth with O(1) amortized complexity
        if (.not. allocated(arena%entries(parent_index)%child_indices)) then
            ! Initialize with reasonable capacity to avoid frequent reallocations
            allocate (arena%entries(parent_index)%child_indices(8))
            arena%entries(parent_index)%child_indices(1) = child_index
            arena%entries(parent_index)%child_count = 1
        else
            new_count = arena%entries(parent_index)%child_count + 1

            ! Only reallocate when we exceed capacity
            if (new_count > size(arena%entries(parent_index)%child_indices)) then
                ! Double the capacity for amortized O(1) performance
                allocate (temp_children( &
                    size(arena%entries(parent_index)%child_indices) * 2))
                temp_children(1:arena%entries(parent_index)%child_count) = &
                    arena%entries(parent_index)%child_indices( &
                    1:arena%entries(parent_index)%child_count)
                call move_alloc(temp_children, &
                    arena%entries(parent_index)%child_indices)
            end if

            ! Add new child at the end
            arena%entries(parent_index)%child_indices(new_count) = child_index
            arena%entries(parent_index)%child_count = new_count
        end if
    end subroutine add_child_indexed

    ! Ensure indexed entry array has sufficient capacity
    subroutine ast_arena_ensure_capacity(this)
        class(ast_arena_t), intent(inout) :: this
        type(ast_entry_t), allocatable :: temp_entries(:)
        type(ast_arena_stats_t) :: stats
        integer :: new_capacity, core_capacity

        if (.not. allocated(this%entries)) then
            stats = this%get_stats()
            core_capacity = stats%capacity ! Use core arena capacity from stats
            new_capacity = max(core_capacity, AST_ARENA_GROWTH_MINIMUM)
            allocate (this%entries(new_capacity))
            ! CRITICAL FIX: Synchronize base arena capacity field
            this%capacity = new_capacity
            return
        end if

        stats = this%get_stats()
        core_capacity = stats%capacity ! Current core arena capacity from stats

        ! Grow indexed entries if needed
        if (this%entry_count >= size(this%entries)) then
            new_capacity = max(size(this%entries) * 2, &
                this%entry_count + AST_ARENA_GROWTH_MINIMUM)

            allocate (temp_entries(new_capacity))
            if (this%entry_count > 0) then
                ! PERFORMANCE FIX: Manually move entries to avoid expensive deep copying
                call move_entries_fast(this%entries(1:this%entry_count), &
                    temp_entries(1:this%entry_count))
            end if

            call move_alloc(temp_entries, this%entries)

            ! CRITICAL FIX: Synchronize base arena capacity field with new capacity
            this%capacity = new_capacity
        end if
    end subroutine ast_arena_ensure_capacity

    ! Report indexed entry statistics
    function ast_arena_indexed_get_stats(this) result(stats)
        class(ast_arena_t), intent(in) :: this
        type(ast_arena_stats_t) :: stats

        ! Get base stats from core arena
        stats = this%ast_arena_core_t%get_stats()

        ! Report indexed entry information
        stats%total_nodes = this%entry_count
        stats%max_depth = this%max_depth

        ! Use indexed entry array size as capacity
        if (allocated(this%entries)) then
            stats%capacity = size(this%entries)
        else
            stats%capacity = 0
        end if

        ! Update other relevant fields
        stats%node_count = this%entry_count
        stats%active_nodes = this%entry_count
    end function ast_arena_indexed_get_stats

    ! Indexed AST entry deep copy
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
            call copy_ast_node_indexed(copy%node, this%node)
        end if
    end function ast_entry_deep_copy

    ! Indexed AST entry assignment - MEMORY SAFE VERSION
    subroutine ast_entry_assign(lhs, rhs)
        class(ast_entry_t), intent(inout) :: lhs
        class(ast_entry_t), intent(in) :: rhs

        ! Copy scalar fields
        lhs%parent_index = rhs%parent_index
        lhs%depth = rhs%depth
        lhs%child_count = rhs%child_count

        ! MEMORY SAFETY: Clean up allocatable strings safely
        if (allocated(lhs%node_type)) deallocate (lhs%node_type)
        if (allocated(rhs%node_type)) then
            lhs%node_type = rhs%node_type
        end if

        ! MEMORY SAFETY: Clean up child indices safely
        if (allocated(lhs%child_indices)) deallocate (lhs%child_indices)
        if (allocated(rhs%child_indices)) then
            lhs%child_indices = rhs%child_indices
        end if

        ! MEMORY SAFETY: Clean up polymorphic node safely
        if (allocated(lhs%node)) deallocate (lhs%node)
        if (allocated(rhs%node)) then
            call copy_ast_node_indexed(lhs%node, rhs%node)
        end if
    end subroutine ast_entry_assign

    ! Reset core and indexed entries - MEMORY SAFE VERSION
    subroutine ast_arena_indexed_reset(this)
        class(ast_arena_t), intent(inout) :: this
        integer :: i

        ! Call parent reset method to reset core arena
        call this%ast_arena_core_t%reset()

        ! MEMORY SAFETY: Properly clean up all entry components
        if (allocated(this%entries)) then
            do i = 1, min(this%entry_count, size(this%entries))
                ! Clean up polymorphic nodes
                if (allocated(this%entries(i)%node)) then
                    deallocate (this%entries(i)%node)
                end if
                ! Clean up allocatable strings
                if (allocated(this%entries(i)%node_type)) then
                    deallocate (this%entries(i)%node_type)
                end if
                ! Clean up child indices
                if (allocated(this%entries(i)%child_indices)) then
                    deallocate (this%entries(i)%child_indices)
                end if
                ! Reset scalar fields
                this%entries(i)%parent_index = 0
                this%entries(i)%depth = 0
                this%entries(i)%child_count = 0
            end do
        end if

        ! Reset indexed entry state
        this%entry_count = 0
        this%max_depth = 0
    end subroutine ast_arena_indexed_reset

    ! Fast entry movement using move_alloc to avoid deep copying - MEMORY SAFE VERSION
    subroutine move_entries_fast(source, dest)
        type(ast_entry_t), intent(inout) :: source(:)
        type(ast_entry_t), intent(inout) :: dest(:)
        integer :: i, max_entries

        ! MEMORY SAFETY: Ensure we don't exceed array bounds
        max_entries = min(size(source), size(dest))

        do i = 1, max_entries
            ! MEMORY SAFETY: Clean up destination first to avoid leaks
            if (allocated(dest(i)%node)) deallocate (dest(i)%node)
            if (allocated(dest(i)%node_type)) deallocate (dest(i)%node_type)
            if (allocated(dest(i)%child_indices)) deallocate (dest(i)%child_indices)

            ! Move scalar fields
            dest(i)%parent_index = source(i)%parent_index
            dest(i)%depth = source(i)%depth
            dest(i)%child_count = source(i)%child_count

            ! Move allocatable strings using move_alloc for performance
            if (allocated(source(i)%node_type)) then
                call move_alloc(source(i)%node_type, dest(i)%node_type)
            end if

            ! Move child indices array using move_alloc
            if (allocated(source(i)%child_indices)) then
                call move_alloc(source(i)%child_indices, dest(i)%child_indices)
            end if

            ! Move polymorphic node using move_alloc (no copying!)
            if (allocated(source(i)%node)) then
                call move_alloc(source(i)%node, dest(i)%node)
            end if
        end do
    end subroutine move_entries_fast

    subroutine destroy_indexed_entries(arena)
        type(ast_arena_t), intent(inout) :: arena
        integer :: i

        if (.not. allocated(arena%entries)) return

        do i = 1, size(arena%entries)
            if (allocated(arena%entries(i)%node)) deallocate (arena%entries(i)%node)
            if (allocated(arena%entries(i)%node_type)) then
                deallocate (arena%entries(i)%node_type)
            end if
            if (allocated(arena%entries(i)%child_indices)) then
                deallocate (arena%entries(i)%child_indices)
            end if
            arena%entries(i)%parent_index = 0
            arena%entries(i)%depth = 0
            arena%entries(i)%child_count = 0
        end do
    end subroutine destroy_indexed_entries

    ! Create AST arena with indexed entries
    function create_ast_arena(initial_capacity) result(arena)
        integer, intent(in), optional :: initial_capacity
        type(ast_arena_t) :: arena
        integer :: capacity

        ! Set default capacity first - PERFORMANCE FIX: Start small for simple programs
        capacity = 16 ! Minimal starting size, will grow as needed
        if (present(initial_capacity)) capacity = initial_capacity

        arena%ast_arena_core_t = create_ast_arena_core(capacity)
        allocate (arena%entries(capacity))
        arena%capacity = capacity
        arena%size = 0 ! Initialize size from base_arena_t
        arena%generation = 1 ! Initialize generation from base_arena_t
    end function create_ast_arena

    ! Push with indexed size synchronization
    subroutine ast_arena_push_with_size_sync(this, node, node_type, parent_index)
        class(ast_arena_t), intent(inout) :: this
        class(ast_node), intent(in) :: node
        character(len=*), intent(in), optional :: node_type
        integer, intent(in), optional :: parent_index

        call ast_arena_push_indexed(this, node, node_type, parent_index)

        ! Sync size field with entry_count
        this%size = this%entry_count

        ! CRITICAL FIX: Sync capacity field to prevent validation errors
        if (allocated(this%entries)) then
            this%capacity = size(this%entries)
        else
            this%capacity = 0
        end if
    end subroutine ast_arena_push_with_size_sync

    ! Destroy AST arena
    subroutine destroy_ast_arena(arena)
        type(ast_arena_t), intent(inout) :: arena

        if (allocated(arena%source_text)) deallocate (arena%source_text)
        if (allocated(arena%source_line_starts)) deallocate (arena%source_line_starts)
        call destroy_ast_arena_core(arena%ast_arena_core_t)

        call destroy_indexed_entries(arena)
        if (allocated(arena%entries)) deallocate (arena%entries)
        arena%entry_count = 0
        arena%max_depth = 0
        arena%size = 0
        arena%capacity = 0
        arena%generation = 0
    end subroutine destroy_ast_arena

    ! Free an AST node
    function free_ast_node_modern(arena, handle) result(free_result)
        type(ast_arena_t), intent(inout) :: arena
        type(ast_handle_t), intent(in) :: handle
        type(ast_free_result_t) :: free_result

        ! Use the core implementation
        free_result = arena%ast_arena_core_t%free_node(handle)

        ! Sync entry_count with actual node count for statistics
        if (free_result%success) then
            arena%entry_count = &
                arena%ast_arena_core_t%get_node_count()
        end if
    end function free_ast_node_modern

    ! Check if node is active
    function is_node_active_modern(arena, handle) result(is_active)
        type(ast_arena_t), intent(inout) :: arena
        type(ast_handle_t), intent(in) :: handle
        logical :: is_active

        ! Use the core implementation
        is_active = arena%ast_arena_core_t%is_active(handle)
    end function is_node_active_modern

    ! Get free statistics
    function get_free_statistics_modern(arena) result(stats)
        type(ast_arena_t), intent(inout) :: arena
        type(ast_arena_stats_t) :: stats

        ! Use the core implementation's free stats directly
        stats = arena%ast_arena_core_t%get_free_stats()
    end function get_free_statistics_modern

    ! Store an AST node
    function store_ast_node_modern(arena, node) result(ast_handle)
        use ast_arena_core, only: core_store => store_ast_node
        type(ast_arena_t), intent(inout) :: arena
        type(ast_node_arena_t), intent(in) :: node
        type(ast_handle_t) :: ast_handle

        ! Delegate to core function
        ast_handle = core_store(arena%ast_arena_core_t, node)

        ! Sync entry_count with actual node count for statistics
        if (is_valid_ast_handle(ast_handle)) then
            arena%entry_count = &
                arena%ast_arena_core_t%get_node_count()
        end if
    end function store_ast_node_modern

    ! Get an AST node
    function get_ast_node_modern(arena, handle) result(arena_node)
        use ast_arena_core, only: core_get => get_ast_node
        type(ast_arena_t), intent(inout) :: arena
        type(ast_handle_t), intent(in) :: handle
        type(ast_node_arena_t) :: arena_node

        ! Delegate to core function
        arena_node = core_get(arena%ast_arena_core_t, handle)
    end function get_ast_node_modern

    ! Clear/reset arena
    subroutine clear_ast_arena(this)
        class(ast_arena_t), intent(inout) :: this

        ! Reset core and indexed entries
        call this%reset()

        ! Sync entry_count with actual node count (should be 0 after reset)
        this%entry_count = &
            this%ast_arena_core_t%get_node_count()
        this%size = this%entry_count
        if (allocated(this%entries)) then
            this%capacity = size(this%entries)
        else
            this%capacity = 0
        end if

        if (allocated(this%source_text)) deallocate (this%source_text)
        if (allocated(this%source_line_starts)) deallocate (this%source_line_starts)
    end subroutine clear_ast_arena

    ! =============================
    ! Safe, index-based introspection
    ! These helpers encapsulate indexed entries
    ! and provide read-only accessors commonly needed by clients.
    ! =============================

    pure logical function has_node_at(this, index) result(has)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in) :: index
        integer :: valid_size

        has = .false.
        if (index <= 0) return
        if (.not. allocated(this%entries)) return
        valid_size = min(this%size, size(this%entries))
        if (index > valid_size) return
        has = allocated(this%entries(index)%node)
    end function has_node_at

    pure integer function get_node_line(this, index) result(line)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in) :: index
        line = 0
        if (.not. this%has_node_at(index)) return
        line = this%entries(index)%node%line
    end function get_node_line

    pure integer function get_node_column(this, index) result(column)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in) :: index
        column = 0
        if (.not. this%has_node_at(index)) return
        column = this%entries(index)%node%column
    end function get_node_column

    pure integer function get_inferred_kind_at(this, index) result(kind)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in) :: index
        kind = 0
        if (.not. this%has_node_at(index)) return
        if (this%entries(index)%node%inferred_type%kind <= 0) return
        kind = this%entries(index)%node%inferred_type%kind
    end function get_inferred_kind_at

    subroutine get_inferred_details_at(this, index, kind, type_size, &
            is_allocatable, is_pointer, found)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in) :: index
        integer, intent(out) :: kind, type_size
        logical, intent(out) :: is_allocatable, is_pointer, found

        kind = 0
        type_size = 0
        is_allocatable = .false.
        is_pointer = .false.
        found = .false.

        if (.not. this%has_node_at(index)) return
        if (this%entries(index)%node%inferred_type%kind <= 0) return

        associate (t => this%entries(index)%node%inferred_type)
            kind = t%kind
            type_size = t%size
            is_allocatable = t%alloc_info%is_allocatable
            is_pointer = t%alloc_info%is_pointer
            found = .true.
        end associate
    end subroutine get_inferred_details_at

    ! Link an array of child indices to a parent node
    ! This establishes parent-child relationships in the arena for AST traversal
    ! If a child already has a different parent, it is removed from that parent first
    subroutine link_children_to_parent(arena, parent_index, child_indices)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: parent_index
        integer, intent(in) :: child_indices(:)

        integer :: i, child_index
        integer :: valid_size
        integer :: current_parent

        if (.not. allocated(arena%entries)) return

        valid_size = arena%entry_count
        if (valid_size <= 0) return
        if (parent_index <= 0 .or. parent_index > valid_size) return
        if (.not. allocated(arena%entries(parent_index)%node)) return

        do i = 1, size(child_indices)
            child_index = child_indices(i)
            if (child_index <= 0 .or. child_index > valid_size) cycle
            if (.not. allocated(arena%entries(child_index)%node)) cycle
            if (child_index == parent_index) cycle

            current_parent = arena%entries(child_index)%parent_index
            if (current_parent > 0 .and. current_parent /= parent_index) then
                call remove_child_from_parent(arena, current_parent, child_index)
            end if

            arena%entries(child_index)%parent_index = parent_index
            arena%entries(child_index)%depth = arena%entries(parent_index)%depth + 1
            if (arena%entries(child_index)%depth > arena%max_depth) then
                arena%max_depth = arena%entries(child_index)%depth
            end if

            if (.not. is_child_present(arena, parent_index, child_index)) then
                call append_child_index(arena, parent_index, child_index)
            end if
        end do
    end subroutine link_children_to_parent

    subroutine append_child_index(arena, parent_index, child_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: parent_index, child_index

        integer, allocatable :: tmp(:)
        integer :: count, new_cap

        if (.not. allocated(arena%entries(parent_index)%child_indices)) then
            new_cap = 4
            allocate (arena%entries(parent_index)%child_indices(new_cap))
            arena%entries(parent_index)%child_count = 1
            arena%entries(parent_index)%child_indices(1) = child_index
            return
        end if

        count = arena%entries(parent_index)%child_count
        if (count < 0) count = 0
        if (count >= size(arena%entries(parent_index)%child_indices)) then
            new_cap = max(2 * &
                size(arena%entries(parent_index)%child_indices), count + 1)
            allocate (tmp(new_cap))
            if (count > 0) then
                tmp(1:count) = arena%entries(parent_index)%child_indices(1:count)
            end if
            call move_alloc(tmp, arena%entries(parent_index)%child_indices)
        end if

        arena%entries(parent_index)%child_count = count + 1
        arena%entries(parent_index)%child_indices(count + 1) = child_index
    end subroutine append_child_index

    ! Check if child is already present in parent's children list
    pure logical function is_child_present(arena, parent_index, child_index) &
            result(present)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: parent_index, child_index
        integer :: i, count, arr_size, valid_size

        present = .false.
        ! Bounds check for parent_index to prevent out-of-bounds access
        if (.not. allocated(arena%entries)) return
        ! Use entry_count for logical size validation
        valid_size = arena%entry_count
        if (valid_size <= 0) return
        if (parent_index <= 0 .or. parent_index > valid_size) return
        if (.not. allocated(arena%entries(parent_index)%child_indices)) return
        count = arena%entries(parent_index)%child_count
        if (count <= 0) return
        arr_size = size(arena%entries(parent_index)%child_indices)
        if (count > arr_size) count = arr_size
        do i = 1, count
            if (arena%entries(parent_index)%child_indices(i) == child_index) then
                present = .true.
                return
            end if
        end do
    end function is_child_present

    ! Remove a child from a parent's children list
    subroutine remove_child_from_parent(arena, parent_index, child_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: parent_index, child_index
        integer :: i, j, old_count, valid_size

        ! Bounds check: ensure parent_index is valid
        if (.not. allocated(arena%entries)) return
        ! Use entry_count for logical size validation
        valid_size = arena%entry_count
        if (valid_size <= 0) return
        if (parent_index <= 0 .or. parent_index > valid_size) return
        if (.not. allocated(arena%entries(parent_index)%child_indices)) return
        old_count = arena%entries(parent_index)%child_count
        if (old_count == 0) return
        ! Ensure old_count does not exceed array bounds
        if (old_count > size(arena%entries(parent_index)%child_indices)) then
            old_count = size(arena%entries(parent_index)%child_indices)
        end if

        j = 0
        do i = 1, old_count
            if (arena%entries(parent_index)%child_indices(i) /= child_index) then
                j = j + 1
                arena%entries(parent_index)%child_indices(j) = &
                    arena%entries(parent_index)%child_indices(i)
            end if
        end do
        arena%entries(parent_index)%child_count = j
    end subroutine remove_child_from_parent

    ! Deep-copy assignment for ast_arena_t
    ! Copies core, indexed entries and source fields
    subroutine ast_arena_modern_assign(lhs, rhs)
        class(ast_arena_t), intent(out) :: lhs
        type(ast_arena_t), intent(in) :: rhs
        integer :: i

        lhs%ast_arena_core_t = rhs%ast_arena_core_t
        lhs%entry_count = rhs%entry_count
        lhs%current_index = rhs%current_index
        lhs%max_depth = rhs%max_depth
        if (allocated(rhs%entries)) then
            allocate (lhs%entries(size(rhs%entries)))
            do i = 1, size(rhs%entries)
                lhs%entries(i) = rhs%entries(i)
            end do
        end if

        ! Copy source_text
        if (allocated(rhs%source_text)) then
            lhs%source_text = rhs%source_text
        end if

        ! Copy source_line_starts
        if (allocated(rhs%source_line_starts)) then
            allocate (lhs%source_line_starts(size(rhs%source_line_starts)))
            lhs%source_line_starts = rhs%source_line_starts
        end if
    end subroutine ast_arena_modern_assign

end module ast_arena_modern
