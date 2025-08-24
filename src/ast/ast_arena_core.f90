module ast_arena_core
    ! Core AST arena implementation with generation-based handles
    ! Focused on essential arena operations without compatibility layer
    
    use ast_base, only: ast_node
    use iso_fortran_env, only: int64
    use arena_memory, only: base_arena_t, arena_handle_t, arena_checkpoint_t
    implicit none
    private
    
    public :: ast_arena_core_t, ast_handle_t, ast_node_arena_t, ast_arena_stats_t
    public :: create_ast_arena_core, destroy_ast_arena_core
    public :: store_ast_node, get_ast_node, is_valid_ast_handle, null_ast_handle
    public :: ast_free_result_t, free_ast_node, is_node_active, get_free_statistics
    
    ! Global counter for unique arena IDs
    integer, save :: next_arena_id = 1
    
    ! Arena-based AST node storage (without allocatables to avoid GCC Bug 114612)
    type :: ast_node_arena_t
        character(len=64) :: node_type_name = ""         ! Node type for debugging
        integer :: node_kind = 0                         ! Numeric node kind
        integer :: parent_handle_id = 0                  ! Parent handle ID
        integer :: parent_handle_gen = 0                 ! Parent handle generation
        integer :: first_child_id = 0                    ! First child handle ID
        integer :: first_child_gen = 0                   ! First child generation
        integer :: next_sibling_id = 0                   ! Next sibling handle ID
        integer :: next_sibling_gen = 0                  ! Next sibling generation
        integer :: depth = 0                             ! Depth in AST tree
        integer :: child_count = 0                       ! Number of direct children
        
        ! Node-specific data (expandable without breaking compatibility)
        character(len=256) :: string_data = ""           ! String content (names, literals)
        integer :: integer_data = 0                      ! Integer content (indices, counts)
        logical :: boolean_data = .false.                ! Boolean flags
        real :: real_data = 0.0                         ! Real values
        
        ! Extended metadata
        integer :: source_line = 0                       ! Source code line number
        integer :: source_column = 0                     ! Source code column
        integer :: source_length = 0                     ! Length in source
        logical :: is_synthetic = .false.                ! Generated during transformation
        logical :: is_visited = .false.                  ! For traversal algorithms
    end type ast_node_arena_t
    
    ! Handle for safe AST node references
    type :: ast_handle_t
        integer :: node_id = 0                           ! Node ID in arena
        integer :: generation = 0                        ! Generation for validation
        integer :: arena_id = 0                          ! Arena identifier for cross-arena safety
    end type ast_handle_t
    
    ! Core AST arena with slot management (extends base_arena_t)
    type, extends(base_arena_t) :: ast_arena_core_t
        private
        ! All internal fields are private - use public methods for access
        type(ast_node_arena_t), allocatable :: nodes(:)      ! slot storage
        integer,               allocatable :: slot_gen(:)    ! per-slot generation  
        integer,               allocatable :: free_stack(:)  ! freelist (indices)
        integer :: cap = 0                               ! arena capacity
        integer :: free_top = 0                         ! freelist top pointer
        integer :: node_count = 0                       ! Number of stored nodes
        integer :: epoch = 1                            ! Current epoch for resets
        integer :: total_allocations = 0                ! Performance counter
        integer :: total_validations = 0                ! Validation counter
        integer :: arena_id = 0                         ! Unique arena identifier
        logical :: is_initialized = .false.             ! Initialization state
    contains
        ! Base arena interface implementations
        procedure :: insert => ast_arena_insert
        procedure :: get => ast_arena_get
        procedure :: valid => ast_arena_valid
        procedure :: free => ast_arena_free
        
        ! Override base implementations
        procedure :: reset => ast_arena_reset
        procedure :: checkpoint => ast_arena_checkpoint
        procedure :: rollback => ast_arena_rollback
        
        ! Core AST operations
        procedure :: get_stats => ast_arena_get_stats
        procedure :: validate => ast_arena_validate_handle
        procedure :: get_node_count => ast_arena_get_node_count
        procedure :: get_arena_id => ast_arena_get_arena_id
        procedure :: free_node => ast_arena_free_node
        procedure :: is_active => ast_arena_is_node_active
        procedure :: get_free_stats => ast_arena_get_free_stats
        procedure :: assign_ast_arena => ast_arena_assign
        generic :: assignment(=) => assign_ast_arena
        
        ! Private field accessors for base interface module
        procedure :: get_cap => ast_arena_get_cap
        procedure :: get_free_top => ast_arena_get_free_top
        procedure :: set_free_top => ast_arena_set_free_top
        procedure :: get_slot_gen => ast_arena_get_slot_gen
        procedure :: set_slot_gen => ast_arena_set_slot_gen
        procedure :: get_free_stack => ast_arena_get_free_stack
        procedure :: set_free_stack => ast_arena_set_free_stack
        procedure :: increment_node_count => ast_arena_increment_node_count
        procedure :: decrement_node_count => ast_arena_decrement_node_count
    end type ast_arena_core_t
    
    ! Statistics for performance monitoring
    type :: ast_arena_stats_t
        integer :: node_count = 0                       ! Number of AST nodes
        integer(int64) :: total_memory = 0              ! Total memory usage
        real :: utilization = 0.0                       ! Memory utilization ratio
        integer :: total_allocations = 0                ! Allocation counter
        integer :: total_validations = 0                ! Validation counter
        real :: allocation_rate = 0.0                   ! Nodes per second
        integer :: max_depth = 0                        ! Maximum tree depth
        integer :: total_children = 0                   ! Total child relationships
        integer :: active_nodes = 0                     ! Number of active (non-freed) nodes
        integer :: freed_nodes = 0                      ! Number of freed nodes
        real :: fragmentation = 0.0                     ! Memory fragmentation ratio
        
        ! Compatibility fields for old arena API
        integer :: total_nodes = 0                      ! Alias for node_count
        integer :: capacity = 0                         ! Arena capacity 
        integer :: memory_usage = 0                     ! Alias for total_memory
    end type ast_arena_stats_t
    
    ! Result type for node freeing operations
    type :: ast_free_result_t
        logical :: success = .false.                    ! Whether freeing succeeded
        integer :: freed_generation = 0                 ! Generation when node was freed
        character(len=:), allocatable :: error_message  ! Error description if failed
    end type ast_free_result_t
    
contains
    
    ! Create AST arena with specified initial capacity
    function create_ast_arena_core(initial_capacity) result(ast_arena)
        integer, intent(in), optional :: initial_capacity
        type(ast_arena_core_t) :: ast_arena
        integer :: capacity, i
        
        capacity = 1024  ! Default initial capacity
        if (present(initial_capacity)) capacity = initial_capacity
        
        ! Initialize slot storage arrays
        allocate(ast_arena%nodes(capacity))
        allocate(ast_arena%slot_gen(capacity))
        allocate(ast_arena%free_stack(capacity))
        
        ! Initialize state
        ast_arena%cap = capacity
        ast_arena%free_top = 0
        ast_arena%node_count = 0
        ast_arena%epoch = 1
        ast_arena%total_allocations = 0
        ast_arena%total_validations = 0
        ast_arena%arena_id = next_arena_id
        next_arena_id = next_arena_id + 1
        
        ! Initialize base class fields
        ast_arena%generation = 1
        ast_arena%capacity = capacity
        
        ! Initialize all slots as invalid with generation 0 (only used slots get valid generations)
        ast_arena%slot_gen(:) = 0
        
        ! Populate free stack with all initial slots
        do i = 1, capacity
            ast_arena%free_top = ast_arena%free_top + 1
            ast_arena%free_stack(ast_arena%free_top) = i
        end do
        
        ast_arena%is_initialized = .true.
    end function create_ast_arena_core
    
    ! Destroy AST arena and free all memory
    subroutine destroy_ast_arena_core(ast_arena)
        type(ast_arena_core_t), intent(inout) :: ast_arena
        
        if (.not. ast_arena%is_initialized) return
        
        if (allocated(ast_arena%nodes)) deallocate(ast_arena%nodes)
        if (allocated(ast_arena%slot_gen)) deallocate(ast_arena%slot_gen)
        if (allocated(ast_arena%free_stack)) deallocate(ast_arena%free_stack)
        
        ast_arena%cap = 0
        ast_arena%free_top = 0
        ast_arena%node_count = 0
        ast_arena%epoch = 0
        ast_arena%total_allocations = 0
        ast_arena%total_validations = 0
        ast_arena%is_initialized = .false.
    end subroutine destroy_ast_arena_core
    
    ! Store AST node in arena and return handle
    function store_ast_node(ast_arena, node) result(handle)
        type(ast_arena_core_t), intent(inout) :: ast_arena
        type(ast_node_arena_t), intent(in) :: node
        type(ast_handle_t) :: handle
        integer :: slot_id
        
        if (.not. ast_arena%is_initialized) then
            handle = null_ast_handle()
            return
        end if
        
        ! Get available slot
        slot_id = pop_slot(ast_arena)
        if (slot_id <= 0) then
            handle = null_ast_handle()
            return
        end if
        
        ! Store node directly in slot
        ast_arena%nodes(slot_id) = node
        
        ! Set slot generation and create handle
        ! Assign valid generation when slot is used (ensures cross-arena safety)
        if (ast_arena%slot_gen(slot_id) == 0) then
            ast_arena%slot_gen(slot_id) = 1  ! First use gets generation 1
        end if
        handle%node_id = slot_id
        handle%generation = ast_arena%slot_gen(slot_id)
        handle%arena_id = ast_arena%arena_id
        
        ! Update counters
        ast_arena%node_count = ast_arena%node_count + 1
        ast_arena%total_allocations = ast_arena%total_allocations + 1
    end function store_ast_node
    
    ! Retrieve AST node from arena by handle
    function get_ast_node(ast_arena, handle) result(node)
        type(ast_arena_core_t), intent(inout) :: ast_arena
        type(ast_handle_t), intent(in) :: handle
        type(ast_node_arena_t) :: node
        
        ! Initialize default node
        node%node_type_name = "UNKNOWN"
        node%node_kind = 0
        
        ! Validate handle
        if (.not. ast_arena%validate(handle)) then
            return
        end if
        
        ! Return actual stored node
        node = ast_arena%nodes(handle%node_id)
    end function get_ast_node
    
    ! Validate AST handle
    function is_valid_ast_handle(handle) result(is_valid)
        type(ast_handle_t), intent(in) :: handle
        logical :: is_valid
        
        is_valid = handle%node_id > 0 .and. handle%generation > 0
    end function is_valid_ast_handle
    
    ! Create null AST handle
    function null_ast_handle() result(handle)
        type(ast_handle_t) :: handle
        
        handle%node_id = 0
        handle%generation = 0
        handle%arena_id = 0
    end function null_ast_handle
    
    ! Reset AST arena to clean state (epoch management)
    subroutine ast_arena_reset(this)
        class(ast_arena_core_t), intent(inout) :: this
        integer :: i
        
        if (.not. this%is_initialized) return
        
        ! Increment generations to invalidate all existing handles
        this%epoch = this%epoch + 1
        this%generation = this%generation + 1  ! Increment base generation
        
        ! Reset counters
        this%node_count = 0
        this%free_top = 0
        this%size = 0  ! Reset base size
        
        ! Mark all slots as invalid (generation 0)
        this%slot_gen(:) = 0
        
        ! Rebuild free stack with all slots
        do i = 1, this%cap
            this%free_top = this%free_top + 1
            this%free_stack(this%free_top) = i
        end do
    end subroutine ast_arena_reset
    
    ! Get arena statistics
    function ast_arena_get_stats(this) result(stats)
        class(ast_arena_core_t), intent(in) :: this
        type(ast_arena_stats_t) :: stats
        
        stats%node_count = this%node_count
        stats%total_allocations = this%total_allocations
        stats%total_validations = this%total_validations
        
        if (this%is_initialized) then
            stats%total_memory = int(storage_size(this%nodes(1)) * this%cap, int64) / 8
            if (this%cap > 0) then
                stats%utilization = real(this%node_count) / real(this%cap)
            else
                stats%utilization = 0.0
            end if
        end if
        
        ! Calculate allocation rate (efficiency metric)
        if (this%total_allocations > 0) then
            stats%allocation_rate = real(this%node_count) / real(this%total_allocations)
        end if
        
        stats%max_depth = 0  ! Not tracked in core arena
        stats%total_children = 0  ! Would be sum of all child_count fields
        
        ! Populate compatibility fields for old arena API
        stats%total_nodes = this%node_count
        stats%capacity = this%cap
        stats%memory_usage = int(stats%total_memory)
        stats%active_nodes = this%node_count
        stats%freed_nodes = this%total_allocations - this%node_count
    end function ast_arena_get_stats
    
    ! Validate AST handle against arena (per-slot generation checking)
    function ast_arena_validate_handle(this, handle) result(is_valid)
        class(ast_arena_core_t), intent(inout) :: this
        type(ast_handle_t), intent(in) :: handle
        logical :: is_valid
        
        this%total_validations = this%total_validations + 1
        
        if (.not. this%is_initialized) then
            is_valid = .false.
            return
        end if
        
        ! Check handle validity
        if (.not. is_valid_ast_handle(handle)) then
            is_valid = .false.
            return
        end if
        
        ! Check node ID range
        if (handle%node_id < 1 .or. handle%node_id > this%cap) then
            is_valid = .false.
            return
        end if
        
        ! Check arena ownership (cross-arena safety)
        if (handle%arena_id /= this%arena_id) then
            is_valid = .false.
            return
        end if
        
        ! Check per-slot generation match (ABA-safe)
        if (this%slot_gen(handle%node_id) /= handle%generation) then
            is_valid = .false.
            return
        end if
        
        is_valid = .true.
    end function ast_arena_validate_handle
    
    ! Get current node count
    function ast_arena_get_node_count(this) result(count)
        class(ast_arena_core_t), intent(in) :: this
        integer :: count
        
        count = this%node_count
    end function ast_arena_get_node_count
    
    ! Get arena ID
    function ast_arena_get_arena_id(this) result(arena_id)
        class(ast_arena_core_t), intent(in) :: this
        integer :: arena_id
        
        arena_id = this%arena_id
    end function ast_arena_get_arena_id
    
    ! Free individual AST node with generation tracking
    function ast_arena_free_node(this, handle) result(free_result)
        class(ast_arena_core_t), intent(inout) :: this
        type(ast_handle_t), intent(in) :: handle
        type(ast_free_result_t) :: free_result
        
        ! Initialize result
        free_result%success = .false.
        free_result%freed_generation = 0
        
        ! Validate arena is initialized
        if (.not. this%is_initialized) then
            allocate(character(40) :: free_result%error_message)
            free_result%error_message = "Arena not initialized"
            return
        end if
        
        ! Validate handle
        if (.not. this%validate(handle)) then
            allocate(character(25) :: free_result%error_message)
            free_result%error_message = "Invalid handle for freeing"
            return
        end if
        
        ! Free the node by incrementing its slot generation (invalidates old handles)
        this%slot_gen(handle%node_id) = this%slot_gen(handle%node_id) + 1  ! Invalidate old handles
        free_result%freed_generation = handle%generation
        
        ! Add slot to free list for reuse
        call push_free(this, handle%node_id)
        
        ! Update counters
        this%node_count = this%node_count - 1
        
        free_result%success = .true.
    end function ast_arena_free_node
    
    ! Check if a node handle is currently active (not freed)
    function ast_arena_is_node_active(this, handle) result(is_active)
        class(ast_arena_core_t), intent(inout) :: this
        type(ast_handle_t), intent(in) :: handle
        logical :: is_active
        
        ! A node is active if its handle is valid
        is_active = this%validate(handle)
    end function ast_arena_is_node_active
    
    ! Get statistics about freed vs active nodes
    function ast_arena_get_free_stats(this) result(stats)
        class(ast_arena_core_t), intent(in) :: this
        type(ast_arena_stats_t) :: stats
        
        ! Get base statistics
        stats = this%get_stats()
        
        ! Add freeing-specific statistics
        stats%active_nodes = this%node_count
        stats%freed_nodes = (this%total_allocations - this%node_count)
        
        ! Calculate fragmentation (ratio of free slots to total capacity)
        if (this%cap > 0) then
            stats%fragmentation = real(this%free_top) / real(this%cap)
        else
            stats%fragmentation = 0.0
        end if
    end function ast_arena_get_free_stats
    
    ! Public interface: Free AST node from arena
    function free_ast_node(ast_arena, handle) result(free_result)
        type(ast_arena_core_t), intent(inout) :: ast_arena
        type(ast_handle_t), intent(in) :: handle
        type(ast_free_result_t) :: free_result
        
        free_result = ast_arena%free_node(handle)
    end function free_ast_node
    
    ! Public interface: Check if node is active
    function is_node_active(ast_arena, handle) result(is_active)
        type(ast_arena_core_t), intent(inout) :: ast_arena
        type(ast_handle_t), intent(in) :: handle
        logical :: is_active
        
        is_active = ast_arena%is_active(handle)
    end function is_node_active
    
    ! Public interface: Get freeing statistics
    function get_free_statistics(ast_arena) result(stats)
        type(ast_arena_core_t), intent(in) :: ast_arena
        type(ast_arena_stats_t) :: stats
        
        stats = ast_arena%get_free_stats()
    end function get_free_statistics
    
    ! Pop slot from freelist or allocate new one
    function pop_slot(arena) result(slot_id)
        type(ast_arena_core_t), intent(inout) :: arena
        integer :: slot_id
        
        if (arena%free_top > 0) then
            ! Pop from freelist
            slot_id = arena%free_stack(arena%free_top)
            arena%free_top = arena%free_top - 1
        else
            ! Need to grow arena
            call grow(arena)
            if (arena%free_top > 0) then
                ! Pop directly after grow
                slot_id = arena%free_stack(arena%free_top)
                arena%free_top = arena%free_top - 1
            else
                slot_id = 0  ! Failed to grow
            end if
        end if
    end function pop_slot
    
    ! Push slot to freelist (not used in current reset strategy)
    subroutine push_free(arena, slot_id)
        type(ast_arena_core_t), intent(inout) :: arena
        integer, intent(in) :: slot_id
        
        if (arena%free_top < arena%cap) then
            arena%free_top = arena%free_top + 1
            arena%free_stack(arena%free_top) = slot_id
        end if
    end subroutine push_free
    
    ! Grow arena capacity and initialize new slots
    subroutine grow(arena)
        type(ast_arena_core_t), intent(inout) :: arena
        type(ast_node_arena_t), allocatable :: new_nodes(:)
        integer, allocatable :: new_slot_gen(:), new_free_stack(:)
        integer :: new_cap, i, old_cap
        
        old_cap = arena%cap
        new_cap = max(old_cap * 2, 16)  ! Minimum growth
        
        ! Allocate new arrays
        allocate(new_nodes(new_cap))
        allocate(new_slot_gen(new_cap))
        allocate(new_free_stack(new_cap))
        
        ! Copy existing data
        if (old_cap > 0) then
            new_nodes(1:old_cap) = arena%nodes(1:old_cap)
            new_slot_gen(1:old_cap) = arena%slot_gen(1:old_cap)
            new_free_stack(1:arena%free_top) = arena%free_stack(1:arena%free_top)
        end if
        
        ! Initialize new slots as free with generation 1
        new_slot_gen(old_cap+1:new_cap) = 1
        
        ! Build freelist for new slots
        do i = old_cap + 1, new_cap
            arena%free_top = arena%free_top + 1
            new_free_stack(arena%free_top) = i
        end do
        
        ! Move to arena
        call move_alloc(new_nodes, arena%nodes)
        call move_alloc(new_slot_gen, arena%slot_gen)
        call move_alloc(new_free_stack, arena%free_stack)
        arena%cap = new_cap
    end subroutine grow
    
    ! Deep copy assignment operator to prevent double free
    subroutine ast_arena_assign(lhs, rhs)
        class(ast_arena_core_t), intent(out) :: lhs
        type(ast_arena_core_t), intent(in) :: rhs
        
        ! Copy scalar members
        lhs%cap = rhs%cap
        lhs%free_top = rhs%free_top
        lhs%node_count = rhs%node_count
        lhs%epoch = rhs%epoch
        lhs%total_allocations = rhs%total_allocations
        lhs%total_validations = rhs%total_validations
        lhs%arena_id = rhs%arena_id
        lhs%is_initialized = rhs%is_initialized
        
        ! Deep copy slot arrays
        if (allocated(rhs%nodes)) then
            allocate(lhs%nodes(size(rhs%nodes)))
            lhs%nodes = rhs%nodes
        end if
        
        if (allocated(rhs%slot_gen)) then
            allocate(lhs%slot_gen(size(rhs%slot_gen)))
            lhs%slot_gen = rhs%slot_gen
        end if
        
        if (allocated(rhs%free_stack)) then
            allocate(lhs%free_stack(size(rhs%free_stack)))
            lhs%free_stack = rhs%free_stack
        end if
    end subroutine ast_arena_assign

    ! ============================================================================
    ! Private Field Accessors for Base Interface Module
    ! ============================================================================
    
    function ast_arena_get_cap(this) result(cap)
        class(ast_arena_core_t), intent(in) :: this
        integer :: cap
        cap = this%cap
    end function ast_arena_get_cap
    
    function ast_arena_get_free_top(this) result(free_top)
        class(ast_arena_core_t), intent(in) :: this
        integer :: free_top
        free_top = this%free_top
    end function ast_arena_get_free_top
    
    subroutine ast_arena_set_free_top(this, free_top)
        class(ast_arena_core_t), intent(inout) :: this
        integer, intent(in) :: free_top
        this%free_top = free_top
    end subroutine ast_arena_set_free_top
    
    function ast_arena_get_slot_gen(this, index) result(gen)
        class(ast_arena_core_t), intent(in) :: this
        integer, intent(in) :: index
        integer :: gen
        if (index >= 1 .and. index <= this%cap) then
            gen = this%slot_gen(index)
        else
            gen = 0
        end if
    end function ast_arena_get_slot_gen
    
    subroutine ast_arena_set_slot_gen(this, index, gen)
        class(ast_arena_core_t), intent(inout) :: this
        integer, intent(in) :: index, gen
        if (index >= 1 .and. index <= this%cap) then
            this%slot_gen(index) = gen
        end if
    end subroutine ast_arena_set_slot_gen
    
    function ast_arena_get_free_stack(this, index) result(value)
        class(ast_arena_core_t), intent(in) :: this
        integer, intent(in) :: index
        integer :: value
        if (index >= 1 .and. index <= this%cap) then
            value = this%free_stack(index)
        else
            value = 0
        end if
    end function ast_arena_get_free_stack
    
    subroutine ast_arena_set_free_stack(this, index, value)
        class(ast_arena_core_t), intent(inout) :: this
        integer, intent(in) :: index, value
        if (index >= 1 .and. index <= this%cap) then
            this%free_stack(index) = value
        end if
    end subroutine ast_arena_set_free_stack
    
    subroutine ast_arena_increment_node_count(this)
        class(ast_arena_core_t), intent(inout) :: this
        this%node_count = this%node_count + 1
    end subroutine ast_arena_increment_node_count
    
    subroutine ast_arena_decrement_node_count(this)
        class(ast_arena_core_t), intent(inout) :: this
        this%node_count = this%node_count - 1
    end subroutine ast_arena_decrement_node_count

    ! ============================================================================
    ! Base Arena Interface Implementations for AST Arena (Issue #369) 
    ! See ast_arena_base_interface.f90 for actual implementations
    ! ============================================================================
    
    ! Insert item into AST arena (base interface)
    function ast_arena_insert(this, item) result(handle)
        class(ast_arena_core_t), intent(inout) :: this
        class(*), intent(in) :: item
        type(arena_handle_t) :: handle
        type(ast_handle_t) :: ast_handle
        
        ! Ensure item is ast_node_arena_t
        select type(item)
        type is (ast_node_arena_t)
            ! Store using the public store_ast_node function
            ast_handle = store_ast_node(this, item)
            
            ! Convert ast_handle_t to arena_handle_t
            handle%chunk_id = 1  ! AST arena uses single logical chunk
            handle%offset = ast_handle%node_id
            ! Use base generation for container API handles
            handle%generation = this%generation
            handle%size = 1
            
            ! Update base class size
            this%size = this%node_count
        class default
            ! Return invalid handle for wrong type
            handle%chunk_id = 0
            handle%offset = 0
            handle%generation = 0
            handle%size = 0
        end select
    end function ast_arena_insert
    
    ! Get item from AST arena (base interface)
    function ast_arena_get(this, handle) result(item)
        class(ast_arena_core_t), intent(in) :: this
        type(arena_handle_t), intent(in) :: handle
        class(*), pointer :: item
        
        ! AST arena doesn't support polymorphic pointer return
        ! This is a limitation of the current design
        ! Users should use get_ast_node for actual retrieval
        item => null()
    end function ast_arena_get
    
    ! Validate handle in AST arena (base interface)
    function ast_arena_valid(this, handle) result(is_valid)
        class(ast_arena_core_t), intent(in) :: this
        type(arena_handle_t), intent(in) :: handle
        logical :: is_valid
        
        is_valid = .false.
        
        ! Basic validation without modifying state
        if (handle%generation == 0) return
        if (handle%offset < 1 .or. handle%offset > this%cap) return
        
        ! Check generation - handle must match current arena generation
        if (handle%generation /= this%generation) return
        
        ! Check if slot is allocated (non-zero generation means allocated)
        if (this%slot_gen(handle%offset) > 0) then
            is_valid = .true.
        end if
    end function ast_arena_valid
    
    ! Free item in AST arena (base interface)
    subroutine ast_arena_free(this, handle)
        class(ast_arena_core_t), intent(inout) :: this
        type(arena_handle_t), intent(in) :: handle
        
        ! Validate handle first
        if (.not. this%valid(handle)) return
        
        ! Mark slot as free by setting generation to 0
        this%slot_gen(handle%offset) = 0
        
        ! Add slot to free list for reuse
        if (this%free_top < this%cap) then
            this%free_top = this%free_top + 1
            this%free_stack(this%free_top) = handle%offset
        end if
        
        ! Update counters
        this%node_count = this%node_count - 1
        this%size = this%node_count  ! Keep base size in sync
    end subroutine ast_arena_free
    
    ! Create checkpoint for AST arena
    function ast_arena_checkpoint(this) result(checkpoint)
        class(ast_arena_core_t), intent(in) :: this
        type(arena_checkpoint_t) :: checkpoint
        
        checkpoint%generation = this%generation
        checkpoint%size = this%node_count  ! Use actual node count
        checkpoint%capacity = this%cap
        checkpoint%chunk_count = 1  ! AST arena uses logical single chunk
        checkpoint%current_chunk = 1
        checkpoint%total_allocated = this%node_count
    end function ast_arena_checkpoint
    
    ! Rollback AST arena to checkpoint
    subroutine ast_arena_rollback(this, checkpoint)
        class(ast_arena_core_t), intent(inout) :: this
        type(arena_checkpoint_t), intent(in) :: checkpoint
        integer :: i
        
        ! Increment global generation to invalidate post-checkpoint handles
        this%generation = this%generation + 1
        
        ! Reset free stack to state at checkpoint
        this%free_top = 0
        
        ! Rebuild free stack with slots allocated after checkpoint
        do i = checkpoint%total_allocated + 1, this%node_count
            if (i <= this%cap) then
                ! Increment slot generation to invalidate old handles
                this%slot_gen(i) = this%slot_gen(i) + 1
                ! Add to free stack for reuse
                this%free_top = this%free_top + 1
                this%free_stack(this%free_top) = i
            end if
        end do
        
        ! Restore counts
        this%size = checkpoint%size
        this%node_count = checkpoint%total_allocated
    end subroutine ast_arena_rollback
    
end module ast_arena_core