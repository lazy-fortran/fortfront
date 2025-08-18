module ast_node_registry
    use ast_base, only: ast_node
    implicit none
    private

    ! Node registry for distributed node management
    type :: node_entry_t
        class(ast_node), allocatable :: node
        integer :: ref_count = 0        ! Reference counting for memory management
        logical :: is_root = .false.    ! Track root nodes for GC
        integer :: generation = 0       ! For generational GC
    contains
        procedure :: increment_ref
        procedure :: decrement_ref
        procedure :: is_owned
        final :: cleanup_entry
    end type node_entry_t

    ! Hierarchical node registry with distributed allocation
    type, public :: ast_node_registry_t
        type(node_entry_t), allocatable :: nodes(:)
        integer :: size = 0
        integer :: capacity = 0
        integer :: generation_counter = 0
        integer, allocatable :: free_list(:)    ! Free slot management
        integer :: free_count = 0
        integer :: initial_capacity = 64       ! Smaller initial size
    contains
        procedure :: register_node
        procedure :: get_node
        procedure :: release_node
        procedure :: add_reference
        procedure :: remove_reference
        procedure :: collect_garbage
        procedure :: get_stats
        procedure :: clear_registry
        procedure :: grow_registry
        procedure :: find_free_slot
        final :: cleanup_registry
    end type ast_node_registry_t

    ! Statistics for monitoring
    type, public :: registry_stats_t
        integer :: total_nodes = 0
        integer :: active_nodes = 0
        integer :: free_slots = 0
        integer :: memory_usage = 0
        integer :: ref_count_total = 0
        real :: fragmentation_ratio = 0.0
    end type registry_stats_t

    public :: create_node_registry

contains

    ! Create new node registry with distributed design
    function create_node_registry(initial_capacity) result(registry)
        integer, intent(in), optional :: initial_capacity
        type(ast_node_registry_t) :: registry
        integer :: cap

        cap = 64  ! Smaller default capacity
        if (present(initial_capacity)) then
            cap = max(initial_capacity, 16)
        end if

        registry%capacity = cap
        registry%initial_capacity = cap
        allocate(registry%nodes(cap))
        allocate(registry%free_list(cap))
        registry%size = 0
        registry%free_count = 0
        registry%generation_counter = 0
    end function create_node_registry

    ! Register a new node with reference counting
    function register_node(this, node, is_root) result(node_id)
        class(ast_node_registry_t), intent(inout) :: this
        class(ast_node), intent(in) :: node
        logical, intent(in), optional :: is_root
        integer :: node_id
        integer :: slot

        ! Find available slot
        slot = this%find_free_slot()
        if (slot == 0) then
            call this%grow_registry()
            slot = this%find_free_slot()
        end if

        ! Register node with reference counting
        if (allocated(this%nodes(slot)%node)) then
            deallocate(this%nodes(slot)%node)
        end if
        allocate(this%nodes(slot)%node, source=node)
        this%nodes(slot)%ref_count = 1  ! Start with one reference
        this%nodes(slot)%generation = this%generation_counter
        
        if (present(is_root)) then
            this%nodes(slot)%is_root = is_root
        else
            this%nodes(slot)%is_root = .false.
        end if

        this%size = this%size + 1
        node_id = slot
    end function register_node

    ! Get node with bounds checking and reference tracking
    function get_node(this, node_id) result(node)
        class(ast_node_registry_t), intent(in) :: this
        integer, intent(in) :: node_id
        class(ast_node), allocatable :: node

        ! Bounds checking
        if (node_id > 0 .and. node_id <= this%capacity) then
            if (allocated(this%nodes(node_id)%node)) then
                allocate(node, source=this%nodes(node_id)%node)
            end if
        end if
    end function get_node

    ! Release node reference and potentially deallocate
    subroutine release_node(this, node_id)
        class(ast_node_registry_t), intent(inout) :: this
        integer, intent(in) :: node_id

        if (node_id > 0 .and. node_id <= this%capacity) then
            if (allocated(this%nodes(node_id)%node)) then
                call this%nodes(node_id)%decrement_ref()
                
                ! If reference count reaches zero, mark as free
                if (this%nodes(node_id)%ref_count <= 0) then
                    deallocate(this%nodes(node_id)%node)
                    this%nodes(node_id)%is_root = .false.
                    
                    ! Add to free list
                    if (this%free_count < size(this%free_list)) then
                        this%free_count = this%free_count + 1
                        this%free_list(this%free_count) = node_id
                    end if
                    
                    this%size = this%size - 1
                end if
            end if
        end if
    end subroutine release_node

    ! Add reference to existing node
    subroutine add_reference(this, node_id)
        class(ast_node_registry_t), intent(inout) :: this
        integer, intent(in) :: node_id

        if (node_id > 0 .and. node_id <= this%capacity) then
            if (allocated(this%nodes(node_id)%node)) then
                call this%nodes(node_id)%increment_ref()
            end if
        end if
    end subroutine add_reference

    ! Remove reference from node
    subroutine remove_reference(this, node_id)
        class(ast_node_registry_t), intent(inout) :: this
        integer, intent(in) :: node_id

        call this%release_node(node_id)
    end subroutine remove_reference

    ! Collect garbage using generational approach
    subroutine collect_garbage(this)
        class(ast_node_registry_t), intent(inout) :: this
        integer :: i, collected = 0

        ! Increment generation for new collection cycle
        this%generation_counter = this%generation_counter + 1

        ! Mark-and-sweep garbage collection
        do i = 1, this%capacity
            if (allocated(this%nodes(i)%node)) then
                ! Collect nodes with zero references and not root
                if (this%nodes(i)%ref_count <= 0 .and. .not. this%nodes(i)%is_root) then
                    deallocate(this%nodes(i)%node)
                    
                    ! Add to free list
                    if (this%free_count < size(this%free_list)) then
                        this%free_count = this%free_count + 1
                        this%free_list(this%free_count) = i
                    end if
                    
                    collected = collected + 1
                end if
            end if
        end do

        this%size = this%size - collected
    end subroutine collect_garbage

    ! Get registry statistics
    function get_stats(this) result(stats)
        class(ast_node_registry_t), intent(in) :: this
        type(registry_stats_t) :: stats
        integer :: i, active_count = 0, ref_total = 0

        ! Count active nodes and references
        do i = 1, this%capacity
            if (allocated(this%nodes(i)%node)) then
                active_count = active_count + 1
                ref_total = ref_total + this%nodes(i)%ref_count
            end if
        end do

        stats%total_nodes = this%size
        stats%active_nodes = active_count
        stats%free_slots = this%free_count
        stats%memory_usage = this%capacity * 64  ! Rough estimate
        stats%ref_count_total = ref_total
        
        if (this%capacity > 0) then
            stats%fragmentation_ratio = real(this%free_count) / real(this%capacity)
        else
            stats%fragmentation_ratio = 0.0
        end if
    end function get_stats

    ! Clear entire registry
    subroutine clear_registry(this)
        class(ast_node_registry_t), intent(inout) :: this
        integer :: i

        ! Deallocate all nodes
        do i = 1, this%capacity
            if (allocated(this%nodes(i)%node)) then
                deallocate(this%nodes(i)%node)
            end if
            this%nodes(i)%ref_count = 0
            this%nodes(i)%is_root = .false.
        end do

        this%size = 0
        this%free_count = 0
        this%generation_counter = 0
    end subroutine clear_registry

    ! Grow registry capacity when needed
    subroutine grow_registry(this)
        class(ast_node_registry_t), intent(inout) :: this
        type(node_entry_t), allocatable :: temp_nodes(:)
        integer, allocatable :: temp_free_list(:)
        integer :: new_capacity, i

        ! Grow by 50% or at least 32 slots
        new_capacity = max(this%capacity + this%capacity/2, this%capacity + 32)

        ! Move existing nodes
        allocate(temp_nodes(new_capacity))
        do i = 1, this%capacity
            if (allocated(this%nodes(i)%node)) then
                call move_alloc(this%nodes(i)%node, temp_nodes(i)%node)
                temp_nodes(i)%ref_count = this%nodes(i)%ref_count
                temp_nodes(i)%is_root = this%nodes(i)%is_root
                temp_nodes(i)%generation = this%nodes(i)%generation
            end if
        end do

        ! Move free list
        allocate(temp_free_list(new_capacity))
        if (this%free_count > 0) then
            temp_free_list(1:this%free_count) = this%free_list(1:this%free_count)
        end if

        ! Replace arrays
        call move_alloc(temp_nodes, this%nodes)
        call move_alloc(temp_free_list, this%free_list)
        this%capacity = new_capacity
    end subroutine grow_registry

    ! Find next available slot
    function find_free_slot(this) result(slot)
        class(ast_node_registry_t), intent(inout) :: this
        integer :: slot
        integer :: i

        slot = 0

        ! Check free list first
        if (this%free_count > 0) then
            slot = this%free_list(this%free_count)
            this%free_count = this%free_count - 1
            return
        end if

        ! Find first unallocated slot
        do i = 1, this%capacity
            if (.not. allocated(this%nodes(i)%node)) then
                slot = i
                return
            end if
        end do

        ! No free slot found
        slot = 0
    end function find_free_slot

    ! Node entry reference counting procedures
    subroutine increment_ref(this)
        class(node_entry_t), intent(inout) :: this
        this%ref_count = this%ref_count + 1
    end subroutine increment_ref

    subroutine decrement_ref(this)
        class(node_entry_t), intent(inout) :: this
        this%ref_count = max(0, this%ref_count - 1)
    end subroutine decrement_ref

    function is_owned(this) result(owned)
        class(node_entry_t), intent(in) :: this
        logical :: owned
        owned = this%ref_count > 0 .or. this%is_root
    end function is_owned

    ! Cleanup procedures
    subroutine cleanup_entry(this)
        type(node_entry_t), intent(inout) :: this
        if (allocated(this%node)) then
            deallocate(this%node)
        end if
    end subroutine cleanup_entry

    subroutine cleanup_registry(this)
        type(ast_node_registry_t), intent(inout) :: this
        call this%clear_registry()
        if (allocated(this%nodes)) then
            deallocate(this%nodes)
        end if
        if (allocated(this%free_list)) then
            deallocate(this%free_list)
        end if
    end subroutine cleanup_registry

end module ast_node_registry