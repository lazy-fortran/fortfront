module cst_arena
    use, intrinsic :: iso_fortran_env, only: int64
    use cst_nodes, only: cst_node_t
    implicit none
    private

    public :: cst_arena_t, cst_handle_t
    public :: create_cst_arena

    ! CST arena handle for safe node access
    type :: cst_handle_t
        integer :: index           ! Node index in arena
        integer(int64) :: generation ! Generation for safety
    end type cst_handle_t

    ! CST-specific arena implementation
    type :: cst_arena_t
        type(cst_node_t), allocatable :: nodes(:)    ! CST nodes storage
        integer(int64), allocatable :: generations(:) ! Generation tracking
        integer :: size                               ! Current arena size
        integer :: capacity                           ! Arena capacity
        integer(int64) :: next_uid                    ! Next UID to assign
        integer(int64) :: global_generation           ! Global generation counter
    contains
        procedure :: push => push_cst_node
        procedure :: get => get_cst_node
        procedure :: is_valid_handle => is_valid_cst_handle
        procedure :: clear => clear_cst_arena
        procedure :: resize => resize_cst_arena
    end type cst_arena_t

contains

    ! Create new CST arena
    function create_cst_arena(initial_capacity) result(arena)
        integer, intent(in), optional :: initial_capacity
        type(cst_arena_t) :: arena
        
        integer :: capacity
        
        capacity = 1024  ! Default capacity
        if (present(initial_capacity)) capacity = initial_capacity
        
        arena%size = 0
        arena%capacity = capacity
        arena%next_uid = 1_int64
        arena%global_generation = 1_int64
        
        allocate(arena%nodes(capacity))
        allocate(arena%generations(capacity))
        arena%generations(:) = 0_int64
    end function create_cst_arena

    ! Push new CST node to arena
    function push_cst_node(this, node) result(handle)
        class(cst_arena_t), intent(inout) :: this
        type(cst_node_t), intent(in) :: node
        type(cst_handle_t) :: handle
        
        ! Resize if necessary
        if (this%size >= this%capacity) then
            call this%resize(this%capacity * 2)
        end if
        
        ! Add node to arena
        this%size = this%size + 1
        this%nodes(this%size) = node
        this%nodes(this%size)%uid = this%next_uid
        this%generations(this%size) = this%global_generation
        
        ! Create handle
        handle%index = this%size
        handle%generation = this%global_generation
        
        ! Update counters
        this%next_uid = this%next_uid + 1_int64
        this%global_generation = this%global_generation + 1_int64
    end function push_cst_node

    ! Get CST node by handle
    function get_cst_node(this, handle) result(node)
        class(cst_arena_t), intent(in) :: this
        type(cst_handle_t), intent(in) :: handle
        type(cst_node_t) :: node
        
        ! Initialize with invalid node
        node%kind = -1
        node%uid = 0_int64
        node%start_pos = -1
        node%end_pos = -1
        
        ! Validate handle
        if (.not. this%is_valid_handle(handle)) return
        
        ! Return copy of node
        node = this%nodes(handle%index)
    end function get_cst_node

    ! Check if handle is valid
    function is_valid_cst_handle(this, handle) result(is_valid)
        class(cst_arena_t), intent(in) :: this
        type(cst_handle_t), intent(in) :: handle
        logical :: is_valid
        
        is_valid = .false.
        
        ! Check bounds
        if (handle%index < 1 .or. handle%index > this%size) return
        
        ! Check generation
        if (handle%generation /= this%generations(handle%index)) return
        
        is_valid = .true.
    end function is_valid_cst_handle

    ! Clear arena contents
    subroutine clear_cst_arena(this)
        class(cst_arena_t), intent(inout) :: this
        
        this%size = 0
        this%next_uid = 1_int64
        this%global_generation = this%global_generation + 1_int64
        this%generations(:) = 0_int64
    end subroutine clear_cst_arena

    ! Resize arena capacity
    subroutine resize_cst_arena(this, new_capacity)
        class(cst_arena_t), intent(inout) :: this
        integer, intent(in) :: new_capacity
        
        type(cst_node_t), allocatable :: temp_nodes(:)
        integer(int64), allocatable :: temp_generations(:)
        
        ! Skip if new capacity is not larger
        if (new_capacity <= this%capacity) return
        
        ! Allocate temporary storage
        allocate(temp_nodes(new_capacity))
        allocate(temp_generations(new_capacity))
        
        ! Copy existing data
        if (this%size > 0) then
            temp_nodes(1:this%size) = this%nodes(1:this%size)
            temp_generations(1:this%size) = this%generations(1:this%size)
        end if
        
        ! Initialize new slots
        temp_generations(this%size+1:new_capacity) = 0_int64
        
        ! Replace arrays
        call move_alloc(temp_nodes, this%nodes)
        call move_alloc(temp_generations, this%generations)
        
        this%capacity = new_capacity
    end subroutine resize_cst_arena

end module cst_arena
