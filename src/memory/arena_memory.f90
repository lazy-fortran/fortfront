module arena_memory
    implicit none
    private

    ! Core arena memory allocator with generation-based safety
    ! Provides O(1) allocation and bulk deallocation for 10-100x performance
    ! 
    ! Key features:
    ! - Simple pointer increment allocation (O(1))
    ! - Generation tracking for use-after-free prevention
    ! - Bulk deallocation in O(1) time
    ! - Cache-optimal sequential memory layout
    ! - Automatic growth when needed

    ! Default configuration
    integer, parameter :: DEFAULT_CHUNK_SIZE = 65536    ! 64KB chunks
    integer, parameter :: MIN_CHUNK_SIZE = 4096         ! 4KB minimum
    integer, parameter :: MAX_CHUNK_SIZE = 1048576      ! 1MB maximum
    integer, parameter :: DEFAULT_ALIGNMENT = 8         ! 8-byte alignment

    ! Handle for memory allocation with generation validation
    type, public :: arena_handle_t
        integer :: offset = 0         ! Byte offset within chunk
        integer :: size = 0           ! Size of allocation
        integer :: generation = 0     ! Generation for validation
        integer :: chunk_id = 0       ! Which chunk contains data
    end type arena_handle_t

    ! Memory chunk for arena storage
    type :: arena_chunk_t
        integer(1), allocatable :: data(:)    ! Raw byte storage
        integer :: capacity = 0               ! Chunk capacity in bytes
        integer :: used = 0                   ! Bytes currently used
        integer :: generation = 1             ! Current generation
    contains
        procedure :: assign_chunk => arena_chunk_assign
        generic :: assignment(=) => assign_chunk
    end type arena_chunk_t

    ! Core arena allocator
    type, public :: arena_t
        type(arena_chunk_t), allocatable :: chunks(:)  ! Memory chunks
        integer :: chunk_count = 0                     ! Number of chunks
        integer :: current_chunk = 1                   ! Active chunk index
        integer :: total_allocated = 0                 ! Total bytes allocated
        integer :: total_capacity = 0                  ! Total capacity
        integer :: chunk_size = DEFAULT_CHUNK_SIZE     ! Size for new chunks
        integer :: alignment = DEFAULT_ALIGNMENT       ! Memory alignment
        integer :: generation = 1                      ! Global generation
    contains
        procedure :: allocate => arena_allocate
        procedure :: validate => arena_validate
        procedure :: reset => arena_reset
        procedure :: clear => arena_clear
        procedure :: get_stats => arena_get_stats
        procedure :: grow => arena_grow
        procedure :: get_data => arena_get_data
        procedure :: set_data => arena_set_data
        procedure :: assign_arena => arena_assign
        generic :: assignment(=) => assign_arena
    end type arena_t

    ! Arena statistics for monitoring
    type, public :: arena_stats_t
        integer :: total_allocated = 0      ! Total bytes allocated
        integer :: total_capacity = 0       ! Total capacity in bytes
        integer :: chunk_count = 0          ! Number of chunks
        integer :: current_generation = 0   ! Current generation number
        real :: utilization = 0.0           ! Memory utilization (0-1)
    end type arena_stats_t

    ! Public interface
    public :: create_arena, destroy_arena
    public :: is_valid_handle, null_handle
    
    ! Example usage:
    !   type(arena_t) :: arena
    !   type(arena_handle_t) :: handle
    !   integer(1) :: buffer(100)
    !   logical :: status
    !   
    !   arena = create_arena(chunk_size=8192)
    !   handle = arena%allocate(100)
    !   buffer = 42
    !   call arena%set_data(handle, buffer, status)
    !   call arena%get_data(handle, buffer, status)
    !   call destroy_arena(arena)

contains

    ! Create a new arena with specified chunk size
    function create_arena(chunk_size) result(arena)
        integer, intent(in), optional :: chunk_size
        type(arena_t) :: arena
        integer :: size

        ! Set chunk size with bounds checking
        if (present(chunk_size)) then
            size = max(MIN_CHUNK_SIZE, min(chunk_size, MAX_CHUNK_SIZE))
        else
            size = DEFAULT_CHUNK_SIZE
        end if
        arena%chunk_size = size

        ! Initialize with one chunk
        arena%chunk_count = 1
        arena%current_chunk = 1
        allocate(arena%chunks(8))  ! Initial capacity for chunk array
        
        ! Allocate first chunk
        arena%chunks(1)%capacity = size
        allocate(arena%chunks(1)%data(size))
        arena%chunks(1)%data = 0  ! Zero-initialize
        arena%chunks(1)%used = 0
        arena%chunks(1)%generation = 1
        
        arena%total_capacity = size
        arena%total_allocated = 0
        arena%generation = 1
    end function create_arena

    ! Destroy arena and free all memory
    subroutine destroy_arena(arena)
        type(arena_t), intent(inout) :: arena
        integer :: i

        if (allocated(arena%chunks)) then
            do i = 1, arena%chunk_count
                if (allocated(arena%chunks(i)%data)) then
                    deallocate(arena%chunks(i)%data)
                end if
            end do
            deallocate(arena%chunks)
        end if
        
        arena%chunk_count = 0
        arena%current_chunk = 1
        arena%total_allocated = 0
        arena%total_capacity = 0
        arena%generation = arena%generation + 1  ! Invalidate all handles
    end subroutine destroy_arena

    ! Allocate memory from arena (O(1) operation)
    function arena_allocate(this, size) result(handle)
        class(arena_t), intent(inout) :: this
        integer, intent(in) :: size
        type(arena_handle_t) :: handle
        integer :: aligned_size, required_size
        integer :: chunk_idx

        ! Validate size
        if (size <= 0) then
            handle = null_handle()
            return
        end if

        ! Calculate aligned size
        aligned_size = align_size(size, this%alignment)
        
        ! Find chunk with space or grow
        chunk_idx = this%current_chunk
        do while (chunk_idx <= this%chunk_count)
            if (this%chunks(chunk_idx)%used + aligned_size &
                <= this%chunks(chunk_idx)%capacity) then
                exit  ! Found space
            end if
            chunk_idx = chunk_idx + 1
        end do
        
        ! Need new chunk?
        if (chunk_idx > this%chunk_count) then
            call this%grow(max(aligned_size, this%chunk_size))
            chunk_idx = this%chunk_count  ! Use the newly created chunk
        end if
        
        ! Allocate from chunk
        handle%offset = this%chunks(chunk_idx)%used
        handle%size = size
        handle%generation = this%generation
        handle%chunk_id = chunk_idx
        
        ! Update chunk state
        this%chunks(chunk_idx)%used = &
            this%chunks(chunk_idx)%used + aligned_size
        this%total_allocated = this%total_allocated + aligned_size
        this%current_chunk = chunk_idx
    end function arena_allocate

    ! Validate handle is still valid
    function arena_validate(this, handle) result(valid)
        class(arena_t), intent(in) :: this
        type(arena_handle_t), intent(in) :: handle
        logical :: valid

        valid = .false.
        
        ! Check generation
        if (handle%generation /= this%generation) return
        
        ! Check chunk bounds
        if (handle%chunk_id < 1 .or. &
            handle%chunk_id > this%chunk_count) return
        
        ! Check offset bounds
        if (handle%offset < 0 .or. &
            handle%offset + handle%size > &
            this%chunks(handle%chunk_id)%used) return
        
        valid = .true.
    end function arena_validate

    ! Reset arena for reuse (O(1) operation)
    subroutine arena_reset(this)
        class(arena_t), intent(inout) :: this
        integer :: i

        ! Reset all chunks
        do i = 1, this%chunk_count
            this%chunks(i)%used = 0
            this%chunks(i)%generation = &
                this%chunks(i)%generation + 1
        end do
        
        ! Reset arena state
        this%current_chunk = 1
        this%total_allocated = 0
        this%generation = this%generation + 1  ! Invalidate all handles
    end subroutine arena_reset

    ! Clear arena and free excess memory
    subroutine arena_clear(this)
        class(arena_t), intent(inout) :: this
        integer :: i

        ! Keep only first chunk
        if (this%chunk_count > 1) then
            do i = 2, this%chunk_count
                if (allocated(this%chunks(i)%data)) then
                    deallocate(this%chunks(i)%data)
                end if
            end do
            
            ! Resize chunks array
            this%chunk_count = 1
            this%total_capacity = this%chunks(1)%capacity
        end if
        
        ! Reset first chunk
        if (this%chunk_count >= 1) then
            this%chunks(1)%used = 0
            this%chunks(1)%data = 0  ! Zero memory
        end if
        
        this%current_chunk = 1
        this%total_allocated = 0
        this%generation = this%generation + 1
    end subroutine arena_clear

    ! Get arena statistics
    function arena_get_stats(this) result(stats)
        class(arena_t), intent(in) :: this
        type(arena_stats_t) :: stats

        stats%total_allocated = this%total_allocated
        stats%total_capacity = this%total_capacity
        stats%chunk_count = this%chunk_count
        stats%current_generation = this%generation
        
        if (this%total_capacity > 0) then
            stats%utilization = real(this%total_allocated) / &
                                real(this%total_capacity)
        else
            stats%utilization = 0.0
        end if
    end function arena_get_stats

    ! Grow arena by adding new chunk
    subroutine arena_grow(this, min_size)
        class(arena_t), intent(inout) :: this
        integer, intent(in), optional :: min_size
        type(arena_chunk_t), allocatable :: new_chunks(:)
        integer :: new_size, i

        ! Calculate new chunk size
        if (present(min_size)) then
            new_size = max(min_size, this%chunk_size)
        else
            new_size = this%chunk_size
        end if
        
        ! Grow chunks array if needed
        if (this%chunk_count >= size(this%chunks)) then
            allocate(new_chunks(this%chunk_count * 2))
            do i = 1, this%chunk_count
                new_chunks(i) = this%chunks(i)
            end do
            call move_alloc(new_chunks, this%chunks)
        end if
        
        ! Add new chunk
        this%chunk_count = this%chunk_count + 1
        this%chunks(this%chunk_count)%capacity = new_size
        allocate(this%chunks(this%chunk_count)%data(new_size))
        this%chunks(this%chunk_count)%data = 0
        this%chunks(this%chunk_count)%used = 0
        this%chunks(this%chunk_count)%generation = this%generation
        
        this%total_capacity = this%total_capacity + new_size
    end subroutine arena_grow

    ! Get data from arena (safe copy)
    subroutine arena_get_data(this, handle, buffer, status)
        class(arena_t), intent(in) :: this
        type(arena_handle_t), intent(in) :: handle
        integer(1), intent(out) :: buffer(:)
        logical, intent(out) :: status
        
        status = .false.
        
        ! Validate handle first
        if (.not. this%validate(handle)) return
        
        ! Check buffer size
        if (size(buffer) < handle%size) return
        
        ! Copy data
        buffer(1:handle%size) = this%chunks(handle%chunk_id)%data( &
            handle%offset + 1:handle%offset + handle%size)
        
        status = .true.
    end subroutine arena_get_data

    ! Set data in arena (safe copy)
    subroutine arena_set_data(this, handle, buffer, status)
        class(arena_t), intent(inout) :: this
        type(arena_handle_t), intent(in) :: handle
        integer(1), intent(in) :: buffer(:)
        logical, intent(out) :: status
        
        status = .false.
        
        ! Validate handle first
        if (.not. this%validate(handle)) return
        
        ! Check buffer size
        if (size(buffer) < handle%size) return
        
        ! Copy data
        this%chunks(handle%chunk_id)%data( &
            handle%offset + 1:handle%offset + handle%size) = buffer(1:handle%size)
        
        status = .true.
    end subroutine arena_set_data

    ! Helper: Align size to boundary
    pure function align_size(size, alignment) result(aligned)
        integer, intent(in) :: size, alignment
        integer :: aligned
        integer :: remainder

        remainder = mod(size, alignment)
        if (remainder == 0) then
            aligned = size
        else
            aligned = size + (alignment - remainder)
        end if
    end function align_size

    ! Check if handle is valid (without arena reference)
    pure function is_valid_handle(handle) result(valid)
        type(arena_handle_t), intent(in) :: handle
        logical :: valid

        valid = handle%generation > 0 .and. &
                handle%size > 0 .and. &
                handle%chunk_id > 0
    end function is_valid_handle

    ! Create null handle
    pure function null_handle() result(handle)
        type(arena_handle_t) :: handle

        handle%offset = 0
        handle%size = 0
        handle%generation = 0
        handle%chunk_id = 0
    end function null_handle

    ! Deep copy assignment for arena chunks (prevents double free)
    subroutine arena_chunk_assign(lhs, rhs)
        class(arena_chunk_t), intent(out) :: lhs
        type(arena_chunk_t), intent(in) :: rhs

        ! Copy scalar members
        lhs%capacity = rhs%capacity
        lhs%used = rhs%used
        lhs%generation = rhs%generation

        ! Deep copy allocatable data
        if (allocated(rhs%data)) then
            allocate(lhs%data(size(rhs%data)))
            lhs%data = rhs%data
        end if
    end subroutine arena_chunk_assign

    ! Deep copy assignment for arenas (prevents double free)
    subroutine arena_assign(lhs, rhs)
        class(arena_t), intent(out) :: lhs
        type(arena_t), intent(in) :: rhs
        integer :: i

        ! Copy scalar members
        lhs%chunk_count = rhs%chunk_count
        lhs%current_chunk = rhs%current_chunk
        lhs%total_allocated = rhs%total_allocated
        lhs%total_capacity = rhs%total_capacity
        lhs%chunk_size = rhs%chunk_size
        lhs%alignment = rhs%alignment
        lhs%generation = rhs%generation

        ! Deep copy chunks array
        if (allocated(rhs%chunks)) then
            allocate(lhs%chunks(size(rhs%chunks)))
            do i = 1, size(rhs%chunks)
                lhs%chunks(i) = rhs%chunks(i)  ! Uses chunk assignment operator
            end do
        end if
    end subroutine arena_assign

end module arena_memory