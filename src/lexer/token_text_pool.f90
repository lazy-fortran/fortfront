module token_text_pool_mod
    use string_utils_mod, only: to_lower
    implicit none
    private

    ! Token text handle type - cheap to copy, references shared storage
    type, public :: token_text_handle_t
        integer :: pool_id = 0      ! Index into pool's storage
        integer :: hash_val = 0     ! Cached hash for quick lookups
    end type token_text_handle_t

    ! Internal storage entry for a unique string
    type :: pool_entry_t
        character(len=:), allocatable :: original
        character(len=:), allocatable :: lowered
        integer :: refcount = 0
        integer :: next_collision = 0  ! For hash collision chaining
        logical :: is_active = .false.
    end type pool_entry_t

    ! The token text pool itself
    type, public :: token_text_pool_t
        type(pool_entry_t), allocatable :: entries(:)
        integer, allocatable :: hash_buckets(:)
        integer :: capacity = 0
        integer :: size = 0
        integer :: bucket_count = 0
    contains
        procedure :: init => pool_init
        procedure :: destroy => pool_destroy
        procedure :: intern => pool_intern
        procedure :: release => pool_release
        procedure :: get_original => pool_get_original
        procedure :: get_lower => pool_get_lower
        procedure :: get_refcount => pool_get_refcount
    end type token_text_pool_t

    ! Public null handle constant
    public :: null_text_handle
    public :: is_null_handle

contains

    ! Create a null handle
    pure function null_text_handle() result(handle)
        type(token_text_handle_t) :: handle
        handle%pool_id = 0
        handle%hash_val = 0
    end function null_text_handle

    ! Check if handle is null
    pure logical function is_null_handle(handle) result(is_null)
        type(token_text_handle_t), intent(in) :: handle
        is_null = (handle%pool_id == 0)
    end function is_null_handle

    ! Initialize the pool with initial capacity
    subroutine pool_init(this, initial_capacity)
        class(token_text_pool_t), intent(inout) :: this
        integer, intent(in), optional :: initial_capacity
        integer :: cap

        cap = 1024  ! Default initial capacity
        if (present(initial_capacity)) cap = max(16, initial_capacity)

        this%capacity = cap
        this%bucket_count = cap
        this%size = 0

        if (allocated(this%entries)) deallocate (this%entries)
        if (allocated(this%hash_buckets)) deallocate (this%hash_buckets)

        allocate (this%entries(cap))
        allocate (this%hash_buckets(this%bucket_count))
        this%hash_buckets = 0  ! Empty buckets
    end subroutine pool_init

    ! Destroy the pool and free all resources
    subroutine pool_destroy(this)
        class(token_text_pool_t), intent(inout) :: this
        integer :: i

        if (allocated(this%entries)) then
            do i = 1, this%capacity
                if (this%entries(i)%is_active) then
                    if (allocated(this%entries(i)%original)) then
                        deallocate (this%entries(i)%original)
                    end if
                    if (allocated(this%entries(i)%lowered)) then
                        deallocate (this%entries(i)%lowered)
                    end if
                end if
            end do
            deallocate (this%entries)
        end if

        if (allocated(this%hash_buckets)) deallocate (this%hash_buckets)

        this%capacity = 0
        this%size = 0
        this%bucket_count = 0
    end subroutine pool_destroy

    ! Simple hash function for strings
    pure integer function hash_string(str) result(hash)
        character(len=*), intent(in) :: str
        integer :: i
        integer(kind=8) :: h

        h = 5381_8
        do i = 1, len(str)
            h = ieor(ishft(h, 5) + h, int(ichar(str(i:i)), kind=8))
        end do
        hash = int(iand(h, 2147483647_8))  ! Ensure positive
    end function hash_string

    ! Grow the pool when it's getting full
    subroutine pool_grow(this)
        class(token_text_pool_t), intent(inout) :: this
        type(pool_entry_t), allocatable :: new_entries(:)
        integer, allocatable :: new_buckets(:)
        integer :: new_cap, new_bucket_count
        integer :: i, bucket_idx, hash_val

        new_cap = this%capacity * 2
        new_bucket_count = new_cap

        ! Allocate new storage
        allocate (new_entries(new_cap))
        allocate (new_buckets(new_bucket_count))
        new_buckets = 0

        ! Copy existing entries and rehash
        do i = 1, this%capacity
            if (this%entries(i)%is_active) then
                new_entries(i) = this%entries(i)
                new_entries(i)%next_collision = 0

                ! Rehash into new bucket array
                hash_val = hash_string(this%entries(i)%original)
                bucket_idx = mod(hash_val, new_bucket_count) + 1

                if (new_buckets(bucket_idx) == 0) then
                    new_buckets(bucket_idx) = i
                else
                    ! Chain collision
                    new_entries(i)%next_collision = new_buckets(bucket_idx)
                    new_buckets(bucket_idx) = i
                end if
            end if
        end do

        ! Replace old storage
        call move_alloc(new_entries, this%entries)
        call move_alloc(new_buckets, this%hash_buckets)
        this%capacity = new_cap
        this%bucket_count = new_bucket_count
    end subroutine pool_grow

    ! Intern a string in the pool (returns handle, increments refcount)
    function pool_intern(this, text) result(handle)
        class(token_text_pool_t), intent(inout) :: this
        character(len=*), intent(in) :: text
        type(token_text_handle_t) :: handle
        integer :: bucket_idx, entry_id, hash_val
        integer :: free_slot

        if (len(text) == 0) then
            handle = null_text_handle()
            return
        end if

        ! Compute hash
        hash_val = hash_string(text)
        bucket_idx = mod(hash_val, this%bucket_count) + 1

        ! Search for existing entry in this bucket's chain
        entry_id = this%hash_buckets(bucket_idx)
        do while (entry_id /= 0)
            if (this%entries(entry_id)%is_active) then
                if (this%entries(entry_id)%original == text) then
                    ! Found existing entry - increment refcount and return
                    this%entries(entry_id)%refcount = &
                        this%entries(entry_id)%refcount + 1
                    handle%pool_id = entry_id
                    handle%hash_val = hash_val
                    return
                end if
            end if
            entry_id = this%entries(entry_id)%next_collision
        end do

        ! Not found - need to create new entry
        ! Check if we need to grow
        if (this%size >= this%capacity * 3 / 4) then
            call pool_grow(this)
            ! Recompute bucket after grow
            bucket_idx = mod(hash_val, this%bucket_count) + 1
        end if

        ! Find free slot
        free_slot = 0
        do entry_id = 1, this%capacity
            if (.not. this%entries(entry_id)%is_active) then
                free_slot = entry_id
                exit
            end if
        end do

        if (free_slot == 0) then
            ! Should not happen after grow, but handle gracefully
            call pool_grow(this)
            bucket_idx = mod(hash_val, this%bucket_count) + 1
            do entry_id = 1, this%capacity
                if (.not. this%entries(entry_id)%is_active) then
                    free_slot = entry_id
                    exit
                end if
            end do
        end if

        ! Create new entry
        this%entries(free_slot)%original = text
        this%entries(free_slot)%refcount = 1
        this%entries(free_slot)%is_active = .true.
        this%entries(free_slot)%next_collision = this%hash_buckets(bucket_idx)
        if (allocated(this%entries(free_slot)%lowered)) then
            deallocate (this%entries(free_slot)%lowered)
        end if

        ! Insert at head of bucket chain
        this%hash_buckets(bucket_idx) = free_slot
        this%size = this%size + 1

        handle%pool_id = free_slot
        handle%hash_val = hash_val
    end function pool_intern

    ! Release a handle (decrements refcount, frees if reaches 0)
    subroutine pool_release(this, handle)
        class(token_text_pool_t), intent(inout) :: this
        type(token_text_handle_t), intent(in) :: handle

        if (is_null_handle(handle)) return
        if (handle%pool_id < 1 .or. handle%pool_id > this%capacity) return
        if (.not. this%entries(handle%pool_id)%is_active) return

        this%entries(handle%pool_id)%refcount = &
            this%entries(handle%pool_id)%refcount - 1

        if (this%entries(handle%pool_id)%refcount <= 0) then
            ! Free the entry
            if (allocated(this%entries(handle%pool_id)%original)) then
                deallocate (this%entries(handle%pool_id)%original)
            end if
            if (allocated(this%entries(handle%pool_id)%lowered)) then
                deallocate (this%entries(handle%pool_id)%lowered)
            end if
            this%entries(handle%pool_id)%is_active = .false.
            this%size = this%size - 1
        end if
    end subroutine pool_release

    ! Get original text from handle
    function pool_get_original(this, handle) result(text)
        class(token_text_pool_t), intent(in) :: this
        type(token_text_handle_t), intent(in) :: handle
        character(len=:), allocatable :: text

        if (is_null_handle(handle)) then
            text = ""
            return
        end if

        if (handle%pool_id < 1 .or. handle%pool_id > this%capacity) then
            text = ""
            return
        end if

        if (.not. this%entries(handle%pool_id)%is_active) then
            text = ""
            return
        end if

        if (allocated(this%entries(handle%pool_id)%original)) then
            text = this%entries(handle%pool_id)%original
        else
            text = ""
        end if
    end function pool_get_original

    ! Get lowercased text from handle (caches on first access)
    function pool_get_lower(this, handle) result(text)
        class(token_text_pool_t), intent(inout) :: this
        type(token_text_handle_t), intent(in) :: handle
        character(len=:), allocatable :: text

        if (is_null_handle(handle)) then
            text = ""
            return
        end if

        if (handle%pool_id < 1 .or. handle%pool_id > this%capacity) then
            text = ""
            return
        end if

        if (.not. this%entries(handle%pool_id)%is_active) then
            text = ""
            return
        end if

        ! Compute lowercase on first access and cache it
        if (.not. allocated(this%entries(handle%pool_id)%lowered)) then
            if (allocated(this%entries(handle%pool_id)%original)) then
                this%entries(handle%pool_id)%lowered = &
                    to_lower(this%entries(handle%pool_id)%original)
            else
                this%entries(handle%pool_id)%lowered = ""
            end if
        end if

        text = this%entries(handle%pool_id)%lowered
    end function pool_get_lower

    ! Get reference count for debugging
    pure integer function pool_get_refcount(this, handle) result(count)
        class(token_text_pool_t), intent(in) :: this
        type(token_text_handle_t), intent(in) :: handle

        if (is_null_handle(handle)) then
            count = 0
            return
        end if

        if (handle%pool_id < 1 .or. handle%pool_id > this%capacity) then
            count = 0
            return
        end if

        if (.not. this%entries(handle%pool_id)%is_active) then
            count = 0
            return
        end if

        count = this%entries(handle%pool_id)%refcount
    end function pool_get_refcount

end module token_text_pool_mod
