module standardizer_decl_table_mod
    use arena_memory, only: arena_t, create_arena, destroy_arena
    use string_utils_mod, only: to_lower
    implicit none
    private

    ! Declaration entry stored in hash table
    type :: decl_entry_t
        character(len=64) :: name = ""
        character(len=64) :: var_type = ""
        logical :: is_declared = .false.
        integer :: proc_id = 0
        integer :: rank_signature = 0
        integer :: next_collision = 0  ! For collision chaining
        logical :: is_active = .false.
    end type decl_entry_t

    ! Hash table for declarations
    type, public :: decl_table_t
        type(decl_entry_t), allocatable :: entries(:)
        integer, allocatable :: hash_buckets(:)
        integer :: capacity = 0
        integer :: size = 0
        integer :: bucket_count = 0
        type(arena_t) :: arena
    contains
        procedure :: init => decl_table_init
        procedure :: reset => decl_table_reset
        procedure :: destroy => decl_table_destroy
        procedure :: add_or_update => decl_table_add_or_update
        procedure :: lookup => decl_table_lookup
        procedure :: iterate => decl_table_iterate
        procedure :: get_size => decl_table_get_size
        procedure :: to_arrays => decl_table_to_arrays
    end type decl_table_t

contains

    ! Initialize the declaration table
    subroutine decl_table_init(this, initial_capacity)
        class(decl_table_t), intent(inout) :: this
        integer, intent(in), optional :: initial_capacity
        integer :: cap

        cap = 256  ! Default capacity
        if (present(initial_capacity)) cap = max(16, initial_capacity)

        this%capacity = cap
        this%bucket_count = cap
        this%size = 0

        if (allocated(this%entries)) deallocate (this%entries)
        if (allocated(this%hash_buckets)) deallocate (this%hash_buckets)

        allocate (this%entries(cap))
        allocate (this%hash_buckets(this%bucket_count))
        this%hash_buckets = 0

        ! Create arena for auxiliary allocations
        this%arena = create_arena(chunk_size=8192)
    end subroutine decl_table_init

    ! Reset the table (clear all entries but keep capacity)
    subroutine decl_table_reset(this)
        class(decl_table_t), intent(inout) :: this
        integer :: i

        do i = 1, this%capacity
            if (this%entries(i)%is_active) then
                this%entries(i)%name = ""
                this%entries(i)%var_type = ""
                this%entries(i)%is_declared = .false.
                this%entries(i)%is_active = .false.
            end if
        end do

        this%hash_buckets = 0
        this%size = 0

        ! Reset arena
        call destroy_arena(this%arena)
        this%arena = create_arena(chunk_size=8192)
    end subroutine decl_table_reset

    ! Destroy the table
    subroutine decl_table_destroy(this)
        class(decl_table_t), intent(inout) :: this

        if (allocated(this%entries)) deallocate (this%entries)
        if (allocated(this%hash_buckets)) deallocate (this%hash_buckets)

        call destroy_arena(this%arena)

        this%capacity = 0
        this%size = 0
        this%bucket_count = 0
    end subroutine decl_table_destroy

    ! Hash function for declaration keys
    pure integer function hash_decl_key(name, proc_id, rank_sig) result(hash)
        character(len=*), intent(in) :: name
        integer, intent(in) :: proc_id
        integer, intent(in) :: rank_sig
        integer :: i
        integer(kind=8) :: h

        ! Start with string hash
        h = 5381_8
        do i = 1, len_trim(name)
            h = ieor(ishft(h, 5) + h, int(ichar(name(i:i)), kind=8))
        end do

        ! Mix in proc_id and rank_sig
        h = ieor(h, int(proc_id, kind=8))
        h = ieor(ishft(h, 3), int(rank_sig, kind=8))

        hash = int(iand(h, 2147483647_8))  ! Ensure positive
    end function hash_decl_key

    ! Grow the table when it gets full
    subroutine decl_table_grow(this)
        class(decl_table_t), intent(inout) :: this
        type(decl_entry_t), allocatable :: new_entries(:)
        integer, allocatable :: new_buckets(:)
        integer :: new_cap, new_bucket_count
        integer :: i, bucket_idx, hash_val

        new_cap = this%capacity * 2
        new_bucket_count = new_cap

        allocate (new_entries(new_cap))
        allocate (new_buckets(new_bucket_count))
        new_buckets = 0

        ! Copy and rehash
        do i = 1, this%capacity
            if (this%entries(i)%is_active) then
                new_entries(i) = this%entries(i)
                new_entries(i)%next_collision = 0

                hash_val = hash_decl_key(this%entries(i)%name, &
                                         this%entries(i)%proc_id, &
                                         this%entries(i)%rank_signature)
                bucket_idx = mod(hash_val, new_bucket_count) + 1

                if (new_buckets(bucket_idx) == 0) then
                    new_buckets(bucket_idx) = i
                else
                    new_entries(i)%next_collision = new_buckets(bucket_idx)
                    new_buckets(bucket_idx) = i
                end if
            end if
        end do

        call move_alloc(new_entries, this%entries)
        call move_alloc(new_buckets, this%hash_buckets)
        this%capacity = new_cap
        this%bucket_count = new_bucket_count
    end subroutine decl_table_grow

    ! Add or update a declaration
    subroutine decl_table_add_or_update(this, name, var_type, is_declared, &
                                        proc_id, rank_sig)
        class(decl_table_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: var_type
        logical, intent(in), optional :: is_declared
        integer, intent(in), optional :: proc_id
        integer, intent(in), optional :: rank_sig
        integer :: bucket_idx, entry_id, hash_val
        integer :: free_slot, pid, rsig
        character(len=64) :: normalized_name, normalized_existing
        logical :: decl_flag

        if (len_trim(name) == 0) return

        pid = 0
        if (present(proc_id)) pid = proc_id

        rsig = 0
        if (present(rank_sig)) rsig = rank_sig

        decl_flag = .true.
        if (present(is_declared)) decl_flag = is_declared

        ! Normalize name for comparison
        normalized_name = to_lower(trim(name))

        ! Compute hash
        hash_val = hash_decl_key(normalized_name, pid, rsig)
        bucket_idx = mod(hash_val, this%bucket_count) + 1

        ! Search for existing entry
        entry_id = this%hash_buckets(bucket_idx)
        do while (entry_id /= 0)
            if (this%entries(entry_id)%is_active) then
                normalized_existing = to_lower(trim(this%entries(entry_id)%name))
                if (normalized_existing == normalized_name .and. &
                    this%entries(entry_id)%proc_id == pid .and. &
                    this%entries(entry_id)%rank_signature == rsig) then
                    ! Found - update
                    this%entries(entry_id)%var_type = var_type
                    this%entries(entry_id)%is_declared = decl_flag
                    return
                end if
            end if
            entry_id = this%entries(entry_id)%next_collision
        end do

        ! Not found - add new entry
        if (this%size >= this%capacity * 3 / 4) then
            call decl_table_grow(this)
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
            call decl_table_grow(this)
            bucket_idx = mod(hash_val, this%bucket_count) + 1
            do entry_id = 1, this%capacity
                if (.not. this%entries(entry_id)%is_active) then
                    free_slot = entry_id
                    exit
                end if
            end do
        end if

        ! Create new entry
        this%entries(free_slot)%name = name
        this%entries(free_slot)%var_type = var_type
        this%entries(free_slot)%is_declared = decl_flag
        this%entries(free_slot)%proc_id = pid
        this%entries(free_slot)%rank_signature = rsig
        this%entries(free_slot)%is_active = .true.
        this%entries(free_slot)%next_collision = this%hash_buckets(bucket_idx)

        this%hash_buckets(bucket_idx) = free_slot
        this%size = this%size + 1
    end subroutine decl_table_add_or_update

    ! Lookup a declaration
    function decl_table_lookup(this, name, proc_id, rank_sig) result(found_entry)
        class(decl_table_t), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: proc_id
        integer, intent(in), optional :: rank_sig
        type(decl_entry_t) :: found_entry
        integer :: bucket_idx, entry_id, hash_val
        integer :: pid, rsig
        character(len=64) :: normalized_name, normalized_existing

        found_entry%is_active = .false.

        if (len_trim(name) == 0) return

        pid = 0
        if (present(proc_id)) pid = proc_id

        rsig = 0
        if (present(rank_sig)) rsig = rank_sig

        normalized_name = to_lower(trim(name))
        hash_val = hash_decl_key(normalized_name, pid, rsig)
        bucket_idx = mod(hash_val, this%bucket_count) + 1

        entry_id = this%hash_buckets(bucket_idx)
        do while (entry_id /= 0)
            if (this%entries(entry_id)%is_active) then
                normalized_existing = to_lower(trim(this%entries(entry_id)%name))
                if (normalized_existing == normalized_name .and. &
                    this%entries(entry_id)%proc_id == pid .and. &
                    this%entries(entry_id)%rank_signature == rsig) then
                    found_entry = this%entries(entry_id)
                    return
                end if
            end if
            entry_id = this%entries(entry_id)%next_collision
        end do
    end function decl_table_lookup

    ! Iterate over all active entries
    subroutine decl_table_iterate(this, callback)
        class(decl_table_t), intent(in) :: this
        interface
            subroutine callback(entry)
                import :: decl_entry_t
                type(decl_entry_t), intent(in) :: entry
            end subroutine callback
        end interface
        integer :: i

        do i = 1, this%capacity
            if (this%entries(i)%is_active) then
                call callback(this%entries(i))
            end if
        end do
    end subroutine decl_table_iterate

    ! Get number of active entries
    pure integer function decl_table_get_size(this) result(count)
        class(decl_table_t), intent(in) :: this
        count = this%size
    end function decl_table_get_size

    ! Convert table to arrays (for compatibility with existing code)
    subroutine decl_table_to_arrays(this, var_names, var_types, var_declared, &
                                    var_count, max_size)
        class(decl_table_t), intent(in) :: this
        character(len=64), intent(out) :: var_names(:)
        character(len=64), intent(out) :: var_types(:)
        logical, intent(out) :: var_declared(:)
        integer, intent(out) :: var_count
        integer, intent(in) :: max_size
        integer :: i, idx

        idx = 0
        do i = 1, this%capacity
            if (this%entries(i)%is_active) then
                idx = idx + 1
                if (idx > max_size) exit
                var_names(idx) = this%entries(i)%name
                var_types(idx) = this%entries(i)%var_type
                var_declared(idx) = this%entries(i)%is_declared
            end if
        end do
        var_count = idx
    end subroutine decl_table_to_arrays

end module standardizer_decl_table_mod
