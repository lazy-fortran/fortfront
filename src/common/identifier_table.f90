module identifier_table
    use, intrinsic :: iso_fortran_env, only: int32, int64
    implicit none
    private

    integer, parameter, public :: identifier_id_kind = int32

    type :: identifier_entry_t
        character(len=:), allocatable :: value
        integer(int32) :: hash = 0_int32
        integer(int32) :: next = 0_int32
    end type identifier_entry_t

    type, public :: identifier_table_t
        type(identifier_entry_t), allocatable :: entries(:)
        integer(int32), allocatable :: buckets(:)
        integer(int32) :: count = 0_int32
        integer(int32) :: entry_capacity = 0_int32
        integer(int32) :: bucket_count = 0_int32
    end type identifier_table_t

    public :: identifier_table_init, identifier_table_reset
    public :: identifier_table_is_initialized
    public :: identifier_table_intern, identifier_table_find
    public :: identifier_table_get

contains

    subroutine identifier_table_init(table, initial_bucket_count)
        type(identifier_table_t), intent(inout) :: table
        integer, intent(in), optional :: initial_bucket_count
        integer :: bucket_count

        table%count = 0_int32
        table%entry_capacity = 0_int32
        table%bucket_count = 0_int32
        if (allocated(table%entries)) deallocate (table%entries)
        if (allocated(table%buckets)) deallocate (table%buckets)

        if (present(initial_bucket_count)) then
            bucket_count = max(16, initial_bucket_count)
        else
            bucket_count = 64
        end if
        call rebuild_buckets(table, bucket_count)
    end subroutine identifier_table_init

    subroutine identifier_table_reset(table)
        type(identifier_table_t), intent(inout) :: table

        table%count = 0_int32
        if (allocated(table%buckets)) then
            table%buckets = 0_int32
        end if
    end subroutine identifier_table_reset

    logical function identifier_table_is_initialized(table) result(initialized)
        type(identifier_table_t), intent(in) :: table

        initialized = allocated(table%buckets)
    end function identifier_table_is_initialized

    function identifier_table_intern(table, raw_value) result(id)
        type(identifier_table_t), intent(inout) :: table
        character(len=*), intent(in) :: raw_value
        integer(int32) :: id
        character(len=:), allocatable :: key
        integer(int32) :: hash

        key = trim(raw_value)
        if (.not. allocated(table%buckets)) then
            call identifier_table_init(table)
        end if

        hash = compute_hash(key)
        id = lookup_entry(table, key, hash)
        if (id > 0_int32) return

        call ensure_entry_capacity(table, table%count + 1_int32)
        call ensure_bucket_capacity(table, table%count + 1_int32)

        table%count = table%count + 1_int32
        if (allocated(table%entries(table%count)%value)) then
            deallocate (table%entries(table%count)%value)
        end if
        allocate (character(len=len(key)) :: table%entries(table%count)%value)
        table%entries(table%count)%value = key
        table%entries(table%count)%hash = hash
        table%entries(table%count)%next = 0_int32

        call insert_into_buckets(table, table%count)
        id = table%count
    end function identifier_table_intern

    function identifier_table_find(table, raw_value) result(id)
        type(identifier_table_t), intent(in) :: table
        character(len=*), intent(in) :: raw_value
        integer(int32) :: id
        character(len=:), allocatable :: key
        integer(int32) :: hash

        if (.not. allocated(table%buckets)) then
            id = 0_int32
            return
        end if

        if (table%count <= 0_int32) then
            id = 0_int32
            return
        end if

        key = trim(raw_value)
        hash = compute_hash(key)
        id = lookup_entry(table, key, hash)
    end function identifier_table_find

    function identifier_table_get(table, id) result(value)
        type(identifier_table_t), intent(in) :: table
        integer(int32), intent(in) :: id
        character(len=:), allocatable :: value

        if (id <= 0_int32 .or. id > table%count) then
            allocate (character(len=0) :: value)
            value = ''
            return
        end if

        value = table%entries(id)%value
    end function identifier_table_get

    pure function compute_hash(key) result(hash)
        character(len=*), intent(in) :: key
        integer(int32) :: hash
        integer(int64) :: acc
        integer :: i, length

        acc = 1469598103934665603_int64
        length = len_trim(key)
        do i = 1, length
            acc = ieor(acc, int(iachar(key(i:i)), int64))
            acc = acc * 1099511628211_int64
        end do
        hash = int(iand(acc, int(z'7fffffff', int64)), int32)
        if (hash == 0_int32) hash = 1_int32
    end function compute_hash

    subroutine ensure_entry_capacity(table, required)
        type(identifier_table_t), intent(inout) :: table
        integer(int32), intent(in) :: required
        type(identifier_entry_t), allocatable :: new_entries(:)
        integer :: new_capacity

        if (required <= table%entry_capacity) return

        if (table%entry_capacity <= 0_int32) then
            new_capacity = 32
        else
            new_capacity = table%entry_capacity
        end if

        do while (required > new_capacity)
            new_capacity = new_capacity * 2
        end do

        allocate (new_entries(new_capacity))
        if (table%entry_capacity > 0_int32 .and. table%count > 0_int32) then
            new_entries(1:table%count) = table%entries(1:table%count)
        end if
        call move_alloc(new_entries, table%entries)
        table%entry_capacity = new_capacity
    end subroutine ensure_entry_capacity

    subroutine ensure_bucket_capacity(table, required_count)
        type(identifier_table_t), intent(inout) :: table
        integer(int32), intent(in) :: required_count
        integer :: desired

        desired = table%bucket_count
        if (desired <= 0) desired = 64

        do while (required_count > desired * 3 / 4)
            desired = desired * 2
        end do

        if (desired /= table%bucket_count) then
            call rebuild_buckets(table, desired)
        end if
    end subroutine ensure_bucket_capacity

    subroutine rebuild_buckets(table, new_bucket_count)
        type(identifier_table_t), intent(inout) :: table
        integer, intent(in) :: new_bucket_count
        integer(int32) :: i

        if (allocated(table%buckets)) deallocate (table%buckets)
        allocate (table%buckets(new_bucket_count))
        table%buckets = 0_int32
        table%bucket_count = new_bucket_count

        if (table%count <= 0_int32) return

        do i = table%count, 1, -1
            call insert_into_buckets(table, i)
        end do
    end subroutine rebuild_buckets

    subroutine insert_into_buckets(table, entry_id)
        type(identifier_table_t), intent(inout) :: table
        integer(int32), intent(in) :: entry_id
        integer :: bucket

        bucket = bucket_index(table%entries(entry_id)%hash, table%bucket_count)
        table%entries(entry_id)%next = table%buckets(bucket)
        table%buckets(bucket) = entry_id
    end subroutine insert_into_buckets

    integer function bucket_index(hash, bucket_count) result(index)
        integer(int32), intent(in) :: hash
        integer, intent(in) :: bucket_count
        integer(int64) :: mod_value

        if (bucket_count <= 0) then
            index = 1
            return
        end if

        mod_value = modulo(int(hash, int64), int(bucket_count, int64))
        index = int(mod_value, kind(index)) + 1
    end function bucket_index

    function lookup_entry(table, key, hash) result(id)
        type(identifier_table_t), intent(in) :: table
        character(len=*), intent(in) :: key
        integer(int32), intent(in) :: hash
        integer(int32) :: id
        integer :: bucket
        integer(int32) :: current

        id = 0_int32
        if (table%bucket_count <= 0_int32) return

        bucket = bucket_index(hash, table%bucket_count)
        current = table%buckets(bucket)

        do while (current > 0_int32)
            if (table%entries(current)%hash == hash) then
                if (table%entries(current)%value == key) then
                    id = current
                    return
                end if
            end if
            current = table%entries(current)%next
        end do
    end function lookup_entry

end module identifier_table
