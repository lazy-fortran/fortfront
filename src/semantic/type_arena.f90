module type_arena
    ! Arena-based memory management for type system
    ! Eliminates GCC 15.2.1 allocatable component segfaults
    implicit none
    private

    ! Maximum types in arena
    integer, parameter, public :: MAX_TYPES = 10000

    ! Type entry for arena storage
    type :: type_entry_t
        integer :: kind = 0  ! TVAR, TINT, TREAL, etc.
        integer :: var_id = -1  ! for TVAR: type variable ID
        character(len=64) :: var_name = ""  ! for TVAR: fixed-size name
        integer :: size = 0  ! for TCHAR(len=size), TARRAY(size)
        integer :: arg_count = 0  ! number of arguments for TFUN
        integer :: arg_indices(8) = 0  ! indices to other types in arena (TFUN args)
        logical :: is_allocatable = .false.
        logical :: is_pointer = .false.
        logical :: is_target = .false.
        logical :: is_allocated = .false.
        logical :: needs_allocation_check = .false.
        logical :: needs_allocatable_string = .false.
    end type type_entry_t

    ! Type arena - holds all mono_type_t instances
    type, public :: type_arena_t
        type(type_entry_t) :: entries(MAX_TYPES)
        integer :: count = 0
        integer :: capacity = MAX_TYPES
    contains
        procedure :: push => type_arena_push
        procedure :: get => type_arena_get
        procedure :: clear => type_arena_clear
        procedure :: get_stats => type_arena_get_stats
        procedure :: create_var => type_arena_create_var
        procedure :: create_int => type_arena_create_int
        procedure :: create_real => type_arena_create_real
        procedure :: create_char => type_arena_create_char
        procedure :: create_logical => type_arena_create_logical
        procedure :: create_fun => type_arena_create_fun
        procedure :: create_array => type_arena_create_array
    end type type_arena_t

    ! Type reference - replaces mono_type_t
    type, public :: type_ref_t
        integer :: index = 0  ! index into arena
    contains
        procedure :: is_valid => type_ref_is_valid
        procedure :: equals => type_ref_equals
        procedure :: to_string => type_ref_to_string
    end type type_ref_t

    ! Arena statistics
    type, public :: type_arena_stats_t
        integer :: total_types = 0
        integer :: capacity = 0
        integer :: memory_usage = 0
    end type type_arena_stats_t

    ! Global arena instance
    type(type_arena_t), save, public :: global_type_arena

    ! Type kinds (re-exported from type_system_hm)
    integer, parameter, public :: TVAR = 1
    integer, parameter, public :: TINT = 2
    integer, parameter, public :: TREAL = 3
    integer, parameter, public :: TCHAR = 4
    integer, parameter, public :: TLOGICAL = 5
    integer, parameter, public :: TFUN = 6
    integer, parameter, public :: TARRAY = 7

    ! Public interface
    public :: type_entry_t
    public :: create_type_arena, init_type_arena

contains

    ! Create new type arena
    function create_type_arena() result(arena)
        type(type_arena_t) :: arena
        
        arena%count = 0
        arena%capacity = MAX_TYPES
        ! entries array is statically allocated
    end function create_type_arena

    ! Initialize type arena
    subroutine init_type_arena(arena)
        type(type_arena_t), intent(inout) :: arena
        
        arena%count = 0
        arena%capacity = MAX_TYPES
        ! Clear all entries
        arena%entries = type_entry_t()
    end subroutine init_type_arena

    ! Push new type entry to arena
    function type_arena_push(this, entry) result(index)
        class(type_arena_t), intent(inout) :: this
        type(type_entry_t), intent(in) :: entry
        integer :: index
        
        if (this%count >= this%capacity) then
            error stop "Type arena capacity exceeded"
        end if
        
        this%count = this%count + 1
        this%entries(this%count) = entry
        index = this%count
    end function type_arena_push

    ! Get type entry from arena
    function type_arena_get(this, index) result(entry)
        class(type_arena_t), intent(in) :: this
        integer, intent(in) :: index
        type(type_entry_t) :: entry
        
        if (index < 1 .or. index > this%count) then
            error stop "Invalid type arena index"
        end if
        
        entry = this%entries(index)
    end function type_arena_get

    ! Clear arena
    subroutine type_arena_clear(this)
        class(type_arena_t), intent(inout) :: this
        
        this%count = 0
        this%entries = type_entry_t()
    end subroutine type_arena_clear

    ! Get arena statistics
    function type_arena_get_stats(this) result(stats)
        class(type_arena_t), intent(in) :: this
        type(type_arena_stats_t) :: stats
        
        stats%total_types = this%count
        stats%capacity = this%capacity
        stats%memory_usage = this%capacity * storage_size(this%entries(1)) / 8
    end function type_arena_get_stats

    ! Factory method: create type variable
    function type_arena_create_var(this, id, name) result(type_ref)
        class(type_arena_t), intent(inout) :: this
        integer, intent(in) :: id
        character(len=*), intent(in), optional :: name
        type(type_ref_t) :: type_ref
        type(type_entry_t) :: entry
        
        entry%kind = TVAR
        entry%var_id = id
        
        if (present(name)) then
            if (len(name) <= len(entry%var_name)) then
                entry%var_name = name
            else
                entry%var_name = name(1:len(entry%var_name))
            end if
        else
            ! Generate name from id
            if (id <= 26) then
                entry%var_name = "'"//achar(iachar('a') + id - 1)
            else
                write(entry%var_name, '("''", A1, I0)') &
                    achar(iachar('a') + mod(id - 1, 26)), (id - 1)/26
            end if
        end if
        
        type_ref%index = this%push(entry)
    end function type_arena_create_var

    ! Factory method: create integer type
    function type_arena_create_int(this) result(type_ref)
        class(type_arena_t), intent(inout) :: this
        type(type_ref_t) :: type_ref
        type(type_entry_t) :: entry
        
        entry%kind = TINT
        type_ref%index = this%push(entry)
    end function type_arena_create_int

    ! Factory method: create real type
    function type_arena_create_real(this) result(type_ref)
        class(type_arena_t), intent(inout) :: this
        type(type_ref_t) :: type_ref
        type(type_entry_t) :: entry
        
        entry%kind = TREAL
        type_ref%index = this%push(entry)
    end function type_arena_create_real

    ! Factory method: create character type
    function type_arena_create_char(this, char_size) result(type_ref)
        class(type_arena_t), intent(inout) :: this
        integer, intent(in), optional :: char_size
        type(type_ref_t) :: type_ref
        type(type_entry_t) :: entry
        
        entry%kind = TCHAR
        if (present(char_size)) then
            entry%size = char_size
        else
            entry%size = 1
        end if
        type_ref%index = this%push(entry)
    end function type_arena_create_char

    ! Factory method: create logical type
    function type_arena_create_logical(this) result(type_ref)
        class(type_arena_t), intent(inout) :: this
        type(type_ref_t) :: type_ref
        type(type_entry_t) :: entry
        
        entry%kind = TLOGICAL
        type_ref%index = this%push(entry)
    end function type_arena_create_logical

    ! Factory method: create function type
    function type_arena_create_fun(this, arg_refs, result_ref) result(type_ref)
        class(type_arena_t), intent(inout) :: this
        type(type_ref_t), intent(in) :: arg_refs(:)
        type(type_ref_t), intent(in) :: result_ref
        type(type_ref_t) :: type_ref
        type(type_entry_t) :: entry
        integer :: i
        
        entry%kind = TFUN
        entry%arg_count = size(arg_refs) + 1  ! +1 for result type
        
        if (entry%arg_count > size(entry%arg_indices)) then
            error stop "Too many function arguments for fixed-size storage"
        end if
        
        ! Store argument references
        do i = 1, size(arg_refs)
            entry%arg_indices(i) = arg_refs(i)%index
        end do
        
        ! Store result reference as last argument
        entry%arg_indices(entry%arg_count) = result_ref%index
        
        type_ref%index = this%push(entry)
    end function type_arena_create_fun

    ! Factory method: create array type
    function type_arena_create_array(this, element_ref, array_size) result(type_ref)
        class(type_arena_t), intent(inout) :: this
        type(type_ref_t), intent(in) :: element_ref
        integer, intent(in), optional :: array_size
        type(type_ref_t) :: type_ref
        type(type_entry_t) :: entry
        
        entry%kind = TARRAY
        entry%arg_count = 1
        entry%arg_indices(1) = element_ref%index
        
        if (present(array_size)) then
            entry%size = array_size
        else
            entry%size = 0  ! dynamic/unknown size
        end if
        
        type_ref%index = this%push(entry)
    end function type_arena_create_array

    ! Check if type reference is valid
    function type_ref_is_valid(this) result(valid)
        class(type_ref_t), intent(in) :: this
        logical :: valid
        
        valid = (this%index > 0 .and. this%index <= global_type_arena%count)
    end function type_ref_is_valid

    ! Check type equality
    function type_ref_equals(this, other) result(equal)
        class(type_ref_t), intent(in) :: this
        type(type_ref_t), intent(in) :: other
        logical :: equal
        type(type_entry_t) :: entry1, entry2
        integer :: i
        
        if (.not. (this%is_valid() .and. other%is_valid())) then
            equal = .false.
            return
        end if
        
        if (this%index == other%index) then
            equal = .true.
            return
        end if
        
        entry1 = global_type_arena%get(this%index)
        entry2 = global_type_arena%get(other%index)
        
        ! Check basic equality
        equal = (entry1%kind == entry2%kind)
        if (.not. equal) return
        
        select case (entry1%kind)
        case (TVAR)
            equal = (entry1%var_id == entry2%var_id)
        case (TCHAR)
            equal = (entry1%size == entry2%size)
        case (TFUN, TARRAY)
            equal = (entry1%arg_count == entry2%arg_count)
            if (equal) then
                do i = 1, entry1%arg_count
                    if (entry1%arg_indices(i) /= entry2%arg_indices(i)) then
                        equal = .false.
                        exit
                    end if
                end do
            end if
        end select
    end function type_ref_equals

    ! Convert type to string representation
    recursive function type_ref_to_string(this) result(str)
        class(type_ref_t), intent(in) :: this
        character(len=256) :: str
        type(type_entry_t) :: entry
        type(type_ref_t) :: arg_ref
        character(len=256) :: arg_str
        integer :: i
        
        if (.not. this%is_valid()) then
            str = "<invalid>"
            return
        end if
        
        entry = global_type_arena%get(this%index)
        
        select case (entry%kind)
        case (TVAR)
            str = trim(entry%var_name)
        case (TINT)
            str = "integer"
        case (TREAL)
            str = "real"
        case (TCHAR)
            if (entry%size == 1) then
                str = "character"
            else
                write(str, '("character(len=", I0, ")")') entry%size
            end if
        case (TLOGICAL)
            str = "logical"
        case (TFUN)
            if (entry%arg_count > 1) then
                str = "("
                do i = 1, entry%arg_count - 1
                    arg_ref%index = entry%arg_indices(i)
                    arg_str = arg_ref%to_string()
                    if (i == 1) then
                        str = trim(str) // trim(arg_str)
                    else
                        str = trim(str) // " -> " // trim(arg_str)
                    end if
                end do
                ! Result type
                arg_ref%index = entry%arg_indices(entry%arg_count)
                arg_str = arg_ref%to_string()
                str = trim(str) // " -> " // trim(arg_str) // ")"
            else
                str = "() -> <unknown>"
            end if
        case (TARRAY)
            if (entry%arg_count > 0) then
                arg_ref%index = entry%arg_indices(1)
                arg_str = arg_ref%to_string()
                if (entry%size > 0) then
                    write(str, '(A, "(", I0, ")")') trim(arg_str), entry%size
                else
                    str = trim(arg_str) // "(:)"
                end if
            else
                str = "array"
            end if
        case default
            str = "<unknown>"
        end select
    end function type_ref_to_string

end module type_arena