module type_env_arena
    ! Arena-based type environment
    ! Eliminates allocatable component issues in type_env_t
    use type_arena, only: type_ref_t, global_type_arena
    implicit none
    private

    ! Maximum entries in environment
    integer, parameter :: MAX_ENV_ENTRIES = 1000

    ! Environment entry
    type :: env_entry_t
        character(len=256) :: name = ""
        type(type_ref_t) :: type_scheme
        logical :: is_quantified = .false.
        integer :: quantifier_count = 0
        integer :: quantifier_ids(32) = 0  ! Fixed-size array for type variables
    end type env_entry_t

    ! Type environment with arena-based storage
    type, public :: type_env_arena_t
        type(env_entry_t) :: entries(MAX_ENV_ENTRIES)
        integer :: count = 0
        integer :: capacity = MAX_ENV_ENTRIES
    contains
        procedure :: lookup => env_arena_lookup
        procedure :: extend => env_arena_extend
        procedure :: extend_many => env_arena_extend_many
        procedure :: remove => env_arena_remove
        procedure :: clear => env_arena_clear
        procedure :: get_names => env_arena_get_names
        procedure :: get_count => env_arena_get_count
    end type type_env_arena_t

    ! Public interface
    public :: env_entry_t, create_type_env_arena

contains

    ! Create new type environment arena
    function create_type_env_arena() result(env)
        type(type_env_arena_t) :: env
        
        env%count = 0
        env%capacity = MAX_ENV_ENTRIES
        env%entries = env_entry_t()
    end function create_type_env_arena

    ! Lookup type scheme for identifier
    function env_arena_lookup(this, name) result(type_scheme)
        class(type_env_arena_t), intent(in) :: this
        character(len=*), intent(in) :: name
        type(type_ref_t) :: type_scheme
        integer :: i
        
        ! Initialize to invalid reference
        type_scheme%index = 0
        
        do i = 1, this%count
            if (trim(this%entries(i)%name) == trim(name)) then
                type_scheme = this%entries(i)%type_scheme
                exit
            end if
        end do
    end function env_arena_lookup

    ! Extend environment with new binding
    subroutine env_arena_extend(this, name, type_scheme, quantifiers)
        class(type_env_arena_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        type(type_ref_t), intent(in) :: type_scheme
        integer, intent(in), optional :: quantifiers(:)
        integer :: i
        
        if (this%count >= this%capacity) then
            error stop "Type environment capacity exceeded"
        end if
        
        ! Check if already exists - update instead of adding
        do i = 1, this%count
            if (trim(this%entries(i)%name) == trim(name)) then
                this%entries(i)%type_scheme = type_scheme
                
                ! Update quantifiers if provided
                if (present(quantifiers)) then
                    this%entries(i)%is_quantified = .true.
                    this%entries(i)%quantifier_count = size(quantifiers)
                    if (size(quantifiers) <= size(this%entries(i)%quantifier_ids)) then
                        this%entries(i)%quantifier_ids(1:size(quantifiers)) = quantifiers
                    else
                        error stop "Too many quantifiers for fixed-size storage"
                    end if
                else
                    this%entries(i)%is_quantified = .false.
                    this%entries(i)%quantifier_count = 0
                end if
                return
            end if
        end do
        
        ! Add new entry
        this%count = this%count + 1
        this%entries(this%count)%name = name
        this%entries(this%count)%type_scheme = type_scheme
        
        if (present(quantifiers)) then
            this%entries(this%count)%is_quantified = .true.
            this%entries(this%count)%quantifier_count = size(quantifiers)
            if (size(quantifiers) <= size(this%entries(this%count)%quantifier_ids)) then
                this%entries(this%count)%quantifier_ids(1:size(quantifiers)) = quantifiers
            else
                error stop "Too many quantifiers for fixed-size storage"
            end if
        else
            this%entries(this%count)%is_quantified = .false.
            this%entries(this%count)%quantifier_count = 0
        end if
    end subroutine env_arena_extend

    ! Extend environment with multiple bindings
    subroutine env_arena_extend_many(this, names, type_schemes, quantifiers_list)
        class(type_env_arena_t), intent(inout) :: this
        character(len=*), intent(in) :: names(:)
        type(type_ref_t), intent(in) :: type_schemes(:)
        integer, intent(in), optional :: quantifiers_list(:, :)
        integer :: i
        
        if (size(names) /= size(type_schemes)) then
            error stop "Names and type schemes arrays must have same size"
        end if
        
        do i = 1, size(names)
            if (present(quantifiers_list)) then
                call this%extend(names(i), type_schemes(i), quantifiers_list(:, i))
            else
                call this%extend(names(i), type_schemes(i))
            end if
        end do
    end subroutine env_arena_extend_many

    ! Remove binding from environment
    subroutine env_arena_remove(this, name)
        class(type_env_arena_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        integer :: i, j
        
        do i = 1, this%count
            if (trim(this%entries(i)%name) == trim(name)) then
                ! Shift remaining entries down
                do j = i, this%count - 1
                    this%entries(j) = this%entries(j + 1)
                end do
                
                ! Clear last entry
                this%entries(this%count) = env_entry_t()
                this%count = this%count - 1
                exit
            end if
        end do
    end subroutine env_arena_remove

    ! Clear all entries
    subroutine env_arena_clear(this)
        class(type_env_arena_t), intent(inout) :: this
        
        this%count = 0
        this%entries = env_entry_t()
    end subroutine env_arena_clear

    ! Get all names in environment
    subroutine env_arena_get_names(this, names, actual_count)
        class(type_env_arena_t), intent(in) :: this
        character(len=256), intent(out) :: names(:)
        integer, intent(out) :: actual_count
        integer :: i, max_count
        
        max_count = min(size(names), this%count)
        
        do i = 1, max_count
            names(i) = this%entries(i)%name
        end do
        
        actual_count = max_count
    end subroutine env_arena_get_names

    ! Get count of entries
    function env_arena_get_count(this) result(count)
        class(type_env_arena_t), intent(in) :: this
        integer :: count
        
        count = this%count
    end function env_arena_get_count

end module type_env_arena