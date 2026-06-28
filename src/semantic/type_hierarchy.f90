module type_hierarchy
    ! Type hierarchy tracking for F2003 derived type inheritance
    implicit none
    private

    public :: type_hierarchy_t, type_hierarchy_entry_t
    public :: create_type_hierarchy, destroy_type_hierarchy

    integer, parameter :: INITIAL_CAPACITY = 64
    integer, parameter :: MAX_CHAIN_DEPTH = 100

    type :: type_hierarchy_entry_t
        character(len=:), allocatable :: type_name
        character(len=:), allocatable :: parent_name
        logical :: is_abstract = .false.
        character(len=:), allocatable :: deferred_procedures(:)
        integer :: depth = 0
    end type type_hierarchy_entry_t

    type :: type_hierarchy_t
        type(type_hierarchy_entry_t), allocatable :: entries(:)
        integer :: count = 0
        integer :: capacity = 0
    contains
        procedure :: register_type => hierarchy_register_type
        procedure :: find_parent => hierarchy_find_parent
        procedure :: is_subtype_of => hierarchy_is_subtype_of
        procedure :: get_inheritance_chain => hierarchy_get_inheritance_chain
        procedure :: validate_no_cycles => hierarchy_validate_no_cycles
        procedure :: find_entry => hierarchy_find_entry
        procedure :: ensure_capacity => hierarchy_ensure_capacity
        procedure :: get_depth => hierarchy_get_depth
    end type type_hierarchy_t

contains

    function create_type_hierarchy() result(hierarchy)
        type(type_hierarchy_t) :: hierarchy
        hierarchy%count = 0
        hierarchy%capacity = INITIAL_CAPACITY
        allocate (hierarchy%entries(INITIAL_CAPACITY))
    end function create_type_hierarchy

    subroutine destroy_type_hierarchy(hierarchy)
        type(type_hierarchy_t), intent(inout) :: hierarchy
        type(type_hierarchy_entry_t), allocatable :: temp(:)

        if (allocated(hierarchy%entries)) then
            call move_alloc(hierarchy%entries, temp)
        end if
        hierarchy%count = 0
        hierarchy%capacity = 0
    end subroutine destroy_type_hierarchy

    subroutine hierarchy_ensure_capacity(this, required_capacity)
        class(type_hierarchy_t), intent(inout) :: this
        integer, intent(in) :: required_capacity
        type(type_hierarchy_entry_t), allocatable :: temp(:)
        integer :: new_capacity, i

        if (required_capacity <= this%capacity) return

        new_capacity = max(required_capacity, this%capacity * 2)
        allocate (temp(new_capacity))

        do i = 1, this%count
            if (allocated(this%entries(i)%type_name)) then
                temp(i)%type_name = this%entries(i)%type_name
            end if
            if (allocated(this%entries(i)%parent_name)) then
                temp(i)%parent_name = this%entries(i)%parent_name
            end if
            temp(i)%is_abstract = this%entries(i)%is_abstract
            if (allocated(this%entries(i)%deferred_procedures)) then
                temp(i)%deferred_procedures = this%entries(i)%deferred_procedures
            end if
            temp(i)%depth = this%entries(i)%depth
        end do

        call move_alloc(temp, this%entries)
        this%capacity = new_capacity
    end subroutine hierarchy_ensure_capacity

    subroutine hierarchy_register_type(this, type_name, parent_name, &
            is_abstract, deferred_proc_names, &
            error_msg)
        class(type_hierarchy_t), intent(inout) :: this
        character(len=*), intent(in) :: type_name
        character(len=*), intent(in), optional :: parent_name
        logical, intent(in), optional :: is_abstract
        character(len=*), intent(in), optional :: deferred_proc_names(:)
        character(len=:), allocatable, intent(out), optional :: error_msg

        integer :: idx, parent_idx, parent_depth
        character(len=:), allocatable :: temp_error

        idx = this%find_entry(type_name)
        if (idx > 0) then
            if (present(error_msg)) then
                error_msg = 'Type already registered: '//trim(type_name)
            end if
            return
        end if

        if (present(parent_name)) then
            parent_idx = this%find_entry(parent_name)
            if (parent_idx <= 0) then
                if (present(error_msg)) then
                    error_msg = 'Parent type not found: '//trim(parent_name)
                end if
                return
            end if
            parent_depth = this%entries(parent_idx)%depth
        else
            parent_depth = 0
        end if

        call this%ensure_capacity(this%count + 1)
        this%count = this%count + 1
        idx = this%count

        this%entries(idx)%type_name = trim(type_name)
        if (present(parent_name)) then
            this%entries(idx)%parent_name = trim(parent_name)
        end if
        if (present(is_abstract)) then
            this%entries(idx)%is_abstract = is_abstract
        end if
        if (present(deferred_proc_names)) then
            this%entries(idx)%deferred_procedures = deferred_proc_names
        end if
        this%entries(idx)%depth = parent_depth + 1

        if (present(parent_name)) then
            if (.not. this%validate_no_cycles(type_name, temp_error)) then
                this%count = this%count - 1
                if (present(error_msg)) then
                    error_msg = temp_error
                end if
            end if
        end if
    end subroutine hierarchy_register_type

    function hierarchy_find_entry(this, type_name) result(idx)
        class(type_hierarchy_t), intent(in) :: this
        character(len=*), intent(in) :: type_name
        integer :: idx
        integer :: i
        character(len=:), allocatable :: lower_name, lower_entry

        if (len_trim(type_name) == 0) then
            idx = 0
            return
        end if

        lower_name = to_lower_case(trim(type_name))
        do i = 1, this%count
            if (allocated(this%entries(i)%type_name)) then
                lower_entry = to_lower_case(this%entries(i)%type_name)
                if (lower_name == lower_entry) then
                    idx = i
                    return
                end if
            end if
        end do
        idx = 0
    end function hierarchy_find_entry

    subroutine hierarchy_find_parent(this, type_name, parent_name)
        class(type_hierarchy_t), intent(in) :: this
        character(len=*), intent(in) :: type_name
        character(len=:), allocatable, intent(out) :: parent_name
        integer :: idx

        idx = this%find_entry(type_name)
        if (idx > 0) then
            if (allocated(this%entries(idx)%parent_name)) then
                parent_name = this%entries(idx)%parent_name
            end if
        end if
    end subroutine hierarchy_find_parent

    function hierarchy_is_subtype_of(this, child_type, parent_type) result(is_sub)
        class(type_hierarchy_t), intent(in) :: this
        character(len=*), intent(in) :: child_type, parent_type
        logical :: is_sub
        character(len=:), allocatable :: current_type
        integer :: depth

        is_sub = .false.
        current_type = child_type
        depth = 0

        do while (depth < MAX_CHAIN_DEPTH)
            if (.not. allocated(current_type)) exit
            if (len_trim(current_type) == 0) exit
            if (to_lower_case(current_type) == to_lower_case(parent_type)) then
                is_sub = .true.
                return
            end if
            block
                character(len=:), allocatable :: next_type
                call this%find_parent(current_type, next_type)
                call move_alloc(next_type, current_type)
            end block
            depth = depth + 1
        end do
    end function hierarchy_is_subtype_of

    subroutine hierarchy_get_inheritance_chain(this, type_name, chain)
        class(type_hierarchy_t), intent(in) :: this
        character(len=*), intent(in) :: type_name
        character(len=:), allocatable, intent(out) :: chain(:)
        character(len=:), allocatable :: temp_chain(:)
        character(len=:), allocatable :: current_type
        integer :: depth, idx, i

        depth = this%get_depth(type_name)
        if (depth <= 0) then
            allocate (character(len=0) :: chain(0))
            return
        end if

        allocate (character(len=len(type_name)*2) :: temp_chain(depth))
        current_type = type_name
        idx = 1

        do while (idx <= depth)
            if (.not. allocated(current_type)) exit
            if (len_trim(current_type) == 0) exit
            temp_chain(idx) = current_type
            block
                character(len=:), allocatable :: next_type
                call this%find_parent(current_type, next_type)
                call move_alloc(next_type, current_type)
            end block
            idx = idx + 1
        end do

        allocate (character(len=len(type_name)*2) :: chain(idx - 1))
        do i = 1, idx - 1
            chain(i) = temp_chain(i)
        end do
    end subroutine hierarchy_get_inheritance_chain

    function hierarchy_validate_no_cycles(this, type_name, error_msg) &
            result(is_valid)
        class(type_hierarchy_t), intent(in) :: this
        character(len=*), intent(in) :: type_name
        character(len=:), allocatable, intent(out), optional :: error_msg
        logical :: is_valid
        character(len=:), allocatable :: current_type
        integer :: depth

        is_valid = .true.
        depth = 0
        call this%find_parent(type_name, current_type)

        do while (depth < MAX_CHAIN_DEPTH)
            if (.not. allocated(current_type)) exit
            if (len_trim(current_type) == 0) exit
            if (to_lower_case(current_type) == to_lower_case(type_name)) then
                is_valid = .false.
                if (present(error_msg)) then
                    error_msg = 'Circular inheritance detected for type: '// &
                        trim(type_name)
                end if
                return
            end if
            block
                character(len=:), allocatable :: next_type
                call this%find_parent(current_type, next_type)
                call move_alloc(next_type, current_type)
            end block
            depth = depth + 1
        end do

        if (depth >= MAX_CHAIN_DEPTH) then
            is_valid = .false.
            if (present(error_msg)) then
                error_msg = 'Inheritance chain too deep for type: '// &
                    trim(type_name)
            end if
        end if
    end function hierarchy_validate_no_cycles

    function hierarchy_get_depth(this, type_name) result(depth)
        class(type_hierarchy_t), intent(in) :: this
        character(len=*), intent(in) :: type_name
        integer :: depth
        integer :: idx

        idx = this%find_entry(type_name)
        if (idx > 0) then
            depth = this%entries(idx)%depth
        else
            depth = 0
        end if
    end function hierarchy_get_depth

    function to_lower_case(input) result(output)
        character(len=*), intent(in) :: input
        character(len=:), allocatable :: output
        integer :: i, ic
        character(len=26), parameter :: upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
        character(len=26), parameter :: lower = 'abcdefghijklmnopqrstuvwxyz'

        allocate (character(len=len(input)) :: output)
        output = input

        do i = 1, len(input)
            ic = index(upper, input(i:i))
            if (ic > 0) then
                output(i:i) = lower(ic:ic)
            end if
        end do
    end function to_lower_case

end module type_hierarchy
