module type_system_unified
    ! Unified type system using arena-based storage
    ! Provides compatibility layer for legacy API while using efficient arena storage

    use, intrinsic :: iso_fortran_env, only: error_unit
    use type_system_arena
    use error_handling, only: result_t, create_error_result, &
                              success_result, ERROR_MEMORY
    use identifier_table, only: identifier_table_t, identifier_id_kind, &
                                identifier_table_intern, identifier_table_find
    implicit none
    private

    ! Re-export arena types and constants
    public :: type_arena_t, mono_handle_t, poly_handle_t, args_handle_t
    public :: arena_mono_type_t, arena_poly_type_t
    public :: create_type_arena, destroy_type_arena

    ! Type kind constants (compatible with legacy system)
    integer, parameter, public :: TVAR = 1  ! Type variable
    integer, parameter, public :: TINT = 2  ! Integer type
    integer, parameter, public :: TREAL = 3  ! Real type
    integer, parameter, public :: TCHAR = 4  ! Character type
    integer, parameter, public :: TLOGICAL = 5  ! Logical type
    integer, parameter, public :: TFUN = 6  ! Function type
    integer, parameter, public :: TARRAY = 7  ! Array type
    integer, parameter, public :: TCOMPLEX = 8  ! Complex type
    integer, parameter, public :: TDOUBLE = 9  ! Double precision type
    integer, parameter, public :: TDERIVED = 10  ! Derived/user-defined type

    ! Compatibility layer types (lightweight wrappers around arena handles)
    public :: type_var_t, mono_type_t, poly_type_t, type_env_t, substitution_t
    public :: allocation_info_t

    type :: type_var_t
        integer :: id = 0
        character(len=64) :: name = ""  ! Fixed size to avoid allocatable issues
    contains
        procedure :: assign => type_var_assign
        generic :: assignment(=) => assign
    end type type_var_t

    type :: allocation_info_t
        logical :: is_allocatable = .false.
        logical :: is_pointer = .false.
        logical :: is_target = .false.
        logical :: is_allocated = .false.
        logical :: needs_allocation_check = .false.
        logical :: needs_allocatable_string = .false.
    end type allocation_info_t

    ! Arena-backed types providing legacy API
    type :: mono_type_t
        type(mono_handle_t) :: handle
        type(type_arena_t), pointer :: arena => null()  ! Reference to parent arena
        ! Cached values for compatibility (updated lazily)
        integer :: kind = 0
        type(type_var_t) :: var
        integer :: size = 0
        logical :: is_unsigned = .false.
        type(allocation_info_t) :: alloc_info
    contains
        procedure :: to_string => mono_type_to_string
        procedure :: assign => mono_type_assign
        procedure :: get_kind => mono_type_get_kind
        procedure :: get_size => mono_type_get_size
        procedure :: get_alloc_info => mono_type_get_alloc_info
        procedure :: sync_from_arena => mono_type_sync_from_arena
        procedure :: has_args => mono_type_has_args
        procedure :: get_arg => mono_type_get_arg
        procedure :: get_args_count => mono_type_get_args_count
        generic :: assignment(=) => assign
    end type mono_type_t

    type :: poly_type_t
        type(poly_handle_t) :: handle
        type(type_arena_t), pointer :: arena => null()
        ! Cached mono type for compatibility - simplified version without arena linkage
        integer :: mono_kind = 0
        integer :: mono_size = 0
        type(type_var_t) :: mono_var
        type(allocation_info_t) :: mono_alloc_info
        logical :: mono_is_unsigned = .false.
        logical :: mono_synced = .false.
    contains
        procedure :: assign => poly_type_assign
        procedure :: sync_mono => poly_type_sync_mono
        procedure :: get_mono => poly_type_get_mono
        generic :: assignment(=) => assign
    end type poly_type_t

    ! Maximum sizes for fixed arrays (GCC 15.2.1 compatibility)
    ! Increased for large-scale processing (100K+ lines) - Issue #1046
    integer, parameter :: MAX_SUBST_SIZE = 512
    ! Increase environment capacity to better handle larger inputs (Issue #1046)
    integer, parameter :: MAX_ENV_SIZE = 4096

    type :: substitution_t
        integer :: count = 0
        integer :: capacity = 0
        type(type_var_t), allocatable :: vars(:)
        type(mono_type_t), allocatable :: types(:)
    contains
        procedure :: add => substitution_add
        procedure :: apply => substitution_apply
        procedure :: assign => substitution_assign
        procedure :: ensure_capacity => substitution_ensure_capacity
        generic :: assignment(=) => assign
    end type substitution_t

    type :: type_env_t
        integer :: count = 0
        integer :: capacity = 0
        integer(identifier_id_kind), allocatable :: name_ids(:)
        type(poly_type_t), allocatable :: schemes(:)
        type(identifier_table_t), pointer :: identifiers => null()
        logical :: capacity_exceeded_reported = .false.
        logical :: is_fixed = .false.
    contains
        procedure :: extend => type_env_extend
        procedure :: assign => type_env_assign
        procedure :: ensure_capacity => type_env_ensure_capacity
        generic :: assignment(=) => assign
    end type type_env_t

    ! Global arena for type operations - initialized by create functions
    type(type_arena_t), target, save :: global_arena
    logical, save :: arena_initialized = .false.

    ! Public API functions (compatibility with legacy system)
    public :: create_type_var, create_mono_type, create_poly_type, create_fun_type
    public :: compose_substitutions, occurs_check, free_type_vars
    public :: reset_type_system

    ! Compatibility wrapper functions for type_checker
    public :: type_has_args, type_get_arg, type_get_args_count

    ! Additional compatibility functions for semantic analyzer
    public :: type_args_allocated, type_args_size, type_args_element

contains

    ! Initialize global arena if needed
    subroutine ensure_arena_initialized()
        if (.not. arena_initialized) then
            call init_type_arena(global_arena, 65536)
            arena_initialized = .true.
        end if
    end subroutine ensure_arena_initialized

    ! Reset type system arena to prevent accumulation across transformations
    ! CRITICAL: Must be called at the start of each transformation to prevent
    ! type IDs from accumulating and causing circular references or slowdowns
    subroutine reset_type_system()
        if (arena_initialized) then
            call global_arena%reset()
        end if
    end subroutine reset_type_system

    ! Create type variable (compatibility function)
    function create_type_var(id, name) result(tv)
        integer, intent(in) :: id
        character(len=*), intent(in), optional :: name
        type(type_var_t) :: tv

        tv%id = id
        if (present(name)) then
            tv%name = name
        else
            write (tv%name, '("t", I0)') id
        end if
    end function create_type_var

    ! Create monomorphic type (compatibility function)
    function create_mono_type(kind, var, args, char_size, array_size, is_unsigned) &
        result(mt)
        integer, intent(in) :: kind
        type(type_var_t), intent(in), optional :: var
        type(mono_type_t), intent(in), optional :: args(:)
        integer, intent(in), optional :: char_size, array_size
        logical, intent(in), optional :: is_unsigned
        type(mono_type_t) :: mt

        type(arena_mono_type_t) :: arena_type
        type(args_handle_t) :: args_handle
        type(mono_handle_t), allocatable :: arg_handles(:)
        integer :: i

        call ensure_arena_initialized()

        ! Build arena type
        arena_type = arena_mono_type_t()
        arena_type%kind = kind
        arena_type%size = 0
        if (present(char_size)) arena_type%size = char_size
        if (present(array_size)) arena_type%size = array_size
        if (present(is_unsigned)) arena_type%is_unsigned = is_unsigned

        if (present(var)) then
            arena_type%var_id = var%id
            arena_type%var_name = var%name
        end if

        ! Handle arguments for function and array types
        if (present(args)) then
            allocate (arg_handles(size(args)))
            do i = 1, size(args)
                arg_handles(i) = args(i)%handle
            end do
            args_handle = store_type_args(global_arena, arg_handles)
            arena_type%args = args_handle
        else
            arena_type%args = null_args_handle()
        end if

        ! Store in arena
        mt%handle = store_mono_type(global_arena, arena_type)
        mt%arena => global_arena

        ! Cache values for immediate access
        mt%kind = kind
        mt%size = arena_type%size
        mt%is_unsigned = arena_type%is_unsigned
        if (present(var)) mt%var = var

        ! Set allocation info if needed
        mt%alloc_info%is_allocatable = arena_type%is_allocatable
        mt%alloc_info%is_pointer = arena_type%is_pointer
        mt%alloc_info%is_target = arena_type%is_target
    end function create_mono_type

    ! Create polymorphic type (compatibility function)
    function create_poly_type(forall_vars, mono) result(pt)
        type(type_var_t), intent(in) :: forall_vars(:)
        type(mono_type_t), intent(in) :: mono
        type(poly_type_t) :: pt

        type(arena_poly_type_t) :: arena_poly
        type(args_handle_t) :: vars_handle
        type(mono_handle_t), allocatable :: var_handles(:)
        integer :: i

        call ensure_arena_initialized()

        arena_poly = arena_poly_type_t()

        ! Convert type vars to handles (simplified - we'll store as empty for now)
        allocate (var_handles(0))
        ! Empty for now - full implementation would convert forall_vars
        vars_handle = store_type_args(global_arena, var_handles)

        arena_poly%forall_vars = vars_handle
        arena_poly%mono = mono%handle

        pt%handle = store_poly_type(global_arena, arena_poly)
        pt%arena => global_arena

        ! Initialize cached mono type fields
        pt%mono_kind = mono%kind
        pt%mono_size = mono%size
        pt%mono_var = mono%var
        pt%mono_alloc_info = mono%alloc_info
        pt%mono_is_unsigned = mono%is_unsigned
        pt%mono_synced = .true.
    end function create_poly_type

    ! Create function type (compatibility function)
    function create_fun_type(arg_type, result_type) result(fun_type)
        type(mono_type_t), intent(in) :: arg_type, result_type
        type(mono_type_t) :: fun_type

        type(mono_type_t) :: args(2)

        args(1) = arg_type
        args(2) = result_type
        fun_type = create_mono_type(TFUN, args=args)
    end function create_fun_type

    ! Compose substitutions (stub for now)
    function compose_substitutions(s1, s2) result(composed)
        type(substitution_t), intent(in) :: s1, s2
        type(substitution_t) :: composed

        integer :: i
        type(mono_type_t) :: applied

        composed%count = 0
        composed%capacity = 0

        if (s1%count + s2%count > 0) then
            call composed%ensure_capacity(s1%count + s2%count)
        end if

        do i = 1, s2%count
            call s1%apply(s2%types(i), applied)
            call composed%add(s2%vars(i), applied)
        end do

        do i = 1, s1%count
            if (substitution_find_var(composed, s1%vars(i)%id) == 0) then
                call composed%add(s1%vars(i), s1%types(i))
            end if
        end do
    end function compose_substitutions

    ! Occurs check - detect whether a type variable appears within a type.
    function occurs_check(var, typ) result(occurs)
        type(type_var_t), intent(in) :: var
        type(mono_type_t), intent(in) :: typ
        logical :: occurs

        type(mono_type_t), allocatable :: stack(:)
        integer, allocatable :: seen(:)
        integer :: top
        integer :: seen_count
        integer :: i
        integer :: type_id
        type(mono_type_t) :: current

        occurs = .false.
        if (var%id <= 0) return
        if (typ%kind == 0) return

        allocate (stack(16))
        allocate (seen(32))
        top = 1
        stack(top) = typ
        seen_count = 0

        do while (top > 0)
            current = stack(top)
            top = top - 1

            if (current%kind == TVAR) then
                if (current%var%id == var%id) then
                    occurs = .true.
                    exit
                end if
                cycle
            end if

            type_id = current%handle%type_id
            if (type_id > 0) then
                if (seen_count > 0) then
                    if (any(seen(1:seen_count) == type_id)) cycle
                end if
                if (seen_count >= size(seen)) then
                    call grow_seen_array(seen)
                end if
                seen_count = seen_count + 1
                seen(seen_count) = type_id
            end if

            if (.not. current%has_args()) cycle
            do i = 1, current%get_args_count()
                if (top >= size(stack)) then
                    call grow_type_stack(stack)
                end if
                top = top + 1
                stack(top) = current%get_arg(i)
            end do
        end do
    end function occurs_check

    ! Collect free type variables appearing within a type (uniqued by ID).
    subroutine free_type_vars(typ, vars)
        type(mono_type_t), intent(in) :: typ
        type(type_var_t), allocatable, intent(out) :: vars(:)

        type(type_var_t), allocatable :: found(:)
        type(mono_type_t), allocatable :: stack(:)
        integer, allocatable :: seen(:)
        integer :: top
        integer :: found_count
        integer :: seen_count
        integer :: i
        integer :: j
        integer :: type_id
        logical :: already_present
        type(mono_type_t) :: current

        if (allocated(vars)) deallocate (vars)
        if (typ%kind == 0) then
            allocate (vars(0))
            return
        end if

        allocate (found(16))
        allocate (stack(16))
        allocate (seen(32))
        top = 1
        stack(top) = typ
        found_count = 0
        seen_count = 0

        do while (top > 0)
            current = stack(top)
            top = top - 1

            if (current%kind == TVAR) then
                if (current%var%id > 0) then
                    already_present = .false.
                    do j = 1, found_count
                        if (found(j)%id == current%var%id) then
                            already_present = .true.
                            exit
                        end if
                    end do
                    if (.not. already_present) then
                        if (found_count >= size(found)) then
                            call grow_type_var_array(found)
                        end if
                        found_count = found_count + 1
                        found(found_count) = current%var
                    end if
                end if
                cycle
            end if

            type_id = current%handle%type_id
            if (type_id > 0) then
                if (seen_count > 0) then
                    if (any(seen(1:seen_count) == type_id)) cycle
                end if
                if (seen_count >= size(seen)) then
                    call grow_seen_array(seen)
                end if
                seen_count = seen_count + 1
                seen(seen_count) = type_id
            end if

            if (.not. current%has_args()) cycle
            do i = 1, current%get_args_count()
                if (top >= size(stack)) then
                    call grow_type_stack(stack)
                end if
                top = top + 1
                stack(top) = current%get_arg(i)
            end do
        end do

        allocate (vars(found_count))
        if (found_count > 0) vars = found(1:found_count)
    end subroutine free_type_vars

    ! Type variable assignment
    subroutine type_var_assign(lhs, rhs)
        class(type_var_t), intent(out) :: lhs
        type(type_var_t), intent(in) :: rhs

        lhs%id = rhs%id
        lhs%name = rhs%name
    end subroutine type_var_assign

    ! Mono type assignment
    subroutine mono_type_assign(lhs, rhs)
        class(mono_type_t), intent(out) :: lhs
        type(mono_type_t), intent(in) :: rhs

        lhs%handle = rhs%handle
        lhs%arena => rhs%arena

        ! Copy cached values
        lhs%kind = rhs%kind
        lhs%var = rhs%var
        lhs%size = rhs%size
        lhs%is_unsigned = rhs%is_unsigned
        lhs%alloc_info = rhs%alloc_info
    end subroutine mono_type_assign

    ! Poly type assignment
    subroutine poly_type_assign(lhs, rhs)
        class(poly_type_t), intent(out) :: lhs
        type(poly_type_t), intent(in) :: rhs

        lhs%handle = rhs%handle
        lhs%arena => rhs%arena
        lhs%mono_kind = rhs%mono_kind
        lhs%mono_size = rhs%mono_size
        lhs%mono_var = rhs%mono_var
        lhs%mono_alloc_info = rhs%mono_alloc_info
        lhs%mono_is_unsigned = rhs%mono_is_unsigned
        lhs%mono_synced = rhs%mono_synced
    end subroutine poly_type_assign

    ! Substitution assignment
    subroutine substitution_assign(lhs, rhs)
        class(substitution_t), intent(out) :: lhs
        type(substitution_t), intent(in) :: rhs
        integer :: i

        lhs%count = rhs%count
        lhs%capacity = rhs%capacity
        if (lhs%capacity < lhs%count) lhs%capacity = lhs%count
        if (allocated(lhs%vars)) deallocate (lhs%vars)
        if (allocated(lhs%types)) deallocate (lhs%types)
        if (lhs%capacity > 0) then
            allocate (lhs%vars(lhs%capacity))
            allocate (lhs%types(lhs%capacity))
        end if

        ! Copy only used elements for efficiency
        if (rhs%count > 0) then
            do i = 1, rhs%count
                lhs%vars(i) = rhs%vars(i)
                lhs%types(i) = rhs%types(i)
            end do
        end if
    end subroutine substitution_assign

    ! Type environment assignment
    subroutine type_env_assign(lhs, rhs)
        class(type_env_t), intent(out) :: lhs
        type(type_env_t), intent(in) :: rhs
        integer :: i

        lhs%count = rhs%count
        lhs%capacity = rhs%capacity
        lhs%capacity_exceeded_reported = rhs%capacity_exceeded_reported
        lhs%is_fixed = rhs%is_fixed
        if (lhs%capacity <= 0) then
            ! Try to infer capacity from source arrays if available
            if (allocated(rhs%name_ids)) lhs%capacity = size(rhs%name_ids)
            if (lhs%capacity <= 0) lhs%capacity = lhs%count
        end if
        if (lhs%capacity < lhs%count) lhs%capacity = lhs%count
        if (allocated(lhs%name_ids)) deallocate (lhs%name_ids)
        if (allocated(lhs%schemes)) deallocate (lhs%schemes)
        if (lhs%capacity > 0) then
            allocate (lhs%name_ids(lhs%capacity))
            allocate (lhs%schemes(lhs%capacity))
        end if

        if (associated(rhs%identifiers)) then
            lhs%identifiers => rhs%identifiers
        else
            nullify (lhs%identifiers)
        end if

        ! Copy only used elements
        if (rhs%count > 0) then
            do i = 1, rhs%count
                lhs%name_ids(i) = rhs%name_ids(i)
                lhs%schemes(i) = rhs%schemes(i)
            end do
        end if
    end subroutine type_env_assign

    ! Mono type helper functions
    function mono_type_to_string(this) result(str)
        class(mono_type_t), intent(in) :: this
        character(len=64) :: str

        type(arena_mono_type_t) :: arena_type

        if (.not. is_valid_mono_handle(this%handle) .or. .not. &
            & associated(this%arena)) then
            str = "invalid_type"
            return
        end if

        arena_type = get_mono_type(this%arena, this%handle)

        select case (arena_type%kind)
        case (TVAR)
            write (str, '("''", A)') trim(arena_type%var_name)
        case (TINT)
            if (arena_type%is_unsigned) then
                str = "unsigned_integer"
            else
                str = "integer"
            end if
        case (TREAL)
            str = "real"
        case (TCHAR)
            if (arena_type%size > 0) then
                write (str, '("character(len=", I0, ")")') arena_type%size
            else
                str = "character"
            end if
        case (TLOGICAL)
            str = "logical"
        case (TCOMPLEX)
            str = "complex"
        case (TFUN)
            str = "function"  ! Simplified
        case (TARRAY)
            str = "array"  ! Simplified
        case default
            str = "unknown"
        end select
    end function mono_type_to_string

    function mono_type_get_kind(this) result(kind)
        class(mono_type_t), intent(in) :: this
        integer :: kind

        ! Use cached value for performance
        kind = this%kind
    end function mono_type_get_kind

    function mono_type_get_size(this) result(size)
        class(mono_type_t), intent(in) :: this
        integer :: size

        ! Use cached value for performance
        size = this%size
    end function mono_type_get_size

    function mono_type_get_alloc_info(this) result(alloc_info)
        class(mono_type_t), intent(in) :: this
        type(allocation_info_t) :: alloc_info

        ! Use cached value for performance
        alloc_info = this%alloc_info
    end function mono_type_get_alloc_info

    ! Sync cached values from arena (called when needed)
    subroutine mono_type_sync_from_arena(this)
        class(mono_type_t), intent(inout) :: this
        type(arena_mono_type_t) :: arena_type

        if (.not. is_valid_mono_handle(this%handle) .or. .not. &
            & associated(this%arena)) return

        arena_type = get_mono_type(this%arena, this%handle)
        this%kind = arena_type%kind
        this%size = arena_type%size
        this%var%id = arena_type%var_id
        this%var%name = arena_type%var_name
        this%is_unsigned = arena_type%is_unsigned
        this%alloc_info%is_allocatable = arena_type%is_allocatable
        this%alloc_info%is_pointer = arena_type%is_pointer
        this%alloc_info%is_target = arena_type%is_target
    end subroutine mono_type_sync_from_arena

    ! Check if mono type has arguments
    function mono_type_has_args(this) result(has_args)
        class(mono_type_t), intent(in) :: this
        logical :: has_args

        type(arena_mono_type_t) :: arena_type

        has_args = .false.
        if (.not. is_valid_mono_handle(this%handle) .or. .not. &
            & associated(this%arena)) return

        arena_type = get_mono_type(this%arena, this%handle)
        has_args = is_valid_args_handle(arena_type%args) .and. arena_type%args%count > 0
    end function mono_type_has_args

    ! Get argument count
    function mono_type_get_args_count(this) result(count)
        class(mono_type_t), intent(in) :: this
        integer :: count

        type(arena_mono_type_t) :: arena_type

        count = 0
        if (.not. is_valid_mono_handle(this%handle) .or. .not. &
            & associated(this%arena)) return

        arena_type = get_mono_type(this%arena, this%handle)
        if (is_valid_args_handle(arena_type%args)) then
            count = arena_type%args%count
        end if
    end function mono_type_get_args_count

    ! Get specific argument by index
    function mono_type_get_arg(this, index) result(arg_type)
        class(mono_type_t), intent(in) :: this
        integer, intent(in) :: index
        type(mono_type_t) :: arg_type

        type(arena_mono_type_t) :: arena_type
        type(mono_handle_t), allocatable :: arg_handles(:)

        ! Return invalid type if out of bounds or invalid
        arg_type%kind = 0
        if (.not. is_valid_mono_handle(this%handle) .or. .not. &
            & associated(this%arena)) return

        arena_type = get_mono_type(this%arena, this%handle)
        if (.not. is_valid_args_handle(arena_type%args)) return
        if (index < 1 .or. index > arena_type%args%count) return

        ! Get the arguments array
        call get_type_args(this%arena, arena_type%args, arg_handles)
        if (allocated(arg_handles)) then
            if (size(arg_handles) >= index) then
                arg_type%handle = arg_handles(index)
                arg_type%arena => this%arena
                call arg_type%sync_from_arena()
            end if
        end if
    end function mono_type_get_arg

    ! Substitution helper functions
    subroutine substitution_add(this, var, typ)
        class(substitution_t), intent(inout) :: this
        type(type_var_t), intent(in) :: var
        type(mono_type_t), intent(in) :: typ

        ! Initialize capacity lazily
        if (this%capacity == 0) then
            this%capacity = 64
            allocate (this%vars(this%capacity))
            allocate (this%types(this%capacity))
        end if

        ! Grow if needed
        if (this%count >= this%capacity) then
            call substitution_ensure_capacity(this, this%count + 1)
        end if

        this%count = this%count + 1
        this%vars(this%count) = var
        this%types(this%count) = typ
    end subroutine substitution_add

    subroutine substitution_ensure_capacity(this, required)
        class(substitution_t), intent(inout) :: this
        integer, intent(in) :: required
        integer :: new_capacity, i

        if (this%capacity == 0) then
            this%capacity = max(64, required)
            allocate (this%vars(this%capacity))
            allocate (this%types(this%capacity))
            return
        end if

        if (required > this%capacity) then
            new_capacity = max(this%capacity * 2, required)
            ! Reallocate and copy used elements only
            block
                type(type_var_t), allocatable :: new_vars(:)
                type(mono_type_t), allocatable :: new_types(:)
                allocate (new_vars(new_capacity))
                allocate (new_types(new_capacity))
                if (this%count > 0) then
                    do i = 1, this%count
                        new_vars(i) = this%vars(i)
                        new_types(i) = this%types(i)
                    end do
                end if
                call move_alloc(new_vars, this%vars)
                call move_alloc(new_types, this%types)
            end block
            this%capacity = new_capacity
        end if
    end subroutine substitution_ensure_capacity

    subroutine substitution_apply(this, input, output)
        class(substitution_t), intent(in) :: this
        type(mono_type_t), intent(in) :: input
        type(mono_type_t), intent(out) :: output

        type(mono_type_t), allocatable :: args(:)
        type(mono_handle_t), allocatable :: arg_handles(:)
        type(mono_type_t) :: arg_in
        type(mono_type_t) :: arg_out
        type(arena_mono_type_t) :: arena_type
        integer :: idx
        integer :: i
        logical :: changed

        output = input
        if (this%count <= 0) return
        if (input%kind == 0) return

        if (input%kind == TVAR) then
            idx = substitution_find_var(this, input%var%id)
            if (idx > 0) output = this%types(idx)
            return
        end if

        if (.not. input%has_args()) return

        allocate (args(input%get_args_count()))
        changed = .false.
        do i = 1, size(args)
            arg_in = input%get_arg(i)
            call this%apply(arg_in, arg_out)
            args(i) = arg_out
            if (arg_out%handle%type_id /= arg_in%handle%type_id) then
                changed = .true.
            end if
        end do
        if (.not. changed) return

        if (.not. associated(input%arena)) return
        arena_type = get_mono_type(input%arena, input%handle)
        allocate (arg_handles(size(args)))
        do i = 1, size(args)
            arg_handles(i) = args(i)%handle
        end do
        call ensure_arena_initialized()
        arena_type%args = store_type_args(global_arena, arg_handles)
        output%handle = store_mono_type(global_arena, arena_type)
        output%arena => global_arena
        call output%sync_from_arena()
    end subroutine substitution_apply

    integer function substitution_find_var(subst, var_id) result(index)
        type(substitution_t), intent(in) :: subst
        integer, intent(in) :: var_id
        integer :: i

        index = 0
        if (var_id <= 0) return
        do i = 1, subst%count
            if (subst%vars(i)%id == var_id) then
                index = i
                return
            end if
        end do
    end function substitution_find_var

    subroutine grow_type_stack(stack)
        type(mono_type_t), allocatable, intent(inout) :: stack(:)
        type(mono_type_t), allocatable :: tmp(:)
        integer :: new_size

        new_size = max(2 * size(stack), 16)
        allocate (tmp(new_size))
        tmp(1:size(stack)) = stack
        call move_alloc(tmp, stack)
    end subroutine grow_type_stack

    subroutine grow_seen_array(seen)
        integer, allocatable, intent(inout) :: seen(:)
        integer, allocatable :: tmp(:)
        integer :: new_size

        new_size = max(2 * size(seen), 32)
        allocate (tmp(new_size))
        tmp(1:size(seen)) = seen
        call move_alloc(tmp, seen)
    end subroutine grow_seen_array

    subroutine grow_type_var_array(vars)
        type(type_var_t), allocatable, intent(inout) :: vars(:)
        type(type_var_t), allocatable :: tmp(:)
        integer :: new_size

        new_size = max(2 * size(vars), 16)
        allocate (tmp(new_size))
        tmp(1:size(vars)) = vars
        call move_alloc(tmp, vars)
    end subroutine grow_type_var_array

    ! Type environment helper functions
    subroutine type_env_extend(this, name, scheme)
        class(type_env_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        type(poly_type_t), intent(in) :: scheme
        integer(identifier_id_kind) :: name_id

        if (.not. associated(this%identifiers)) then
            write (error_unit, '(A)') &
                'ERROR: type_env_t missing identifier table; skipping insert'
            return
        end if

        ! Initialize capacity lazily. If user set capacity manually use it.
        if (.not. allocated(this%name_ids) .or. .not. allocated(this%schemes)) then
            if (this%capacity > 0) then
                this%is_fixed = .true.
            else
                this%capacity = 64
                this%is_fixed = .false.
            end if
            allocate (this%name_ids(this%capacity))
            allocate (this%schemes(this%capacity))
        else
            ! Handle pathological zero-sized allocations defensively
            if (size(this%name_ids) == 0 .or. size(this%schemes) == 0) then
                if (this%capacity <= 0) this%capacity = 64
                call type_env_ensure_capacity(this, max(1, this%count + 1))
            end if
        end if

        ! Capacity guard: do not exceed declared capacity
        if (this%count >= this%capacity) then
            if (this%is_fixed) then
                if (.not. this%capacity_exceeded_reported) then
                    write (error_unit, *) &
                        'ERROR: Type environment capacity exceeded (', &
                        this%capacity, ')'
                    write (error_unit, *) 'Consider increasing MAX_ENV_SIZE parameter'
                    this%capacity_exceeded_reported = .true.
                end if
                return
            else
                ! Grow dynamically for non-fixed environments
                call type_env_ensure_capacity(this, this%count + 1)
            end if
        end if

        ! Add binding
        name_id = identifier_table_intern(this%identifiers, name)

        ! Replace existing entry if name already present
        if (this%count > 0) then
            block
                integer :: idx
                do idx = 1, this%count
                    if (this%name_ids(idx) == name_id) then
                        this%schemes(idx) = scheme
                        return
                    end if
                end do
            end block
        end if

        this%count = this%count + 1
        this%name_ids(this%count) = name_id
        this%schemes(this%count) = scheme
    end subroutine type_env_extend

    subroutine type_env_ensure_capacity(this, required)
        class(type_env_t), intent(inout) :: this
        integer, intent(in) :: required
        ! Ensure arrays exist; if not fixed, grow to meet required
        if (.not. allocated(this%name_ids) .or. .not. allocated(this%schemes)) then
            if (this%capacity <= 0) then
                this%capacity = max(64, required)
                this%is_fixed = .false.
            end if
            allocate (this%name_ids(this%capacity))
            allocate (this%schemes(this%capacity))
        else if (size(this%name_ids) == 0 .or. size(this%schemes) == 0) then
            ! Repair zero-sized arrays
            if (this%capacity <= 0) this%capacity = max(64, required)
            block
                integer :: new_capacity
                integer(identifier_id_kind), allocatable :: new_ids(:)
                type(poly_type_t), allocatable :: new_schemes(:)
                new_capacity = max(this%capacity, required)
                allocate (new_ids(new_capacity))
                allocate (new_schemes(new_capacity))
                call move_alloc(new_ids, this%name_ids)
                call move_alloc(new_schemes, this%schemes)
                this%capacity = new_capacity
            end block
        else if (.not. this%is_fixed .and. required > this%capacity) then
            ! Grow dynamically
            block
                integer :: new_capacity, i
                integer(identifier_id_kind), allocatable :: new_ids(:)
                type(poly_type_t), allocatable :: new_schemes(:)
                new_capacity = max(this%capacity * 2, required)
                allocate (new_ids(new_capacity))
                allocate (new_schemes(new_capacity))
                if (this%count > 0) then
                    do i = 1, this%count
                        new_ids(i) = this%name_ids(i)
                        new_schemes(i) = this%schemes(i)
                    end do
                end if
                call move_alloc(new_ids, this%name_ids)
                call move_alloc(new_schemes, this%schemes)
                this%capacity = new_capacity
            end block
        end if
    end subroutine type_env_ensure_capacity

    ! Public wrapper functions for compatibility with type_checker
    function type_has_args(typ) result(has_args)
        type(mono_type_t), intent(in) :: typ
        logical :: has_args
        has_args = typ%has_args()
    end function type_has_args

    function type_get_args_count(typ) result(count)
        type(mono_type_t), intent(in) :: typ
        integer :: count
        count = typ%get_args_count()
    end function type_get_args_count

    function type_get_arg(typ, index) result(arg_type)
        type(mono_type_t), intent(in) :: typ
        integer, intent(in) :: index
        type(mono_type_t) :: arg_type
        arg_type = typ%get_arg(index)
    end function type_get_arg

    ! Additional semantic analyzer compatibility functions
    function type_args_allocated(typ) result(allocated)
        type(mono_type_t), intent(in) :: typ
        logical :: allocated
        allocated = typ%has_args()
    end function type_args_allocated

    function type_args_size(typ) result(size)
        type(mono_type_t), intent(in) :: typ
        integer :: size
        size = typ%get_args_count()
    end function type_args_size

    function type_args_element(typ, index) result(element_type)
        type(mono_type_t), intent(in) :: typ
        integer, intent(in) :: index
        type(mono_type_t) :: element_type
        element_type = typ%get_arg(index)
    end function type_args_element

    ! Poly type mono sync function
    subroutine poly_type_sync_mono(this)
        class(poly_type_t), intent(inout) :: this
        type(arena_poly_type_t) :: arena_poly
        type(arena_mono_type_t) :: arena_mono

        if (.not. is_valid_poly_handle(this%handle) .or. .not. &
            & associated(this%arena)) return
        if (this%mono_synced) return

        arena_poly = get_poly_type(this%arena, this%handle)
        if (is_valid_mono_handle(arena_poly%mono)) then
            arena_mono = get_mono_type(this%arena, arena_poly%mono)
            this%mono_kind = arena_mono%kind
            this%mono_size = arena_mono%size
            this%mono_var%id = arena_mono%var_id
            this%mono_var%name = arena_mono%var_name
            this%mono_alloc_info%is_allocatable = arena_mono%is_allocatable
            this%mono_alloc_info%is_pointer = arena_mono%is_pointer
            this%mono_alloc_info%is_target = arena_mono%is_target
            this%mono_is_unsigned = arena_mono%is_unsigned
        end if
        this%mono_synced = .true.
    end subroutine poly_type_sync_mono

    ! Get mono type from poly type
    function poly_type_get_mono(this) result(mono)
        class(poly_type_t), intent(inout) :: this
        type(mono_type_t) :: mono

        if (is_valid_poly_handle(this%handle) .and. associated(this%arena)) then
            block
                type(arena_poly_type_t) :: arena_poly

                arena_poly = get_poly_type(this%arena, this%handle)
                if (is_valid_mono_handle(arena_poly%mono)) then
                    mono%handle = arena_poly%mono
                    mono%arena => this%arena
                    call mono%sync_from_arena()
                    return
                end if
            end block
        end if

        if (.not. this%mono_synced) then
            call this%sync_mono()
        end if

        mono%kind = this%mono_kind
        mono%size = this%mono_size
        mono%var = this%mono_var
        mono%is_unsigned = this%mono_is_unsigned
        mono%alloc_info = this%mono_alloc_info
    end function poly_type_get_mono

end module type_system_unified
