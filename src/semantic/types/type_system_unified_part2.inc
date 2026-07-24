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
            str = "function" ! Simplified
        case (TARRAY)
            str = "array" ! Simplified
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

    ! Public wrapper functions for type argument access
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
