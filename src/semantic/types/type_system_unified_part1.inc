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

        type(mono_type_t) :: var_type

        call ensure_arena_initialized()

        arena_poly = arena_poly_type_t()

        ! Convert each quantified type variable to a TVAR mono handle so the
        ! forall set is preserved in the arena and recoverable via
        ! get_poly_forall_vars.
        allocate (var_handles(size(forall_vars)))
        do i = 1, size(forall_vars)
            var_type = create_mono_type(TVAR, var=forall_vars(i))
            var_handles(i) = var_type%handle
        end do
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

    ! Recover the quantified type variables stored in a poly type. Returns a
    ! zero-length array when the scheme quantifies no variables.
    function get_poly_forall_vars(pt) result(vars)
        type(poly_type_t), intent(in) :: pt
        type(type_var_t), allocatable :: vars(:)

        type(arena_poly_type_t) :: arena_poly
        type(arena_mono_type_t) :: arena_mono
        type(mono_handle_t), allocatable :: var_handles(:)
        integer :: i

        if (.not. is_valid_poly_handle(pt%handle) .or. .not. associated(pt%arena)) &
            then
            allocate (vars(0))
            return
        end if

        arena_poly = get_poly_type(pt%arena, pt%handle)
        call get_type_args(pt%arena, arena_poly%forall_vars, var_handles)
        if (.not. allocated(var_handles)) then
            allocate (vars(0))
            return
        end if

        allocate (vars(size(var_handles)))
        do i = 1, size(var_handles)
            arena_mono = get_mono_type(pt%arena, var_handles(i))
            vars(i)%id = arena_mono%var_id
            vars(i)%name = arena_mono%var_name
        end do
    end function get_poly_forall_vars

    ! Create function type (compatibility function)
    function create_fun_type(arg_type, result_type) result(fun_type)
        type(mono_type_t), intent(in) :: arg_type, result_type
        type(mono_type_t) :: fun_type

        type(mono_type_t) :: args(2)

        args(1) = arg_type
        args(2) = result_type
        fun_type = create_mono_type(TFUN, args=args)
    end function create_fun_type

    ! (s1 . s2)(x) = s1(s2(x)): map every var in s2 to s1 applied to its
    ! image, then carry over any s1-only bindings.
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
