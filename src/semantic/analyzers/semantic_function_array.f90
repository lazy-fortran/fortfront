module semantic_function_array
    ! Function call and array type inference
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use type_system_unified, only: type_var_t, mono_type_t, poly_type_t, &
                                   create_mono_type, create_poly_type, &
                                   TVAR, TINT, TREAL, TCHAR, TLOGICAL, TCOMPLEX, &
                                   TDOUBLE, TFUN, TARRAY, type_args_allocated, &
                                   type_args_size, type_args_element
    use scope_manager, only: scope_stack_t
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: call_or_subscript_node, array_literal_node
    use ast_nodes_loops, only: do_loop_node
    use ast_nodes_procedure, only: function_def_node
    use ast_nodes_data, only: declaration_node
    use ast_nodes_bounds, only: array_slice_node, range_expression_node, &
                                array_bounds_node
    use intrinsic_registry, only: get_intrinsic_signature, is_intrinsic_function
    use semantic_validation_utils, only: int_to_str
    use string_utils_mod, only: to_lower
    use semantic_array_type_builders, only: collapse_array_rank
    use semantic_type_operations, only: get_common_type
    implicit none
    private

    public :: infer_function_call_type
    public :: infer_array_slice_type
    public :: infer_array_literal_type
    public :: find_return_type

contains

    function infer_function_call_type(arena, call_node, scopes, get_type_fn) &
        result(typ)
        type(ast_arena_t), intent(inout) :: arena
        type(call_or_subscript_node), intent(inout) :: call_node
        type(scope_stack_t), intent(inout) :: scopes
        interface
            function get_type_fn(a, idx) result(t)
                import :: mono_type_t, ast_arena_t
                type(ast_arena_t), intent(inout) :: a
                integer, intent(in) :: idx
                type(mono_type_t) :: t
            end function get_type_fn
        end interface
        type(mono_type_t) :: typ
        type(poly_type_t), allocatable :: scheme
        type(mono_type_t) :: arg_type
        type(mono_type_t) :: scheme_mono
        type(mono_type_t) :: base_array_type
        character(len=:), allocatable :: intrinsic_sig
        character(len=:), allocatable :: lowered_name
        integer :: i
        integer :: subscript_rank
        logical :: is_intrinsic_func
        logical :: has_function_scheme
        logical :: treat_as_array_access
        type(mono_type_t), allocatable :: arg_types(:)
        integer :: deduced_kind

        typ = create_mono_type(TREAL)
        is_intrinsic_func = .false.
        has_function_scheme = .false.
        treat_as_array_access = .false.

        if (allocated(call_node%arg_indices)) then
            allocate (arg_types(size(call_node%arg_indices)))
            do i = 1, size(call_node%arg_indices)
                arg_type = get_type_fn(arena, call_node%arg_indices(i))
                arg_types(i) = arg_type
            end do
        else
            allocate (arg_types(0))
        end if

        if (allocated(call_node%name)) then
            lowered_name = to_lower(trim(call_node%name))
        else
            lowered_name = ""
        end if

        if (allocated(call_node%name)) then
            call scopes%lookup(call_node%name, scheme)
        end if

        if (allocated(scheme)) then
            scheme_mono = scheme%get_mono()
            has_function_scheme = (scheme_mono%kind == TFUN)
            typ = scheme_mono
            if (has_function_scheme .and. type_args_allocated(typ) .and. &
                type_args_size(typ) >= 2) then
                typ = type_args_element(typ, 2)
            end if
        else if (allocated(call_node%name)) then
            if (find_return_type(arena, call_node%name, typ)) then
                has_function_scheme = .true.
            else
                is_intrinsic_func = is_intrinsic_function(call_node%name)

                if (is_intrinsic_func) then
                    intrinsic_sig = get_intrinsic_signature(call_node%name)

                    if (len_trim(intrinsic_sig) > 0) then
                        if (index(intrinsic_sig, "real(") == 1) then
                            typ = create_mono_type(TREAL)
                        else if (index(intrinsic_sig, "integer(") == 1) then
                            typ = create_mono_type(TINT)
                        else if (index(intrinsic_sig, "logical(") == 1) then
                            typ = create_mono_type(TLOGICAL)
                        else if (index(intrinsic_sig, "character(") == 1) then
                            typ = create_mono_type(TCHAR)
                        else if (index(intrinsic_sig, "array(") == 1) then
                            typ = infer_array_intrinsic_type(arena, call_node, &
                                                             get_type_fn)
                        else
                            typ = create_mono_type(TREAL)
                        end if
                    else
                        typ = create_mono_type(TREAL)
                    end if
                else
                    typ = create_mono_type(TREAL)
                end if
            end if
        else
            typ = create_mono_type(TREAL)
        end if

        if (is_intrinsic_func) then
            call refine_character_intrinsic_result(lowered_name, arg_types, typ)
        end if

        subscript_rank = 0
        if (allocated(call_node%arg_indices)) subscript_rank = &
            size(call_node%arg_indices)

        treat_as_array_access = call_node%is_array_access
        if (.not. treat_as_array_access) then
            treat_as_array_access = subscript_rank > 0 .and. &
                                    .not. has_function_scheme .and. &
                                    .not. is_intrinsic_func
        end if

        if (treat_as_array_access .and. typ%kind == TARRAY) then
            base_array_type = collapse_array_rank(typ, subscript_rank)
            if (base_array_type%kind == 0) base_array_type = typ
            typ = base_array_type
            call_node%is_array_access = .true.
        end if

        if (allocated(arg_types)) then
            deduced_kind = deduce_return_kind_from_args(arg_types)
            if (deduced_kind > 0) then
                select case (typ%kind)
                case (TVAR)
                    typ = create_mono_type(deduced_kind)
                case (TREAL)
                    if (deduced_kind /= TREAL) typ = create_mono_type(deduced_kind)
                case (TINT)
                    if (deduced_kind /= TINT) typ = create_mono_type(deduced_kind)
                case default
                    if (typ%kind <= 0) typ = create_mono_type(deduced_kind)
                end select
            end if
        end if
    end function infer_function_call_type

    subroutine refine_character_intrinsic_result(name, arg_types, typ)
        character(len=*), intent(in) :: name
        type(mono_type_t), intent(in) :: arg_types(:)
        type(mono_type_t), intent(inout) :: typ
        type(mono_type_t) :: arg_copy
        integer :: arg_len

        if (name /= "trim" .and. name /= "adjustl" .and. name /= "adjustr") return
        if (size(arg_types) <= 0) return

        arg_copy = arg_types(1)
        call arg_copy%sync_from_arena()
        if (arg_copy%kind /= TCHAR) then
            typ = create_mono_type(TCHAR)
            typ%alloc_info%needs_allocatable_string = .true.
            return
        end if

        arg_len = max(arg_copy%size, 0)
        if (arg_copy%alloc_info%needs_allocatable_string) then
            typ = create_mono_type(TCHAR)
            typ%alloc_info%needs_allocatable_string = .true.
        else
            typ = create_mono_type(TCHAR, char_size=arg_len)
            typ%alloc_info%needs_allocatable_string = .false.
        end if
    end subroutine refine_character_intrinsic_result

    integer function deduce_return_kind_from_args(arg_types) result(kind_value)
        type(mono_type_t), intent(in) :: arg_types(:)
        integer :: i
        integer :: best_kind
        integer :: current_kind

        best_kind = 0
        do i = 1, size(arg_types)
            block
                type(mono_type_t) :: type_copy
                type_copy = arg_types(i)
                call type_copy%sync_from_arena()
                current_kind = type_copy%kind
            end block
            if (current_kind <= 0) cycle
            select case (current_kind)
            case (TDOUBLE)
                kind_value = TDOUBLE
                return
            case (TCOMPLEX)
                if (best_kind /= TDOUBLE) best_kind = TCOMPLEX
            case (TREAL)
                if (best_kind /= TDOUBLE .and. best_kind /= TCOMPLEX) &
                    best_kind = TREAL
            case (TCHAR)
                if (best_kind == 0) best_kind = TCHAR
            case (TLOGICAL)
                if (best_kind == 0) best_kind = TLOGICAL
            case (TINT)
                if (best_kind == 0) best_kind = TINT
            case default
                if (best_kind == 0) best_kind = current_kind
            end select
        end do

        kind_value = best_kind
    end function deduce_return_kind_from_args

    logical function find_return_type(arena, func_name, return_type) result(found)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: func_name
        type(mono_type_t), intent(out) :: return_type
        integer :: i

        found = .false.
        return_type = create_mono_type(TREAL)

        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (node => arena%entries(i)%node)
            type is (function_def_node)
                if (.not. allocated(node%name)) cycle
                if (trim(node%name) /= trim(func_name)) cycle
                if (node%inferred_type%kind == TFUN .and. &
                    type_args_allocated(node%inferred_type) .and. &
                    type_args_size(node%inferred_type) >= 2) then
                    return_type = type_args_element(node%inferred_type, 2)
                    found = .true.
                    return
                else if (allocated(node%return_type)) then
                    select case (trim(node%return_type))
                    case ("integer")
                        return_type = create_mono_type(TINT)
                    case ("logical")
                        return_type = create_mono_type(TLOGICAL)
                    case ("character")
                        return_type = create_mono_type(TCHAR)
                    case default
                        return_type = create_mono_type(TREAL)
                    end select
                    found = .true.
                    return
                end if
            end select
        end do
    end function find_return_type

    function infer_array_slice_type(arena, slice_node, get_type_fn) result(typ)
        type(ast_arena_t), intent(inout) :: arena
        type(array_slice_node), intent(in) :: slice_node
        interface
            function get_type_fn(a, idx) result(t)
                import :: mono_type_t, ast_arena_t
                type(ast_arena_t), intent(inout) :: a
                integer, intent(in) :: idx
                type(mono_type_t) :: t
            end function get_type_fn
        end interface
        type(mono_type_t) :: typ
        type(mono_type_t) :: source_type
        type(mono_type_t) :: walker_type
        type(mono_type_t) :: base_type
        type(mono_type_t), allocatable :: args(:)
        logical, allocatable :: keep_dim(:)
        integer :: max_dims
        integer :: dims_to_process
        integer :: i
        integer :: bounds_idx
        logical :: is_range

        source_type = get_type_fn(arena, slice_node%array_index)
        if (source_type%kind /= TARRAY) then
            typ = source_type
            return
        end if

        walker_type = source_type
        max_dims = 0
        do while (walker_type%kind == TARRAY .and. walker_type%has_args())
            if (walker_type%get_args_count() <= 0) exit
            max_dims = max_dims + 1
            walker_type = walker_type%get_arg(1)
        end do
        base_type = walker_type

        if (max_dims <= 0) then
            typ = source_type
            return
        end if

        allocate (keep_dim(max_dims))
        keep_dim = .false.
        dims_to_process = min(max_dims, slice_node%num_dimensions)
        do i = 1, dims_to_process
            bounds_idx = slice_node%bounds_indices(i)
            is_range = .false.
            if (bounds_idx > 0 .and. bounds_idx <= arena%size) then
                if (allocated(arena%entries(bounds_idx)%node)) then
                    select type (bounds => arena%entries(bounds_idx)%node)
                    type is (range_expression_node)
                        is_range = .true.
                    type is (array_bounds_node)
                        is_range = .true.
                    end select
                end if
            end if
            keep_dim(i) = is_range
        end do

        if (slice_node%num_dimensions < max_dims) then
            keep_dim(slice_node%num_dimensions + 1:max_dims) = .true.
        end if

        if (.not. any(keep_dim)) then
            typ = base_type
            return
        end if

        typ = base_type
        do i = max_dims, 1, -1
            if (.not. keep_dim(i)) cycle
            allocate (args(1))
            args(1) = typ
            typ = create_mono_type(TARRAY, args=args)
            typ%size = 0
            typ%alloc_info%is_allocatable = .true.
            typ%alloc_info%needs_allocation_check = .true.
            typ%alloc_info%is_pointer = .false.
            typ%alloc_info%needs_allocatable_string = .false.
            deallocate (args)
        end do
    end function infer_array_slice_type

    function infer_array_literal_type(arena, array_lit, get_type_fn) result(typ)
        type(ast_arena_t), intent(inout) :: arena
        type(array_literal_node), intent(in) :: array_lit
        interface
            function get_type_fn(a, idx) result(t)
                import :: mono_type_t, ast_arena_t
                type(ast_arena_t), intent(inout) :: a
                integer, intent(in) :: idx
                type(mono_type_t) :: t
            end function get_type_fn
        end interface
        type(mono_type_t) :: typ
        type(mono_type_t) :: element_type, promoted_type, first_type
        type(mono_type_t) :: explicit_type
        type(mono_type_t), allocatable :: args(:), inner_args(:)
        integer :: i, elem_array_size, first_array_size, max_char_len
        logical :: has_real, all_arrays, consistent_sizes
        character(len=:), allocatable :: explicit_spec

        if (.not. allocated(array_lit%element_indices) .or. &
            size(array_lit%element_indices) == 0) then
            explicit_type%kind = 0
            explicit_type%size = 0
            if (allocated(array_lit%type_spec)) then
                explicit_spec = trim(array_lit%type_spec)
                if (len_trim(explicit_spec) > 0) then
                    explicit_type = mono_type_from_type_spec(explicit_spec)
                end if
            end if
            if (explicit_type%kind <= 0) then
                explicit_type = create_mono_type(TINT)
            end if
            allocate (args(1))
            args(1) = explicit_type
            typ = create_mono_type(TARRAY, args=args)
            typ%size = 0
            typ%alloc_info%is_allocatable = .true.
            typ%alloc_info%needs_allocation_check = .true.
            typ%alloc_info%is_pointer = .false.
            typ%alloc_info%needs_allocatable_string = .false.
            deallocate (args)
            return
        end if

        if (allocated(array_lit%type_spec)) then
            explicit_spec = trim(array_lit%type_spec)
            if (len_trim(explicit_spec) > 0) then
                explicit_type = mono_type_from_type_spec(explicit_spec)
                if (explicit_type%kind > 0) then
                    if (allocated(args)) deallocate (args)
                    allocate (args(1))
                    args(1) = explicit_type
                    typ = create_mono_type(TARRAY, args=args, &
                                           array_size=size(array_lit%element_indices))
                    deallocate (args)
                    return
                end if
            end if
        end if

        first_type = resolve_constructor_element_type( &
                     arena, array_lit%element_indices(1), get_type_fn)
        promoted_type = first_type
        has_real = (first_type%kind == TREAL)
        all_arrays = (first_type%kind == TARRAY)
        consistent_sizes = .true.
        max_char_len = 0

        if (all_arrays) then
            first_array_size = first_type%size
        end if

        if (first_type%kind == TCHAR) then
            max_char_len = first_type%size
        end if

        do i = 2, size(array_lit%element_indices)
            element_type = resolve_constructor_element_type( &
                           arena, array_lit%element_indices(i), get_type_fn)

            if (all_arrays .and. element_type%kind /= TARRAY) then
                all_arrays = .false.
            else if (all_arrays .and. element_type%kind == TARRAY) then
                elem_array_size = element_type%size
                if (elem_array_size /= first_array_size) then
                    consistent_sizes = .false.
                end if
            end if

            if (element_type%kind == TCHAR) then
                max_char_len = max(max_char_len, element_type%size)
            end if

            if (element_type%kind == TREAL) then
                has_real = .true.
                if (.not. all_arrays) promoted_type = create_mono_type(TREAL)
            else if (element_type%kind == TARRAY .and. element_type%has_args()) then
                if (element_type%get_args_count() > 0) then
                    promoted_type = element_type%get_arg(1)
                    if (promoted_type%kind == TREAL) then
                        has_real = .true.
                    end if
                end if
            end if
        end do

        if (all_arrays .and. consistent_sizes) then
            if (first_type%has_args() .and. first_type%get_args_count() > 0) then
                if (has_real) then
                    promoted_type = create_mono_type(TREAL)
                else
                    promoted_type = first_type%get_arg(1)
                end if
            else
                promoted_type = create_mono_type(TINT)
            end if

            allocate (inner_args(1))
            inner_args(1) = promoted_type

            allocate (args(1))
            args(1) = create_mono_type(TARRAY, args=inner_args, &
                                       array_size=first_array_size)
            typ = create_mono_type(TARRAY, args=args, &
                                   array_size=size(array_lit%element_indices))
            deallocate (inner_args)
        else
            if (has_real .and. promoted_type%kind == TINT) then
                promoted_type = create_mono_type(TREAL)
            end if

            if (promoted_type%kind == TCHAR .and. max_char_len > 0) then
                promoted_type = create_mono_type(TCHAR, char_size=max_char_len)
            end if

            allocate (args(1))
            args(1) = promoted_type
            typ = create_mono_type(TARRAY, args=args, &
                                   array_size=size(array_lit%element_indices))
        end if
    end function infer_array_literal_type

    recursive function resolve_constructor_element_type(arena, element_index, &
                                                        get_type_fn) &
        result(resolved_type)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: element_index
        interface
            function get_type_fn(a, idx) result(t)
                import :: mono_type_t, ast_arena_t
                type(ast_arena_t), intent(inout) :: a
                integer, intent(in) :: idx
                type(mono_type_t) :: t
            end function get_type_fn
        end interface
        type(mono_type_t) :: resolved_type

        resolved_type%kind = 0
        resolved_type%size = 0
        if (element_index <= 0) return
        if (element_index > arena%size) return
        if (.not. allocated(arena%entries(element_index)%node)) return

        select type (element_node => arena%entries(element_index)%node)
        type is (do_loop_node)
            resolved_type = resolve_implied_do_type(arena, element_index, &
                                                    get_type_fn)
        class default
            resolved_type = get_type_fn(arena, element_index)
        end select
    end function resolve_constructor_element_type

    recursive function resolve_implied_do_type(arena, loop_index, get_type_fn) &
        result(resolved_type)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: loop_index
        interface
            function get_type_fn(a, idx) result(t)
                import :: mono_type_t, ast_arena_t
                type(ast_arena_t), intent(inout) :: a
                integer, intent(in) :: idx
                type(mono_type_t) :: t
            end function get_type_fn
        end interface
        type(mono_type_t) :: resolved_type
        type(mono_type_t) :: element_type
        integer :: i

        resolved_type%kind = 0
        resolved_type%size = 0
        if (loop_index <= 0) return
        if (loop_index > arena%size) return
        if (.not. allocated(arena%entries(loop_index)%node)) return

        select type (loop_node => arena%entries(loop_index)%node)
        type is (do_loop_node)
            if (.not. allocated(loop_node%body_indices)) return
            do i = 1, size(loop_node%body_indices)
                element_type = resolve_constructor_element_type( &
                               arena, loop_node%body_indices(i), get_type_fn)
                if (element_type%kind == 0) cycle
                if (resolved_type%kind == 0) then
                    resolved_type = element_type
                else
                    resolved_type = get_common_type(resolved_type, element_type)
                end if
            end do
            if (resolved_type%kind == 0) resolved_type = create_mono_type(TINT)
        class default
            resolved_type = get_type_fn(arena, loop_index)
        end select
    end function resolve_implied_do_type

    function infer_array_intrinsic_type(arena, call_node, get_type_fn) &
        result(typ)
        type(ast_arena_t), intent(inout) :: arena
        type(call_or_subscript_node), intent(in) :: call_node
        interface
            function get_type_fn(a, idx) result(t)
                import :: mono_type_t, ast_arena_t
                type(ast_arena_t), intent(inout) :: a
                integer, intent(in) :: idx
                type(mono_type_t) :: t
            end function get_type_fn
        end interface
        type(mono_type_t) :: typ
        type(mono_type_t) :: element_type
        type(mono_type_t), allocatable :: args(:)
        integer :: ndims
        integer :: i
        character(len=:), allocatable :: func_name
        character(len=:), allocatable :: lowered_name
        integer :: num_args

        if (allocated(call_node%name)) then
            func_name = trim(call_node%name)
        else
            func_name = ""
        end if

        if (len_trim(func_name) > 0) then
            lowered_name = to_lower(func_name)
        else
            lowered_name = ""
        end if

        num_args = 0
        if (allocated(call_node%arg_indices)) then
            num_args = size(call_node%arg_indices)
        end if

        select case (lowered_name)
        case ("reshape")
            element_type = create_mono_type(TREAL)
            if (allocated(call_node%arg_indices) .and. &
                size(call_node%arg_indices) >= 1) then
                element_type = get_type_fn(arena, call_node%arg_indices(1))
                if (element_type%kind == TARRAY .and. element_type%has_args()) then
                    element_type = element_type%get_arg(1)
                end if
            end if

            ndims = 0
            if (allocated(call_node%arg_indices) .and. &
                size(call_node%arg_indices) >= 2) then
                ndims = infer_reshape_dimensions(arena, &
                                                 call_node%arg_indices(2))
            end if

            if (ndims > 0) then
                typ = element_type
                do i = 1, ndims
                    allocate (args(1))
                    args(1) = typ
                    typ = create_mono_type(TARRAY, args=args)
                    typ%size = 0
                    typ%alloc_info%is_allocatable = .true.
                    typ%alloc_info%needs_allocation_check = .true.
                    typ%alloc_info%is_pointer = .false.
                    typ%alloc_info%needs_allocatable_string = .false.
                    deallocate (args)
                end do
            else
                allocate (args(1))
                args(1) = element_type
                typ = create_mono_type(TARRAY, args=args)
                typ%size = 0
                typ%alloc_info%is_allocatable = .true.
                typ%alloc_info%needs_allocation_check = .true.
                typ%alloc_info%is_pointer = .false.
                typ%alloc_info%needs_allocatable_string = .false.
                deallocate (args)
            end if
            return
        case ("size")
            typ = create_mono_type(TINT)
            return
        case ("lbound", "ubound")
            if (num_args >= 2) then
                typ = create_mono_type(TINT)
            else
                allocate (args(1))
                args(1) = create_mono_type(TINT)
                typ = create_mono_type(TARRAY, args=args)
                typ%size = 0
                typ%alloc_info%is_allocatable = .true.
                typ%alloc_info%needs_allocation_check = .true.
                typ%alloc_info%is_pointer = .false.
                typ%alloc_info%needs_allocatable_string = .false.
                deallocate (args)
            end if
            return
        end select

        select case (lowered_name)
        case ("shape", "maxloc", "minloc")
            element_type = create_mono_type(TINT)
        case ("any", "all")
            element_type = create_mono_type(TLOGICAL)
        case default
            element_type = create_mono_type(TREAL)
        end select

        allocate (args(1))
        args(1) = element_type
        typ = create_mono_type(TARRAY, args=args)
        typ%size = 0
        typ%alloc_info%is_allocatable = .true.
        typ%alloc_info%needs_allocation_check = .true.
        typ%alloc_info%is_pointer = .false.
        typ%alloc_info%needs_allocatable_string = .false.
        deallocate (args)
    end function infer_array_intrinsic_type

    function mono_type_from_type_spec(type_spec) result(explicit_type)
        character(len=*), intent(in) :: type_spec
        type(mono_type_t) :: explicit_type
        character(len=:), allocatable :: trimmed
        character(len=:), allocatable :: lowered
        integer :: char_len

        explicit_type%kind = 0
        explicit_type%size = 0

        trimmed = adjustl(trim(type_spec))
        if (len_trim(trimmed) == 0) return

        lowered = to_lower(trimmed)

        if (index(lowered, 'double precision') == 1) then
            explicit_type = create_mono_type(TDOUBLE)
            return
        else if (index(lowered, 'integer') == 1) then
            explicit_type = create_mono_type(TINT)
            return
        else if (index(lowered, 'real') == 1) then
            explicit_type = create_mono_type(TREAL)
            return
        else if (index(lowered, 'logical') == 1) then
            explicit_type = create_mono_type(TLOGICAL)
            return
        else if (index(lowered, 'complex') == 1) then
            explicit_type = create_mono_type(TCOMPLEX)
            return
        else if (index(lowered, 'character') == 1) then
            char_len = extract_first_integer(trimmed)
            if (char_len > 0) then
                explicit_type = create_mono_type(TCHAR, char_size=char_len)
            else
                explicit_type = create_mono_type(TCHAR)
            end if
            return
        end if
    end function mono_type_from_type_spec

    pure function extract_first_integer(text) result(value)
        character(len=*), intent(in) :: text
        integer :: value
        integer :: i
        integer :: buf_len
        character(len=32) :: buffer
        integer :: ios

        value = -1
        buf_len = 0
        buffer = ' '

        do i = 1, len_trim(text)
            if (text(i:i) >= '0' .and. text(i:i) <= '9') then
                if (buf_len < len(buffer)) then
                    buf_len = buf_len + 1
                    buffer(buf_len:buf_len) = text(i:i)
                end if
            else if (buf_len > 0) then
                exit
            end if
        end do

        if (buf_len > 0) then
            read (buffer(1:buf_len), *, iostat=ios) value
            if (ios /= 0) value = -1
        end if
    end function extract_first_integer

    function infer_reshape_dimensions(arena, shape_idx) result(ndims)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: shape_idx
        integer :: ndims

        ndims = 0
        if (shape_idx <= 0 .or. shape_idx > arena%size) return
        if (.not. allocated(arena%entries(shape_idx)%node)) return

        select type (shape_node => arena%entries(shape_idx)%node)
        type is (array_literal_node)
            if (allocated(shape_node%element_indices)) then
                ndims = size(shape_node%element_indices)
            end if
        end select
    end function infer_reshape_dimensions

end module semantic_function_array
