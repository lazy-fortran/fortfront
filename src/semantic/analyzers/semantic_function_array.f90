module semantic_function_array
    ! Function call and array type inference
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use type_system_unified, only: type_var_t, mono_type_t, poly_type_t, &
                                   create_mono_type, create_poly_type, &
                                   TVAR, TINT, TREAL, TCHAR, TLOGICAL, TFUN, TARRAY, &
                                   type_args_allocated, type_args_size, &
                                   type_args_element
    use scope_manager
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: call_or_subscript_node, array_literal_node
    use ast_nodes_procedure, only: function_def_node
    use ast_nodes_data, only: declaration_node
    use ast_nodes_bounds, only: array_slice_node, range_expression_node, &
                                array_bounds_node
    use intrinsic_registry, only: get_intrinsic_signature, is_intrinsic_function
    use semantic_validation_utils, only: int_to_str
    implicit none
    private

    public :: infer_function_call_type
    public :: infer_array_slice_type
    public :: infer_array_literal_type

contains

    function infer_function_call_type(arena, call_node, scopes, get_type_fn) &
        result(typ)
        type(ast_arena_t), intent(inout) :: arena
        type(call_or_subscript_node), intent(in) :: call_node
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
        character(len=:), allocatable :: intrinsic_sig
        integer :: i
        logical :: is_intrinsic_func

        typ = create_mono_type(TREAL)

        if (allocated(call_node%arg_indices)) then
            do i = 1, size(call_node%arg_indices)
                arg_type = get_type_fn(arena, call_node%arg_indices(i))
            end do
        end if

        if (allocated(call_node%name)) then
            call scopes%lookup(call_node%name, scheme)
        end if

        if (allocated(scheme)) then
            typ = scheme%get_mono()
            if (typ%kind == TFUN .and. type_args_allocated(typ) .and. &
                type_args_size(typ) >= 2) then
                typ = type_args_element(typ, 2)
            end if
        else if (allocated(call_node%name) .and. &
                 find_return_type(arena, call_node%name, typ)) then
            continue
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
    end function infer_function_call_type

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
        type(mono_type_t), allocatable :: args(:), inner_args(:)
        integer :: i, elem_array_size, first_array_size
        logical :: has_real, all_arrays, consistent_sizes

        if (.not. allocated(array_lit%element_indices) .or. &
            size(array_lit%element_indices) == 0) then
            allocate (args(1))
            args(1) = create_mono_type(TINT)
            typ = create_mono_type(TARRAY, args=args)
            return
        end if

        first_type = get_type_fn(arena, array_lit%element_indices(1))
        promoted_type = first_type
        has_real = (first_type%kind == TREAL)
        all_arrays = (first_type%kind == TARRAY)
        consistent_sizes = .true.

        if (all_arrays) then
            first_array_size = first_type%size
        end if

        do i = 2, size(array_lit%element_indices)
            element_type = get_type_fn(arena, array_lit%element_indices(i))

            if (all_arrays .and. element_type%kind /= TARRAY) then
                all_arrays = .false.
            else if (all_arrays .and. element_type%kind == TARRAY) then
                elem_array_size = element_type%size
                if (elem_array_size /= first_array_size) then
                    consistent_sizes = .false.
                end if
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

            allocate (args(1))
            args(1) = promoted_type
            typ = create_mono_type(TARRAY, args=args, &
                                   array_size=size(array_lit%element_indices))
        end if
    end function infer_array_literal_type

end module semantic_function_array
