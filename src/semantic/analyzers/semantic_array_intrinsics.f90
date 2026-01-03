module semantic_array_intrinsics
    use type_system_unified, only: mono_type_t, create_mono_type, TINT, TREAL, &
                                   TLOGICAL, TARRAY
    use type_array_safe, only: safe_extract_array_rank, safe_peel_array_to_base
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: call_or_subscript_node, array_literal_node
    use string_utils_mod, only: to_lower
    use semantic_array_type_builders, only: build_deferred_shape_array, &
                                            build_fixed_shape_array
    use semantic_type_operations, only: get_common_type
    use semantic_function_helpers, only: get_type_lookup, &
                                         gather_call_argument_types
    use semantic_constant_values, only: get_constant_integer_value
    use constant_transformation, only: fold_constants_in_arena
    implicit none
    private

    public :: infer_array_intrinsic_type

contains

    function infer_array_intrinsic_type(arena, call_node, get_type_fn) &
        result(typ)
        type(ast_arena_t), intent(inout) :: arena
        type(call_or_subscript_node), intent(in) :: call_node
        procedure(get_type_lookup) :: get_type_fn
        type(mono_type_t) :: typ
        character(len=:), allocatable :: lowered_name
        integer :: num_args

        lowered_name = ""
        if (allocated(call_node%name)) then
            lowered_name = to_lower(trim(call_node%name))
        end if

        if (allocated(call_node%arg_indices)) then
            num_args = size(call_node%arg_indices)
        else
            num_args = 0
        end if

        if (is_reduction_intrinsic(lowered_name)) then
            typ = infer_reduction_intrinsic_result(arena, call_node, &
                                                   get_type_fn, lowered_name)
            return
        end if

        select case (lowered_name)
        case ("matmul")
            typ = handle_matmul_intrinsic(arena, call_node, get_type_fn, &
                                          num_args)
        case ("reshape")
            typ = handle_reshape_intrinsic(arena, call_node, get_type_fn)
        case ("transpose")
            typ = handle_transpose_intrinsic(arena, call_node, get_type_fn)
        case ("pack")
            typ = handle_pack_intrinsic(arena, call_node, get_type_fn)
        case ("size")
            typ = create_mono_type(TINT)
        case ("lbound", "ubound")
            typ = handle_bound_intrinsic(num_args)
        case default
            typ = handle_generic_array_intrinsic(lowered_name)
        end select
    end function infer_array_intrinsic_type

    function handle_matmul_intrinsic(arena, call_node, get_type_fn, num_args) &
        result(typ)
        type(ast_arena_t), intent(inout) :: arena
        type(call_or_subscript_node), intent(in) :: call_node
        procedure(get_type_lookup) :: get_type_fn
        integer, intent(in) :: num_args
        type(mono_type_t) :: typ
        type(mono_type_t) :: lhs_type
        type(mono_type_t) :: rhs_type
        type(mono_type_t) :: lhs_base
        type(mono_type_t) :: rhs_base
        type(mono_type_t) :: result_element
        type(mono_type_t), allocatable :: arg_types(:)
        integer :: lhs_rank
        integer :: rhs_rank
        integer :: i
        integer, allocatable :: lhs_dims(:)
        integer, allocatable :: rhs_dims(:)
        integer, allocatable :: fixed_dims(:)
        logical :: lhs_dims_known
        logical :: rhs_dims_known

        if (num_args < 2) then
            typ = build_deferred_shape_array(create_mono_type(TREAL), 1)
            return
        end if

        call gather_call_argument_types(arena, call_node, get_type_fn, arg_types)
        do i = 1, size(arg_types)
            call arg_types(i)%sync_from_arena()
        end do

        lhs_type = arg_types(1)
        rhs_type = arg_types(2)
        call lhs_type%sync_from_arena()
        call rhs_type%sync_from_arena()

        call safe_extract_array_rank(lhs_type, lhs_rank, lhs_base)
        call safe_extract_array_rank(rhs_type, rhs_rank, rhs_base)

        result_element = get_common_type(lhs_base, rhs_base)
        call result_element%sync_from_arena()

        call collect_array_dimensions(lhs_type, lhs_dims, lhs_dims_known)
        call collect_array_dimensions(rhs_type, rhs_dims, rhs_dims_known)

        if (lhs_rank == 2 .and. rhs_rank == 2 .and. lhs_dims_known .and. &
            rhs_dims_known) then
            if (size(lhs_dims) >= 2 .and. size(rhs_dims) >= 2) then
                allocate (fixed_dims(2))
                fixed_dims(1) = lhs_dims(1)
                fixed_dims(2) = rhs_dims(2)
                typ = build_fixed_shape_array(result_element, fixed_dims)
                call typ%sync_from_arena()
                call cleanup_matmul_dims(lhs_dims, rhs_dims, fixed_dims)
                return
            end if
        end if

        if (lhs_rank == 2 .and. rhs_rank == 1 .and. lhs_dims_known) then
            if (size(lhs_dims) >= 1) then
                allocate (fixed_dims(1))
                fixed_dims(1) = lhs_dims(1)
                typ = build_fixed_shape_array(result_element, fixed_dims)
                call typ%sync_from_arena()
                call cleanup_matmul_dims(lhs_dims, rhs_dims, fixed_dims)
                return
            end if
        end if

        if (lhs_rank == 1 .and. rhs_rank == 2 .and. rhs_dims_known) then
            if (size(rhs_dims) >= 2) then
                allocate (fixed_dims(1))
                fixed_dims(1) = rhs_dims(2)
                typ = build_fixed_shape_array(result_element, fixed_dims)
                call typ%sync_from_arena()
                call cleanup_matmul_dims(lhs_dims, rhs_dims, fixed_dims)
                return
            end if
        end if

        call cleanup_matmul_dims(lhs_dims, rhs_dims, fixed_dims)
        typ = build_matmul_result(result_element, lhs_rank, rhs_rank)
    end function handle_matmul_intrinsic

    subroutine extract_rank_and_base(source_type, rank, base_type)
        type(mono_type_t), intent(in) :: source_type
        integer, intent(out) :: rank
        type(mono_type_t), intent(out) :: base_type
        type(mono_type_t) :: current
        integer :: visited_indices(100)
        integer :: visited_count
        integer :: i
        logical :: is_cycle

        current = source_type
        call current%sync_from_arena()
        rank = 0
        visited_count = 0

        do while (current%kind == TARRAY .and. current%has_args())
            ! Detect cycles by checking if we've seen this type_id before
            is_cycle = .false.
            do i = 1, visited_count
                if (visited_indices(i) == current%handle%type_id) then
                    is_cycle = .true.
                    exit
                end if
            end do

            if (is_cycle) then
                ! Circular reference detected - stop traversal
                exit
            end if

            ! Track this type_id
            visited_count = visited_count + 1
            if (visited_count <= size(visited_indices)) then
                visited_indices(visited_count) = current%handle%type_id
            end if

            rank = rank + 1
            current = current%get_arg(1)
            call current%sync_from_arena()
        end do
        base_type = current
        call base_type%sync_from_arena()
    end subroutine extract_rank_and_base

    function build_matmul_result(result_element, lhs_rank, rhs_rank) result(typ)
        type(mono_type_t), intent(in) :: result_element
        integer, intent(in) :: lhs_rank
        integer, intent(in) :: rhs_rank
        type(mono_type_t) :: typ

        if (lhs_rank == 2 .and. rhs_rank == 2) then
            typ = build_deferred_shape_array(result_element, 2)
        else if ((lhs_rank == 2 .and. rhs_rank == 1) .or. &
                 (lhs_rank == 1 .and. rhs_rank == 2)) then
            typ = build_deferred_shape_array(result_element, 1)
        else if (lhs_rank == 1 .and. rhs_rank == 1) then
            typ = result_element
        else
            typ = build_deferred_shape_array(result_element, 1)
        end if
    end function build_matmul_result

    function handle_reshape_intrinsic(arena, call_node, get_type_fn) result(typ)
        type(ast_arena_t), intent(inout) :: arena
        type(call_or_subscript_node), intent(in) :: call_node
        procedure(get_type_lookup) :: get_type_fn
        type(mono_type_t) :: typ
        type(mono_type_t) :: element_type
        integer :: ndims
        integer, allocatable :: dimension_sizes(:)
        logical :: has_literal_dimensions

        ! Note: Constant folding is now done at program level for lazy Fortran
        ! (see analyze_program_node_arena in semantic_analyzer_context_impl.f90)

        element_type = create_mono_type(TREAL)
        ndims = 0
        if (allocated(call_node%arg_indices)) then
            if (size(call_node%arg_indices) >= 1) then
                element_type = get_type_fn(arena, call_node%arg_indices(1))
                if (element_type%kind == TARRAY .and. element_type%has_args()) &
                    then
                    element_type = element_type%get_arg(1)
                end if
            end if
            if (size(call_node%arg_indices) >= 2) then
                call extract_reshape_dimensions(arena, &
                                                call_node%arg_indices(2), &
                                                ndims, dimension_sizes, &
                                                has_literal_dimensions)
            end if
        end if

        if (ndims <= 0) then
            typ = build_deferred_shape_array(element_type, 1)
        else if (has_literal_dimensions) then
            typ = build_fixed_shape_array(element_type, dimension_sizes)
            call typ%sync_from_arena()
        else
            typ = build_deferred_shape_array(element_type, ndims)
        end if
    end function handle_reshape_intrinsic

    function handle_transpose_intrinsic(arena, call_node, get_type_fn) result(typ)
        type(ast_arena_t), intent(inout) :: arena
        type(call_or_subscript_node), intent(in) :: call_node
        procedure(get_type_lookup) :: get_type_fn
        type(mono_type_t) :: typ
        type(mono_type_t) :: arg_type
        type(mono_type_t) :: element_type
        integer :: arg_rank
        integer :: result_rank

        element_type = create_mono_type(TREAL)
        arg_rank = 0

        if (allocated(call_node%arg_indices)) then
            if (size(call_node%arg_indices) >= 1) then
                arg_type = get_type_fn(arena, call_node%arg_indices(1))
                call arg_type%sync_from_arena()
                call safe_extract_array_rank(arg_type, arg_rank, element_type)
            end if
        end if

        result_rank = max(2, max(1, arg_rank))
        typ = build_deferred_shape_array(element_type, result_rank)
    end function handle_transpose_intrinsic

    function handle_pack_intrinsic(arena, call_node, get_type_fn) result(typ)
        type(ast_arena_t), intent(inout) :: arena
        type(call_or_subscript_node), intent(in) :: call_node
        procedure(get_type_lookup) :: get_type_fn
        type(mono_type_t) :: typ
        type(mono_type_t) :: arg_type
        type(mono_type_t) :: element_type
        integer :: arg_rank

        element_type = create_mono_type(TREAL)
        arg_rank = 0

        if (allocated(call_node%arg_indices)) then
            if (size(call_node%arg_indices) >= 1) then
                arg_type = get_type_fn(arena, call_node%arg_indices(1))
                call arg_type%sync_from_arena()
                call safe_extract_array_rank(arg_type, arg_rank, element_type)
            end if
        end if

        typ = build_deferred_shape_array(element_type, 1)
    end function handle_pack_intrinsic

    function handle_bound_intrinsic(num_args) result(typ)
        integer, intent(in) :: num_args
        type(mono_type_t) :: typ
        type(mono_type_t) :: integer_type

        integer_type = create_mono_type(TINT)
        if (num_args >= 2) then
            typ = integer_type
        else
            typ = build_deferred_shape_array(integer_type, 1)
        end if
    end function handle_bound_intrinsic

    function handle_generic_array_intrinsic(lowered_name) result(typ)
        character(len=*), intent(in) :: lowered_name
        type(mono_type_t) :: typ
        type(mono_type_t) :: element_type

        select case (lowered_name)
        case ("shape", "maxloc", "minloc")
            element_type = create_mono_type(TINT)
        case ("any", "all")
            element_type = create_mono_type(TLOGICAL)
        case default
            element_type = create_mono_type(TREAL)
        end select

        typ = build_deferred_shape_array(element_type, 1)
    end function handle_generic_array_intrinsic

    subroutine extract_reshape_dimensions(arena, shape_idx, ndims, &
                                          dimension_sizes, has_literals)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: shape_idx
        integer, intent(out) :: ndims
        integer, allocatable, intent(out) :: dimension_sizes(:)
        logical, intent(out) :: has_literals

        ndims = 0
        has_literals = .false.
        if (.not. arena%has_node_at(shape_idx)) return

        select type (shape_node => arena%entries(shape_idx)%node)
        type is (array_literal_node)
            call populate_literal_dimensions(arena, shape_node, ndims, &
                                             dimension_sizes, has_literals)
        end select
    end subroutine extract_reshape_dimensions

    subroutine populate_literal_dimensions(arena, shape_node, ndims, &
                                           dimension_sizes, has_literals)
        type(ast_arena_t), intent(inout) :: arena
        type(array_literal_node), intent(in) :: shape_node
        integer, intent(out) :: ndims
        integer, allocatable, intent(out) :: dimension_sizes(:)
        logical, intent(out) :: has_literals
        integer :: i
        integer :: dim_value

        has_literals = .false.
        ndims = 0
        if (.not. allocated(shape_node%element_indices)) return

        ndims = size(shape_node%element_indices)
        if (ndims <= 0) return

        allocate (dimension_sizes(ndims))
        has_literals = .true.
        do i = 1, ndims
            if (.not. literal_dimension_value(arena, &
                                              shape_node%element_indices(i), &
                                              dim_value)) then
                has_literals = .false.
                exit
            end if
            dimension_sizes(i) = dim_value
        end do

        if (.not. has_literals) then
            deallocate (dimension_sizes)
        end if
    end subroutine populate_literal_dimensions

    logical function literal_dimension_value(arena, elem_idx, value)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: elem_idx
        integer, intent(out) :: value
        logical :: is_constant

        is_constant = get_constant_integer_value(arena, elem_idx, value)
        literal_dimension_value = is_constant .and. value > 0
    end function literal_dimension_value

    pure logical function is_reduction_intrinsic(name) result(is_reduction)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: lowered

        lowered = to_lower(adjustl(trim(name)))
        select case (lowered)
        case ("sum", "product", "maxval", "minval", "any", "all", "count")
            is_reduction = .true.
        case default
            is_reduction = .false.
        end select
    end function is_reduction_intrinsic

    function infer_reduction_intrinsic_result(arena, call_node, get_type_fn, &
                                              lowered_name) result(typ)
        type(ast_arena_t), intent(inout) :: arena
        type(call_or_subscript_node), intent(in) :: call_node
        procedure(get_type_lookup) :: get_type_fn
        character(len=*), intent(in) :: lowered_name
        type(mono_type_t) :: typ
        type(mono_type_t) :: arg_type
        type(mono_type_t) :: element_type
        integer :: num_args
        integer :: element_kind

        typ = create_mono_type(TREAL)
        element_kind = 0

        num_args = 0
        if (allocated(call_node%arg_indices)) num_args = &
            size(call_node%arg_indices)

        if (num_args >= 1) then
            arg_type = get_type_fn(arena, call_node%arg_indices(1))
            call arg_type%sync_from_arena()
            element_type = safe_peel_array_to_base(arg_type)

            if (element_type%kind > 0) element_kind = element_type%kind
        end if

        select case (lowered_name)
        case ("any", "all")
            typ = create_mono_type(TLOGICAL)
        case ("count")
            typ = create_mono_type(TINT)
        case default
            if (element_kind > 0) then
                typ = create_mono_type(element_kind)
            else if (typ%kind <= 0) then
                typ = create_mono_type(TREAL)
            end if
        end select

        typ%alloc_info%is_allocatable = .false.
        typ%alloc_info%needs_allocatable_string = .false.
        typ%alloc_info%needs_allocation_check = .false.
        typ%alloc_info%is_pointer = .false.
    end function infer_reduction_intrinsic_result

    subroutine collect_array_dimensions(array_type, dims, all_known)
        type(mono_type_t), intent(in) :: array_type
        integer, allocatable, intent(out) :: dims(:)
        logical, intent(out) :: all_known
        type(mono_type_t) :: current
        integer :: rank
        integer :: idx
        integer :: visited_indices(100)
        integer :: visited_count
        integer :: i
        logical :: is_cycle

        call safe_extract_array_rank(array_type, rank, current)
        if (rank <= 0) then
            allocate (dims(0))
            all_known = .false.
            return
        end if

        allocate (dims(rank))
        current = array_type
        all_known = .true.
        visited_count = 0

        do idx = 1, rank
            if (current%kind /= TARRAY) then
                all_known = .false.
                exit
            end if

            ! Detect cycles by checking if we've seen this type_id before
            is_cycle = .false.
            do i = 1, visited_count
                if (visited_indices(i) == current%handle%type_id) then
                    is_cycle = .true.
                    exit
                end if
            end do

            if (is_cycle) then
                ! Circular reference detected - stop traversal
                all_known = .false.
                exit
            end if

            ! Track this type_id
            visited_count = visited_count + 1
            if (visited_count <= size(visited_indices)) then
                visited_indices(visited_count) = current%handle%type_id
            end if

            dims(idx) = current%size
            if (dims(idx) <= 0) all_known = .false.
            if (current%has_args() .and. current%get_args_count() >= 1) then
                current = current%get_arg(1)
            else if (idx < rank) then
                all_known = .false.
                exit
            end if
        end do
    end subroutine collect_array_dimensions

    subroutine cleanup_matmul_dims(lhs_dims, rhs_dims, fixed_dims)
        integer, allocatable, intent(inout) :: lhs_dims(:)
        integer, allocatable, intent(inout) :: rhs_dims(:)
        integer, allocatable, intent(inout) :: fixed_dims(:)

        if (allocated(lhs_dims)) deallocate (lhs_dims)
        if (allocated(rhs_dims)) deallocate (rhs_dims)
        if (allocated(fixed_dims)) deallocate (fixed_dims)
    end subroutine cleanup_matmul_dims

end module semantic_array_intrinsics
