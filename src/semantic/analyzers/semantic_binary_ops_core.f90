module semantic_binary_ops_core
    ! Binary operation type inference
    use type_system_unified, only: mono_type_t, create_mono_type, TCHAR, &
                                   TARRAY, allocation_info_t
    use type_array_safe, only: safe_extract_array_rank, safe_peel_array_to_base
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: binary_op_node
    use semantic_binary_operations, only: infer_string_concatenation, &
                                          infer_comparison_operation, &
                                          infer_logical_operation
    use semantic_type_operations, only: get_common_type
    implicit none
    private

    public :: infer_binary_operation
    public :: rewrite_operator

contains

    function infer_binary_operation(arena, binop_index, binop, left_typ, right_typ) &
        result(typ)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: binop_index
        type(binary_op_node), intent(in) :: binop
        type(mono_type_t), intent(in) :: left_typ, right_typ
        type(mono_type_t) :: typ

        if (binop%operator == "+") then
            if (left_typ%kind == TCHAR .and. right_typ%kind == TCHAR) then
                typ = infer_string_concatenation(arena, binop%left_index, &
                                                 binop%right_index, left_typ, &
                                                 right_typ)
                call rewrite_operator(arena, binop_index, "//")
                return
            end if
        end if

        if (binop%operator == "//") then
            typ = infer_string_concatenation(arena, binop%left_index, &
                                             binop%right_index, left_typ, &
                                             right_typ)
        else if (binop%operator == "==" .or. binop%operator == "/=" .or. &
                 binop%operator == "<" .or. binop%operator == "<=" .or. &
                 binop%operator == ">" .or. binop%operator == ">=") then
            typ = infer_comparison_operation(left_typ, right_typ)
        else if (binop%operator == ".and." .or. binop%operator == ".or." .or. &
                 binop%operator == ".not." .or. binop%operator == ".eqv." .or. &
                 binop%operator == ".neqv.") then
            typ = infer_logical_operation()
        else
            typ = get_common_type(left_typ, right_typ)
            if (typ%kind == TARRAY .or. left_typ%kind == TARRAY .or. &
                right_typ%kind == TARRAY) then
                typ = ensure_array_shape(typ, left_typ, right_typ)
            end if
            if (typ%kind == 0) typ = left_typ
        end if
    end function infer_binary_operation

    function ensure_array_shape(common_typ, left_typ, right_typ) result(res_typ)
        type(mono_type_t), intent(in) :: common_typ
        type(mono_type_t), intent(in) :: left_typ
        type(mono_type_t), intent(in) :: right_typ
        type(mono_type_t) :: res_typ
        integer, allocatable :: merged_shape(:)
        logical :: has_arrays

        has_arrays = (left_typ%kind == TARRAY) .or. (right_typ%kind == TARRAY)
        res_typ = common_typ
        if (.not. has_arrays) return

        call merge_array_shapes(left_typ, right_typ, merged_shape)
        if (.not. allocated(merged_shape)) return
        if (size(merged_shape) == 0) return

        res_typ = rebuild_array_type(common_typ, merged_shape, left_typ, right_typ)
    end function ensure_array_shape

    subroutine merge_array_shapes(left_typ, right_typ, merged_shape)
        type(mono_type_t), intent(in) :: left_typ
        type(mono_type_t), intent(in) :: right_typ
        integer, allocatable, intent(out) :: merged_shape(:)
        integer, allocatable :: left_shape(:)
        integer, allocatable :: right_shape(:)
        integer :: rank, i
        integer :: left_dim, right_dim

        call extract_array_shape(left_typ, left_shape)
        call extract_array_shape(right_typ, right_shape)

        rank = max(size(left_shape), size(right_shape))
        if (rank <= 0) then
            allocate (merged_shape(0))
            if (allocated(left_shape)) deallocate (left_shape)
            if (allocated(right_shape)) deallocate (right_shape)
            return
        end if

        allocate (merged_shape(rank))
        merged_shape = 0

        do i = 1, rank
            left_dim = shape_at(left_shape, i)
            right_dim = shape_at(right_shape, i)
            if (left_dim > 0 .and. right_dim > 0) then
                if (left_dim == right_dim) then
                    merged_shape(i) = left_dim
                else
                    merged_shape(i) = 0
                end if
            else if (left_dim > 0) then
                merged_shape(i) = left_dim
            else if (right_dim > 0) then
                merged_shape(i) = right_dim
            else
                merged_shape(i) = 0
            end if
        end do

        if (allocated(left_shape)) deallocate (left_shape)
        if (allocated(right_shape)) deallocate (right_shape)
    end subroutine merge_array_shapes

    function rebuild_array_type(common_typ, shape, left_typ, right_typ) &
        result(array_typ)
        type(mono_type_t), intent(in) :: common_typ
        integer, intent(in) :: shape(:)
        type(mono_type_t), intent(in) :: left_typ
        type(mono_type_t), intent(in) :: right_typ
        type(mono_type_t) :: array_typ
        type(mono_type_t) :: element_type
        type(mono_type_t) :: current
        type(mono_type_t), allocatable :: args(:)
        integer :: i
        type(allocation_info_t) :: alloc_flags

        element_type = extract_element_type(common_typ, left_typ, right_typ)
        if (element_type%kind == 0) then
            array_typ = common_typ
            return
        end if
        current = element_type

        do i = size(shape), 1, -1
            allocate (args(1))
            args(1) = current
            current = create_mono_type(TARRAY, args=args, array_size=shape(i))
            deallocate (args)
        end do

        alloc_flags = merge_alloc_flags(common_typ, left_typ, right_typ, shape)
        current%alloc_info = alloc_flags

        array_typ = current
    end function rebuild_array_type

    type(allocation_info_t) function merge_alloc_flags(common_typ, left_typ, &
                                                       right_typ, shape) result(flags)
        type(mono_type_t), intent(in) :: common_typ
        type(mono_type_t), intent(in) :: left_typ
        type(mono_type_t), intent(in) :: right_typ
        integer, intent(in) :: shape(:)

        flags = common_typ%alloc_info
        if (left_typ%kind == TARRAY) then
            flags%is_allocatable = flags%is_allocatable .or. &
                                   left_typ%alloc_info%is_allocatable
            flags%is_pointer = flags%is_pointer .or. &
                               left_typ%alloc_info%is_pointer
            flags%is_target = flags%is_target .or. &
                              left_typ%alloc_info%is_target
        end if
        if (right_typ%kind == TARRAY) then
            flags%is_allocatable = flags%is_allocatable .or. &
                                   right_typ%alloc_info%is_allocatable
            flags%is_pointer = flags%is_pointer .or. &
                               right_typ%alloc_info%is_pointer
            flags%is_target = flags%is_target .or. &
                              right_typ%alloc_info%is_target
        end if
        if (any(shape <= 0)) then
            flags%is_allocatable = .true.
        end if
    end function merge_alloc_flags

    subroutine extract_array_shape(typ, shape)
        type(mono_type_t), intent(in) :: typ
        integer, allocatable, intent(out) :: shape(:)
        type(mono_type_t) :: current
        integer :: rank, i

        rank = array_rank(typ)
        if (rank <= 0) then
            allocate (shape(0))
            return
        end if

        allocate (shape(rank))
        shape = 0
        current = typ

        do i = 1, rank
            if (current%kind /= TARRAY) exit
            shape(i) = current%size
            if (i < rank) then
                if (current%has_args() .and. current%get_args_count() >= 1) then
                    current = current%get_arg(1)
                else
                    exit
                end if
            end if
        end do
    end subroutine extract_array_shape

    integer function array_rank(typ) result(rank)
        type(mono_type_t), intent(in) :: typ
        type(mono_type_t) :: base_type

        call safe_extract_array_rank(typ, rank, base_type)
    end function array_rank

    integer function shape_at(shape, idx) result(value)
        integer, allocatable, intent(in) :: shape(:)
        integer, intent(in) :: idx

        value = 0
        if (.not. allocated(shape)) return
        if (idx < 1 .or. idx > size(shape)) return
        value = shape(idx)
    end function shape_at

    function extract_element_type(common_typ, left_typ, right_typ) result(elem_typ)
        type(mono_type_t), intent(in) :: common_typ
        type(mono_type_t), intent(in) :: left_typ
        type(mono_type_t), intent(in) :: right_typ
        type(mono_type_t) :: elem_typ

        elem_typ = peel_array_layer(common_typ)
        if (elem_typ%kind /= 0) return

        elem_typ = peel_array_layer(left_typ)
        if (elem_typ%kind /= 0) return

        elem_typ = peel_array_layer(right_typ)
    end function extract_element_type

    function peel_array_layer(typ) result(inner_typ)
        type(mono_type_t), intent(in) :: typ
        type(mono_type_t) :: inner_typ

        inner_typ = safe_peel_array_to_base(typ)
        if (inner_typ%kind == TARRAY) then
            inner_typ%kind = 0
        end if
    end function peel_array_layer

    subroutine rewrite_operator(arena, node_index, new_operator)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: new_operator

        if (.not. arena%has_node_at(node_index)) return

        select type (node => arena%entries(node_index)%node)
        type is (binary_op_node)
            node%operator = new_operator
            arena%entries(node_index)%node = node
        end select
    end subroutine rewrite_operator

end module semantic_binary_ops_core
