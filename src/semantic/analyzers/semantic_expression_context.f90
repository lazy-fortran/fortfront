module semantic_expression_context
    use type_system_unified, only: type_var_t, mono_type_t, create_mono_type, &
                                   create_type_var, TVAR, TINT, TREAL, TCHAR, &
                                   TLOGICAL, TARRAY, TDOUBLE
    use ast_base, only: LITERAL_INTEGER, LITERAL_REAL
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: literal_node, identifier_node, binary_op_node, &
                              array_literal_node, call_or_subscript_node
    use semantic_type_operations, only: get_common_type
    use semantic_array_type_builders, only: collapse_array_rank
    use semantic_function_helpers, only: find_return_type
    implicit none
    private

    public :: infer_type_from_usage_context
    public :: infer_expression_type_static

    integer, parameter :: MAX_EXPR_RECURSION_DEPTH = 256

contains

    recursive subroutine infer_expression_type_static_impl(arena, expr_index, &
                                                           param_names, param_types, &
                                                           visiting, depth, max_depth, &
                                                           typ)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        logical, intent(inout) :: visiting(:)
        integer, intent(in) :: depth
        integer, intent(in) :: max_depth
        type(mono_type_t), intent(out) :: typ
        logical :: abort_eval

        typ%kind = 0
        if (depth > max_depth) return
        if (.not. expression_available(arena, expr_index)) return
        if (expr_index <= size(visiting)) then
            if (visiting(expr_index)) return
            visiting(expr_index) = .true.
        end if
        abort_eval = .false.

        select type (node => arena%entries(expr_index)%node)
        type is (literal_node)
            typ = infer_literal_expression_type(node)
        type is (identifier_node)
            typ = infer_identifier_expression_type(node, param_names, param_types)
        type is (binary_op_node)
            if (node%left_index == expr_index .or. node%right_index == expr_index) then
                abort_eval = .true.
            else
                call infer_binary_expression_type_impl(arena, node, param_names, &
                                                       param_types, visiting, &
                                                       depth + 1, max_depth, typ)
            end if
        type is (array_literal_node)
            typ = infer_array_literal_type_from_context_impl(arena, node, &
                                                             param_names, param_types, &
                                                             visiting, depth + 1, &
                                                             max_depth)
        type is (call_or_subscript_node)
            typ = infer_call_expression_type(arena, node, param_names, param_types)
        class default
            typ%kind = 0
        end select

        if (abort_eval) typ%kind = 0
        if (expr_index <= size(visiting)) visiting(expr_index) = .false.
    end subroutine infer_expression_type_static_impl

    function infer_type_from_usage_context(var_name, next_var_id) result(typ)
        character(len=*), intent(in) :: var_name
        integer, intent(inout) :: next_var_id
        type(mono_type_t) :: typ

        select case (trim(var_name))
        case ('i', 'j', 'k', 'n', 'count', 'index', 'num', 'size')
            typ = create_mono_type(TINT)
        case ('x', 'y', 'z', 'result', 'value', 'temp')
            typ = create_mono_type(TVAR, var=create_type_var(next_var_id, "v"))
            next_var_id = next_var_id + 1
        case ('flag', 'found', 'done', 'success', 'valid')
            typ = create_mono_type(TLOGICAL)
        case default
            typ = infer_type_from_name_pattern(var_name, next_var_id)
        end select
    end function infer_type_from_usage_context

    function infer_type_from_name_pattern(var_name, next_var_id) result(typ)
        character(len=*), intent(in) :: var_name
        integer, intent(inout) :: next_var_id
        type(mono_type_t) :: typ

        if (index(var_name, 'str') > 0 .or. index(var_name, 'name') > 0 .or. &
            index(var_name, 'msg') > 0 .or. index(var_name, 'text') > 0) then
            typ = create_mono_type(TCHAR)
        else if (index(var_name, 'num') > 0 .or. index(var_name, 'count') > 0 .or. &
                 index(var_name, 'idx') > 0) then
            typ = create_mono_type(TINT)
        else
            typ = create_mono_type(TVAR, var=create_type_var(next_var_id, "v"))
            next_var_id = next_var_id + 1
        end if
    end function infer_type_from_name_pattern

    recursive function infer_expression_type_static(arena, expr_index, param_names, &
                                                    param_types) result(typ)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        type(mono_type_t) :: typ
        logical, allocatable :: visiting(:)

        if (arena%size > 0) then
            allocate (visiting(arena%size))
        else
            allocate (visiting(0))
        end if
        visiting = .false.

        call infer_expression_type_static_impl(arena, expr_index, param_names, &
                                               param_types, visiting, 0, &
                                               MAX_EXPR_RECURSION_DEPTH, typ)
    end function infer_expression_type_static

    logical function expression_available(arena, expr_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index

        expression_available = .false.
        if (expr_index <= 0 .or. expr_index > arena%size) return
        if (.not. allocated(arena%entries(expr_index)%node)) return
        expression_available = .true.
    end function expression_available

    function infer_literal_expression_type(node) result(typ)
        type(literal_node), intent(in) :: node
        type(mono_type_t) :: typ

        select case (node%literal_kind)
        case (LITERAL_INTEGER)
            typ = create_mono_type(TINT)
        case (LITERAL_REAL)
            typ = create_mono_type(TREAL)
        case default
            typ%kind = 0
        end select
    end function infer_literal_expression_type

    function infer_identifier_expression_type(node, param_names, param_types) &
        result(typ)
        type(identifier_node), intent(in) :: node
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        type(mono_type_t) :: typ
        integer :: i

        typ%kind = 0
        if (allocated(node%name)) then
            do i = 1, size(param_names)
                if (trim(param_names(i)) == trim(node%name)) then
                    typ = param_types(i)
                    if (typ%kind /= 0) return
                end if
            end do
        end if
        if (node%inferred_type%kind > 0) typ = node%inferred_type
    end function infer_identifier_expression_type

    recursive subroutine infer_binary_expression_type_impl(arena, node, param_names, &
                                                           param_types, visiting, &
                                                           depth, max_depth, typ)
        type(ast_arena_t), intent(in) :: arena
        type(binary_op_node), intent(in) :: node
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        logical, intent(inout) :: visiting(:)
        integer, intent(in) :: depth
        integer, intent(in) :: max_depth
        type(mono_type_t), intent(out) :: typ
        type(mono_type_t) :: left_typ
        type(mono_type_t) :: right_typ

        typ%kind = 0
        if (depth > max_depth) return

        if (node%operator == "==" .or. node%operator == "/=" .or. &
            node%operator == "<" .or. node%operator == "<=" .or. &
            node%operator == ">" .or. node%operator == ">=") then
            typ = create_mono_type(TLOGICAL)
            return
        end if

        if (node%operator == ".and." .or. node%operator == ".or." .or. &
            node%operator == ".not." .or. node%operator == ".eqv." .or. &
            node%operator == ".neqv.") then
            typ = create_mono_type(TLOGICAL)
            return
        end if

        call infer_expression_type_static_impl(arena, node%left_index, param_names, &
                                               param_types, visiting, depth + 1, &
                                               max_depth, left_typ)
        call infer_expression_type_static_impl(arena, node%right_index, param_names, &
                                               param_types, visiting, depth + 1, &
                                               max_depth, right_typ)
        if (left_typ%kind == 0) left_typ = right_typ
        if (right_typ%kind == 0) right_typ = left_typ
        if (left_typ%kind == 0 .and. right_typ%kind == 0) then
            typ%kind = 0
        else
            typ = get_common_type(left_typ, right_typ)
        end if
    end subroutine infer_binary_expression_type_impl

    function infer_array_literal_type_from_context_impl(arena, node, param_names, &
                                                        param_types, visiting, depth, &
                                                        max_depth) result(typ)
        type(ast_arena_t), intent(in) :: arena
        type(array_literal_node), intent(in) :: node
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        logical, intent(inout) :: visiting(:)
        integer, intent(in) :: depth
        integer, intent(in) :: max_depth
        type(mono_type_t) :: typ
        type(mono_type_t) :: element_type
        type(mono_type_t), allocatable :: args(:)
        integer :: elem_count

        typ%kind = 0
        if (depth > max_depth) return

        elem_count = count_array_elements(node)
        element_type = infer_array_element_type_impl(arena, node, param_names, &
                                                     param_types, visiting, &
                                                     depth + 1, max_depth)

        allocate (args(1))
        args(1) = element_type
        if (elem_count > 0) then
            typ = create_mono_type(TARRAY, args=args, array_size=elem_count)
        else
            typ = create_mono_type(TARRAY, args=args)
        end if
    end function infer_array_literal_type_from_context_impl

    integer function count_array_elements(node) result(count)
        type(array_literal_node), intent(in) :: node

        if (allocated(node%element_indices)) then
            count = size(node%element_indices)
        else
            count = 0
        end if
    end function count_array_elements

    function infer_array_element_type_impl(arena, node, param_names, param_types, &
                                           visiting, depth, max_depth) &
        result(element_type)
        type(ast_arena_t), intent(in) :: arena
        type(array_literal_node), intent(in) :: node
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        logical, intent(inout) :: visiting(:)
        integer, intent(in) :: depth
        integer, intent(in) :: max_depth
        type(mono_type_t) :: element_type
        type(mono_type_t) :: other_type
        integer :: elem_idx
        integer :: elem_count

        element_type%kind = 0
        if (depth > max_depth) return
        elem_count = count_array_elements(node)
        if (elem_count == 0) then
            element_type = create_mono_type(TINT)
            return
        end if

        call infer_expression_type_static_impl(arena, node%element_indices(1), &
                                               param_names, param_types, visiting, &
                                               depth + 1, max_depth, element_type)
        if (element_type%kind == 0) element_type = create_mono_type(TREAL)

        do elem_idx = 2, elem_count
            call infer_expression_type_static_impl(arena, &
                                                   node%element_indices(elem_idx), &
                                                   param_names, param_types, visiting, &
                                                   depth + 1, max_depth, other_type)
            if (other_type%kind == 0) cycle
            if (element_type%kind == TARRAY .and. other_type%kind /= TARRAY) cycle
            if (element_type%kind /= TARRAY .and. other_type%kind == TARRAY) then
                element_type = other_type
            else
                element_type = get_common_type(element_type, other_type)
            end if
        end do
    end function infer_array_element_type_impl

    function infer_call_expression_type(arena, node, param_names, param_types) &
        result(typ)
        type(ast_arena_t), intent(in) :: arena
        type(call_or_subscript_node), intent(in) :: node
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        type(mono_type_t) :: typ
        integer :: i
        integer :: rank
        logical :: found_type

        typ%kind = 0
        if (node%inferred_type%kind > 0) then
            typ = node%inferred_type
            return
        end if

        if (.not. allocated(node%name)) return
        if (.not. allocated(node%arg_indices)) return

        rank = size(node%arg_indices)
        do i = 1, size(param_names)
            if (trim(param_names(i)) /= trim(node%name)) cycle
            typ = collapse_array_rank(param_types(i), rank)
            if (typ%kind == 0) typ = param_types(i)
            if (typ%kind == TARRAY) typ = collapse_array_rank(typ, rank)
            if (typ%kind == 0) typ = create_mono_type(TREAL)
            return
        end do

        found_type = find_return_type(arena, node%name, typ)
        if (.not. found_type) then
            typ = create_mono_type(TVAR, var=create_type_var(0, "unknown_call"))
        end if
    end function infer_call_expression_type

end module semantic_expression_context
