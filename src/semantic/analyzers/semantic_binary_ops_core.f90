module semantic_binary_ops_core
    ! Binary operation type inference
    use type_system_unified, only: mono_type_t, create_mono_type, TCHAR, TREAL
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
                typ = infer_string_concatenation(left_typ, right_typ)
                call rewrite_operator(arena, binop_index, "//")
                return
            end if
        end if

        if (binop%operator == "//") then
            typ = infer_string_concatenation(left_typ, right_typ)
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
            if (typ%kind == 0) typ = left_typ
        end if
    end function infer_binary_operation

    subroutine rewrite_operator(arena, node_index, new_operator)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: new_operator

        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        select type (node => arena%entries(node_index)%node)
        type is (binary_op_node)
            node%operator = new_operator
            arena%entries(node_index)%node = node
        end select
    end subroutine rewrite_operator

end module semantic_binary_ops_core
