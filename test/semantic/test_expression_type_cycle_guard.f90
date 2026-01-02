program test_expression_type_cycle_guard
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_core, only: binary_op_node
    use semantic_expression_context, only: infer_expression_type_static
    use type_system_unified, only: mono_type_t
    implicit none

    type(ast_arena_t) :: arena
    type(binary_op_node) :: binop
    character(len=64), allocatable :: param_names(:)
    type(mono_type_t), allocatable :: param_types(:)
    type(mono_type_t) :: typ

    print *, "Testing infer_expression_type_static cycle guard..."

    arena = create_ast_arena()

    binop%left_index = 1
    binop%right_index = 1
    binop%operator = "+"
    call arena%push(binop, "binary_op")

    allocate (param_names(0))
    allocate (param_types(0))

    typ = infer_expression_type_static(arena, 1, param_names, param_types)
    if (typ%kind /= 0) then
        print *, "FAIL: expected unknown type for self-referential binary op"
        stop 1
    end if

    print *, "PASS: infer_expression_type_static handled self-reference safely"
end program test_expression_type_cycle_guard
