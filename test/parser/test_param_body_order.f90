program test_param_body_order
    use ast_core
    use ast_factory
    use codegen_core
    implicit none

    type(ast_arena_t) :: arena
    integer :: func_index
    integer :: param_a_idx, param_b_idx, decl_a_idx, decl_b_idx
    integer, allocatable :: param_indices(:), body_indices(:)
    character(len=:), allocatable :: code

    ! Create parameter nodes for function signature
    param_a_idx = push_identifier(arena, "a", 1, 1)
    param_b_idx = push_identifier(arena, "b", 1, 2)

    ! Create parameter declaration nodes for body
    decl_a_idx = push_parameter_declaration(arena, "a", "real", &
                                            kind_value=8, intent_value=1, &
                                            line=2, column=1)
    decl_b_idx = push_parameter_declaration(arena, "b", "real", &
                                            kind_value=8, intent_value=1, &
                                            line=2, column=2)

    ! Test 1: Parameters in correct order
    print *, "Test 1: Body declarations in a, b order"
    param_indices = [param_a_idx, param_b_idx]
    body_indices = [decl_a_idx, decl_b_idx]
    func_index = push_function_def(arena, "test1", param_indices, "real(8)", body_indices, 1, 1)
    code = generate_code_from_arena(arena, func_index)
    print *, trim(code)
    print *, ""

    ! Test 2: Body declarations in reverse order
    print *, "Test 2: Body declarations in b, a order"
    body_indices = [decl_b_idx, decl_a_idx]
    func_index = push_function_def(arena, "test2", param_indices, "real(8)", body_indices, 1, 1)
    code = generate_code_from_arena(arena, func_index)
    print *, trim(code)

end program test_param_body_order
