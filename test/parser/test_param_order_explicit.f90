program test_param_order_explicit
    use ast_core
    use ast_factory
    use codegen_core
    implicit none

    type(ast_arena_t) :: arena
    integer :: func_index
    integer :: param_a_idx, param_b_idx
    integer, allocatable :: param_indices(:), body_indices(:)
    character(len=:), allocatable :: code

    ! Create parameter declaration nodes with explicit intent
    param_a_idx = push_parameter_declaration(arena, "a", "real", &
                                             kind_value=8, intent_value=1, &
                                             line=1, column=1)
    param_b_idx = push_parameter_declaration(arena, "b", "real", &
                                             kind_value=8, intent_value=1, &
                                             line=1, column=2)

    ! Create function with parameters in a, b order
    param_indices = [param_a_idx, param_b_idx]
    body_indices = [param_a_idx, param_b_idx]

    func_index = push_function_def(arena, "test_func", param_indices, "real(8)", body_indices, 1, 1)

    ! Generate code
    code = generate_code_from_arena(arena, func_index)

    print *, "Generated code:"
    print *, code
    print *, ""

    ! Check order in function signature
    if (index(code, "function test_func(a, b)") > 0) then
        print *, "PASS: Parameters in correct order in function signature"
    else
        print *, "FAIL: Parameters not in correct order in function signature"
        if (index(code, "function test_func(b, a)") > 0) then
            print *, "ERROR: Parameters are REVERSED in function signature!"
        end if
    end if

    ! Check order in declaration
    if (index(code, "real(8), intent(in) :: a, b") > 0) then
        print *, "PASS: Parameters in correct order in declaration"
    else
        print *, "FAIL: Parameters not in correct order in declaration"
        if (index(code, "real(8), intent(in) :: b, a") > 0) then
            print *, "ERROR: Parameters are REVERSED in declaration!"
        end if
    end if

end program test_param_order_explicit
