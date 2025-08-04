program test_subroutine_standardization
    use ast_core
    use ast_factory
    use standardizer
    use codegen_core
    implicit none
    
    logical :: all_tests_passed
    
    print *, "=== Subroutine Standardization Tests ==="
    
    all_tests_passed = .true.
    
    ! Test subroutine standardization
    if (.not. test_subroutine_with_params()) all_tests_passed = .false.
    if (.not. test_subroutine_implicit_none()) all_tests_passed = .false.
    if (.not. test_subroutine_param_type_standardization()) all_tests_passed = .false.
    if (.not. test_subroutine_empty_params()) all_tests_passed = .false.
    if (.not. test_subroutine_with_existing_implicit_none()) all_tests_passed = .false.
    
    if (all_tests_passed) then
        print *, "All subroutine standardization tests passed!"
        stop 0
    else
        print *, "Some subroutine standardization tests failed!"
        stop 1
    end if

contains

    function test_subroutine_with_params() result(passed)
        logical :: passed
        type(ast_arena_t) :: arena
        type(subroutine_def_node) :: sub_def
        type(identifier_node) :: param1, param2
        type(assignment_node) :: assign_stmt
        type(identifier_node) :: var_ref, result_var
        type(literal_node) :: value
        character(len=:), allocatable :: code_result
        integer :: sub_index, param1_index, param2_index, assign_index
        integer, allocatable :: param_indices(:), body_indices(:)
        
        passed = .true.
        
        ! Create arena
        arena = create_ast_arena()
        
        ! Create parameters
        param1 = create_identifier("x", 1, 1)
        call arena%push(param1, "param", 0)
        param1_index = arena%size
        
        param2 = create_identifier("y", 1, 1)
        call arena%push(param2, "param", 0)
        param2_index = arena%size
        
        ! Create assignment statement: y = x
        var_ref = create_identifier("x", 2, 1)
        call arena%push(var_ref, "var_ref", 0)
        
        result_var = create_identifier("y", 2, 1)
        call arena%push(result_var, "result_var", 0)
        
        assign_stmt = create_assignment(arena%size - 1, arena%size, 2, 1)
        call arena%push(assign_stmt, "assignment", 0)
        assign_index = arena%size
        
        ! Create subroutine
        allocate(param_indices(2))
        param_indices(1) = param1_index
        param_indices(2) = param2_index
        
        allocate(body_indices(1))
        body_indices(1) = assign_index
        
        sub_def%name = "compute"
        sub_def%param_indices = param_indices
        sub_def%body_indices = body_indices
        sub_def%line = 1
        sub_def%column = 1
        
        call arena%push(sub_def, "subroutine", 0)
        sub_index = arena%size
        
        ! Standardize the AST
        call standardize_ast(arena, sub_index)
        
        ! Generate code
        code_result = generate_code_from_arena(arena, sub_index)
        
        ! Check that implicit none was added
        if (index(code_result, "implicit none") == 0) then
            passed = .false.
            print *, "FAIL: Implicit none not added to subroutine"
        end if
        
        ! Check that parameters have declarations
        if (index(code_result, "real(8)") == 0 .and. index(code_result, "integer") == 0) then
            passed = .false.
            print *, "FAIL: Parameter declarations not added"
        end if
        
        if (passed) then
            print *, "PASS: test_subroutine_with_params"
        else
            print *, "FAIL: test_subroutine_with_params"
            print *, "Generated code:"
            print *, trim(code_result)
        end if
        
        call arena%clear()
    end function test_subroutine_with_params
    
    function test_subroutine_implicit_none() result(passed)
        logical :: passed
        type(ast_arena_t) :: arena
        type(subroutine_def_node) :: sub_def
        type(assignment_node) :: assign_stmt
        type(identifier_node) :: var_ref
        type(literal_node) :: value
        character(len=:), allocatable :: code_result
        integer :: sub_index, assign_index
        integer, allocatable :: body_indices(:)
        
        passed = .true.
        
        ! Create arena
        arena = create_ast_arena()
        
        ! Create a simple assignment for the body
        var_ref = create_identifier("result", 2, 1)
        call arena%push(var_ref, "var_ref", 0)
        
        value = create_literal("0", LITERAL_INTEGER, 2, 10)
        call arena%push(value, "value", 0)
        
        assign_stmt = create_assignment(arena%size - 1, arena%size, 2, 1)
        call arena%push(assign_stmt, "assignment", 0)
        assign_index = arena%size
        
        ! Create subroutine without parameters
        allocate(body_indices(1))
        body_indices(1) = assign_index
        
        sub_def%name = "init"
        sub_def%body_indices = body_indices
        sub_def%line = 1
        sub_def%column = 1
        
        call arena%push(sub_def, "subroutine", 0)
        sub_index = arena%size
        
        ! Standardize the AST
        call standardize_ast(arena, sub_index)
        
        ! Generate code
        code_result = generate_code_from_arena(arena, sub_index)
        
        ! Check that implicit none was added as first statement in body
        if (index(code_result, "implicit none") == 0) then
            passed = .false.
            print *, "FAIL: Implicit none not added to subroutine without params"
        end if
        
        if (passed) then
            print *, "PASS: test_subroutine_implicit_none"
        else
            print *, "FAIL: test_subroutine_implicit_none"
            print *, "Generated code:"
            print *, trim(code_result)
        end if
        
        call arena%clear()
    end function test_subroutine_implicit_none
    
    function test_subroutine_param_type_standardization() result(passed)
        logical :: passed
        type(ast_arena_t) :: arena
        type(subroutine_def_node) :: sub_def
        type(identifier_node) :: param_i, param_x
        type(declaration_node) :: decl_real
        character(len=:), allocatable :: code_result
        integer :: sub_index, param_i_index, param_x_index, decl_index
        integer, allocatable :: param_indices(:), body_indices(:)
        
        passed = .true.
        
        ! Create arena
        arena = create_ast_arena()
        
        ! Create parameters - i should be integer, x should be real
        param_i = create_identifier("i", 1, 1)
        call arena%push(param_i, "param", 0)
        param_i_index = arena%size
        
        param_x = create_identifier("x", 1, 1)
        call arena%push(param_x, "param", 0)
        param_x_index = arena%size
        
        ! Create a declaration for x with type "real" (should be standardized to real(8))
        decl_real%var_name = "x"
        decl_real%type_name = "real"
        decl_real%line = 2
        decl_real%column = 1
        call arena%push(decl_real, "declaration", 0)
        decl_index = arena%size
        
        ! Create subroutine
        allocate(param_indices(2))
        param_indices(1) = param_i_index
        param_indices(2) = param_x_index
        
        allocate(body_indices(1))
        body_indices(1) = decl_index
        
        sub_def%name = "process"
        sub_def%param_indices = param_indices
        sub_def%body_indices = body_indices
        sub_def%line = 1
        sub_def%column = 1
        
        call arena%push(sub_def, "subroutine", 0)
        sub_index = arena%size
        
        ! Standardize the AST
        call standardize_ast(arena, sub_index)
        
        ! Generate code
        code_result = generate_code_from_arena(arena, sub_index)
        
        ! Check that real was standardized to real(8)
        if (index(code_result, "real(8)") == 0) then
            passed = .false.
            print *, "FAIL: real type not standardized to real(8)"
        end if
        
        ! Check that i parameter gets integer type
        if (index(code_result, "integer") == 0) then
            passed = .false.
            print *, "FAIL: integer type not added for i parameter"
        end if
        
        if (passed) then
            print *, "PASS: test_subroutine_param_type_standardization"
        else
            print *, "FAIL: test_subroutine_param_type_standardization"
            print *, "Generated code:"
            print *, trim(code_result)
        end if
        
        call arena%clear()
    end function test_subroutine_param_type_standardization

    function test_subroutine_empty_params() result(passed)
        logical :: passed
        type(ast_arena_t) :: arena
        type(subroutine_def_node) :: sub_def
        character(len=:), allocatable :: code_result
        integer :: sub_index
        integer, allocatable :: param_indices(:), body_indices(:)
        
        passed = .true.
        
        ! Create arena
        arena = create_ast_arena()
        
        ! Create subroutine with empty parameter list
        allocate(param_indices(0))
        allocate(body_indices(0))
        
        sub_def%name = "empty_params"
        sub_def%param_indices = param_indices
        sub_def%body_indices = body_indices
        sub_def%line = 1
        sub_def%column = 1
        
        call arena%push(sub_def, "subroutine", 0)
        sub_index = arena%size
        
        ! Standardize the AST
        call standardize_ast(arena, sub_index)
        
        ! Generate code - should not crash
        code_result = generate_code_from_arena(arena, sub_index)
        
        ! Check that code was generated
        if (len_trim(code_result) == 0) then
            passed = .false.
            print *, "FAIL: No code generated for empty parameter subroutine"
        end if
        
        if (passed) then
            print *, "PASS: test_subroutine_empty_params"
        else
            print *, "FAIL: test_subroutine_empty_params"
        end if
        
        call arena%clear()
    end function test_subroutine_empty_params

    function test_subroutine_with_existing_implicit_none() result(passed)
        logical :: passed
        type(ast_arena_t) :: arena
        type(subroutine_def_node) :: sub_def
        type(literal_node) :: implicit_none_node
        character(len=:), allocatable :: code_result
        integer :: sub_index, implicit_none_index
        integer, allocatable :: body_indices(:)
        integer :: implicit_count
        
        passed = .true.
        
        ! Create arena
        arena = create_ast_arena()
        
        ! Create existing implicit none statement
        implicit_none_node%value = "implicit none"
        implicit_none_node%literal_kind = LITERAL_STRING
        implicit_none_node%line = 2
        implicit_none_node%column = 1
        call arena%push(implicit_none_node, "implicit_none", 0)
        implicit_none_index = arena%size
        
        ! Create subroutine with existing implicit none
        allocate(body_indices(1))
        body_indices(1) = implicit_none_index
        
        sub_def%name = "has_implicit"
        sub_def%body_indices = body_indices
        sub_def%line = 1
        sub_def%column = 1
        
        call arena%push(sub_def, "subroutine", 0)
        sub_index = arena%size
        
        ! Standardize the AST
        call standardize_ast(arena, sub_index)
        
        ! Generate code
        code_result = generate_code_from_arena(arena, sub_index)
        
        ! Count occurrences of "implicit none" - should only be 2 (one added, one existing)
        implicit_count = 0
        block
            integer :: pos, start_pos
            start_pos = 1
            do while (start_pos <= len(code_result))
                pos = index(code_result(start_pos:), "implicit none")
                if (pos > 0) then
                    implicit_count = implicit_count + 1
                    start_pos = start_pos + pos + 12  ! Skip past "implicit none"
                else
                    exit
                end if
            end do
        end block
        
        ! We expect 2 occurrences: one in program main, one in subroutine (but not duplicate in subroutine)
        if (implicit_count /= 2) then
            passed = .false.
            print *, "FAIL: Expected 2 'implicit none' statements (1 in program, 1 in subroutine), found", implicit_count
        end if
        
        if (passed) then
            print *, "PASS: test_subroutine_with_existing_implicit_none"
        else
            print *, "FAIL: test_subroutine_with_existing_implicit_none"
            print *, "Generated code:"
            print *, trim(code_result)
        end if
        
        call arena%clear()
    end function test_subroutine_with_existing_implicit_none

end program test_subroutine_standardization