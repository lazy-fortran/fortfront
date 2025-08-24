program test_source_reconstruction_ast_nodes
    use source_reconstruction_analyzer, only: source_location_t, &
                                              source_context_t, &
                                              strategy_dispatcher_t
    use ast_core, only: ast_arena_t, create_ast_arena, &
                        create_program, program_node, &
                        create_identifier, identifier_node, &
                        literal_node, LITERAL_INTEGER, LITERAL_REAL, &
                        create_literal, assignment_node, binary_op_node, &
                        create_assignment, if_node, create_if, &
                        do_loop_node, create_do_loop, &
                        function_def_node, create_function_def, &
                        declaration_node, create_declaration
    implicit none

    integer :: total_tests, passed_tests

    total_tests = 0
    passed_tests = 0

    print *, "=== Source Reconstruction AST Node Tests ==="
    print *, ""

    ! Test 1: Reconstruct identifier node
    call test_start("Reconstruct identifier node")
    block
        type(strategy_dispatcher_t) :: dispatcher
        type(source_context_t) :: context
        type(ast_arena_t) :: arena
        type(identifier_node) :: id_node
        character(:), allocatable :: result
        integer :: node_index
        
        call dispatcher%initialize_default_strategies()
        arena = create_ast_arena()
        
        id_node = create_identifier("my_variable", line=1, column=1)
        call arena%push(id_node, "identifier")
        node_index = arena%current_index
        
        result = dispatcher%reconstruct_node(context, arena, node_index)
        
        if (result == "my_variable") then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: 'my_variable'"
            print *, "  Got: '", result, "'"
        end if
    end block

    ! Test 2: Reconstruct integer literal node
    call test_start("Reconstruct integer literal node")
    block
        type(strategy_dispatcher_t) :: dispatcher
        type(source_context_t) :: context
        type(ast_arena_t) :: arena
        type(literal_node) :: lit_node
        character(:), allocatable :: result
        integer :: node_index
        
        call dispatcher%initialize_default_strategies()
        arena = create_ast_arena()
        
        lit_node = create_literal("42", LITERAL_INTEGER, line=1, column=1)
        call arena%push(lit_node, "literal")
        node_index = arena%current_index
        
        result = dispatcher%reconstruct_node(context, arena, node_index)
        
        if (result == "42") then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: '42'"
            print *, "  Got: '", result, "'"
        end if
    end block

    ! Test 3: Reconstruct real literal node
    call test_start("Reconstruct real literal node")
    block
        type(strategy_dispatcher_t) :: dispatcher
        type(source_context_t) :: context
        type(ast_arena_t) :: arena
        type(literal_node) :: lit_node
        character(:), allocatable :: result
        integer :: node_index
        
        call dispatcher%initialize_default_strategies()
        arena = create_ast_arena()
        
        lit_node = create_literal("3.14159", LITERAL_REAL, line=1, column=1)
        call arena%push(lit_node, "literal")
        node_index = arena%current_index
        
        result = dispatcher%reconstruct_node(context, arena, node_index)
        
        if (result == "3.14159") then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: '3.14159'"
            print *, "  Got: '", result, "'"
        end if
    end block

    ! Test 4: Reconstruct program node
    call test_start("Reconstruct program node")
    block
        type(strategy_dispatcher_t) :: dispatcher
        type(source_context_t) :: context
        type(ast_arena_t) :: arena
        type(program_node) :: prog_node
        character(:), allocatable :: result
        integer :: node_index
        
        call dispatcher%initialize_default_strategies()
        arena = create_ast_arena()
        
        prog_node = create_program("test_program", [integer::], line=1, column=1)
        call arena%push(prog_node, "program")
        node_index = arena%current_index
        
        result = dispatcher%reconstruct_node(context, arena, node_index)
        
        ! Should generate some form of program reconstruction
        if (len(result) > 0) then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: non-empty program reconstruction"
            print *, "  Got: '", result, "'"
        end if
    end block

    ! Test 5: Reconstruct assignment node
    call test_start("Reconstruct assignment node")
    block
        type(strategy_dispatcher_t) :: dispatcher
        type(source_context_t) :: context
        type(ast_arena_t) :: arena
        type(assignment_node) :: assign_node
        character(:), allocatable :: result
        integer :: node_index
        
        call dispatcher%initialize_default_strategies()
        arena = create_ast_arena()
        
        assign_node = create_assignment(1, 2, line=1, column=1)  ! lhs=1, rhs=2
        call arena%push(assign_node, "assignment")
        node_index = arena%current_index
        
        result = dispatcher%reconstruct_node(context, arena, node_index)
        
        ! Should generate some form of assignment reconstruction
        if (len(result) > 0) then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: non-empty assignment reconstruction"
            print *, "  Got: '", result, "'"
        end if
    end block

    ! Test 6: Reconstruct if node (simplified)
    call test_start("Reconstruct if node")
    block
        type(strategy_dispatcher_t) :: dispatcher
        type(source_context_t) :: context
        type(ast_arena_t) :: arena
        type(if_node) :: if_construct
        character(:), allocatable :: result
        integer :: node_index
        
        call dispatcher%initialize_default_strategies()
        arena = create_ast_arena()
        
        if_construct = create_if(1, line=1, column=1)
        call arena%push(if_construct, "if")
        node_index = arena%current_index
        
        result = dispatcher%reconstruct_node(context, arena, node_index)
        
        ! Should generate some form of if reconstruction
        if (len(result) > 0) then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: non-empty if reconstruction"
            print *, "  Got: '", result, "'"
        end if
    end block

    ! Test 7: Reconstruct do loop node
    call test_start("Reconstruct do loop node")
    block
        type(strategy_dispatcher_t) :: dispatcher
        type(source_context_t) :: context
        type(ast_arena_t) :: arena
        type(do_loop_node) :: do_construct
        character(:), allocatable :: result
        integer :: node_index
        
        call dispatcher%initialize_default_strategies()
        arena = create_ast_arena()
        
        do_construct = create_do_loop("i", 1, 2, line=1, column=1)
        call arena%push(do_construct, "do_loop")
        node_index = arena%current_index
        
        result = dispatcher%reconstruct_node(context, arena, node_index)
        
        ! Should generate some form of do loop reconstruction
        if (len(result) > 0) then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: non-empty do loop reconstruction"
            print *, "  Got: '", result, "'"
        end if
    end block

    ! Test 8: Reconstruct function definition node
    call test_start("Reconstruct function definition node")
    block
        type(strategy_dispatcher_t) :: dispatcher
        type(source_context_t) :: context
        type(ast_arena_t) :: arena
        type(function_def_node) :: func_node
        character(:), allocatable :: result
        integer :: node_index
        
        call dispatcher%initialize_default_strategies()
        arena = create_ast_arena()
        
        func_node = create_function_def("my_function", return_type="integer", &
                                       line=1, column=1)
        call arena%push(func_node, "function_def")
        node_index = arena%current_index
        
        result = dispatcher%reconstruct_node(context, arena, node_index)
        
        ! Should generate some form of function reconstruction
        if (len(result) > 0) then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: non-empty function reconstruction"
            print *, "  Got: '", result, "'"
        end if
    end block

    ! Test 9: Reconstruct declaration node
    call test_start("Reconstruct declaration node")
    block
        type(strategy_dispatcher_t) :: dispatcher
        type(source_context_t) :: context
        type(ast_arena_t) :: arena
        type(declaration_node) :: decl_node
        character(:), allocatable :: result
        integer :: node_index
        
        call dispatcher%initialize_default_strategies()
        arena = create_ast_arena()
        
        decl_node = create_declaration("integer", "x", line=1, column=1)
        call arena%push(decl_node, "declaration")
        node_index = arena%current_index
        
        result = dispatcher%reconstruct_node(context, arena, node_index)
        
        ! Should generate some form of declaration reconstruction
        if (len(result) > 0) then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: non-empty declaration reconstruction"
            print *, "  Got: '", result, "'"
        end if
    end block

    ! Test 10: Reconstruct unknown node type
    call test_start("Reconstruct unknown node type")
    block
        type(strategy_dispatcher_t) :: dispatcher
        type(source_context_t) :: context
        type(ast_arena_t) :: arena
        type(identifier_node) :: id_node
        character(:), allocatable :: result
        integer :: node_index
        
        call dispatcher%initialize_default_strategies()
        arena = create_ast_arena()
        
        ! Create node with unknown type
        id_node = create_identifier("test", line=1, column=1)
        call arena%push(id_node, "unknown_type")
        node_index = arena%current_index
        
        result = dispatcher%reconstruct_node(context, arena, node_index)
        
        ! Should indicate unknown node type
        if (index(result, "unknown_node_type") > 0) then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: contains 'unknown_node_type'"
            print *, "  Got: '", result, "'"
        end if
    end block

    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", passed_tests, "/", total_tests, " tests"

    if (passed_tests == total_tests) then
        print *, "All source reconstruction AST node tests passed!"
        stop 0
    else
        print *, "Some source reconstruction AST node tests failed!"
        stop 1
    end if

contains

    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        total_tests = total_tests + 1
        write(*, '(A,A)', advance='no') "Testing: ", test_name
    end subroutine test_start

    subroutine test_pass()
        print *, " ... PASSED"
        passed_tests = passed_tests + 1
    end subroutine test_pass

    subroutine test_fail()
        print *, " ... FAILED"
    end subroutine test_fail

end program test_source_reconstruction_ast_nodes