program test_ast_operations_direct
    use ast_operations
    use ast_core
    implicit none
    
    integer :: test_count, pass_count
    
    test_count = 0
    pass_count = 0
    
    print *, "=== AST Operations Direct Tests ==="
    
    ! Test AST operations functions
    call test_arena_creation()
    call test_literal_creation()
    call test_program_creation()
    call test_node_type_detection()
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "All AST operations tests passed!"
        stop 0
    else
        print *, "Some AST operations tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_arena_creation()
        type(ast_arena_t) :: arena
        
        call test_start("Create AST arena")
        
        arena = create_ast_arena()
        
        if (arena%size == 0 .and. arena%capacity > 0) then
            call test_pass()
        else
            call test_fail("Arena not created properly")
        end if
    end subroutine test_arena_creation
    
    subroutine test_literal_creation()
        type(ast_arena_t) :: arena
        integer :: node_id
        
        call test_start("Create literal node")
        
        arena = create_ast_arena()
        node_id = create_literal_node(arena, "42", INT_TYPE)
        
        if (node_id > 0 .and. arena%size == 1) then
            call test_pass()
        else
            call test_fail("Literal node not created")
        end if
    end subroutine test_literal_creation
    
    subroutine test_program_creation()
        type(ast_arena_t) :: arena
        integer :: lit_id, prog_id
        integer :: body(1)
        
        call test_start("Create program node")
        
        arena = create_ast_arena()
        lit_id = create_literal_node(arena, "0", INT_TYPE)
        body(1) = lit_id
        prog_id = create_program_node(arena, "test_prog", body)
        
        if (prog_id > 0 .and. arena%size == 2) then
            call test_pass()
        else
            call test_fail("Program node not created")
        end if
    end subroutine test_program_creation
    
    subroutine test_node_type_detection()
        type(ast_arena_t) :: arena
        integer :: lit_id, prog_id, node_type
        integer :: body(1)
        
        call test_start("Detect node type")
        
        arena = create_ast_arena()
        lit_id = create_literal_node(arena, "123", INT_TYPE)
        body(1) = lit_id
        prog_id = create_program_node(arena, "main", body)
        
        node_type = get_node_type(arena, prog_id)
        
        if (node_type == NODE_PROGRAM) then
            call test_pass()
        else
            call test_fail("Wrong node type detected")
        end if
    end subroutine test_node_type_detection
    
    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A,A)', advance='no') "Testing: ", test_name
    end subroutine test_start
    
    subroutine test_pass()
        print *, " ... PASSED"
        pass_count = pass_count + 1
    end subroutine test_pass
    
    subroutine test_fail(reason)
        character(len=*), intent(in) :: reason
        print *, " ... FAILED"
        print *, "  Reason: ", reason
    end subroutine test_fail
    
end program test_ast_operations_direct