program test_ast_factory_direct
    use ast_factory
    use ast_core
    implicit none
    
    integer :: test_count, pass_count
    
    test_count = 0
    pass_count = 0
    
    print *, "=== AST Factory Direct Tests ==="
    
    ! Test node creation functions
    call test_node_creation()
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "All AST factory tests passed!"
        stop 0
    else
        print *, "Some AST factory tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_node_creation()
        type(ast_arena_t) :: arena
        integer :: id_idx, lit_idx, assign_idx, binop_idx
        integer, allocatable :: body(:)
        
        call test_start("Identifier creation")
        arena = create_ast_arena()
        id_idx = push_identifier(arena, "test_var", line=1, column=5)
        if (id_idx > 0 .and. arena%size == 1) then
            call test_pass()
        else
            call test_fail("Identifier not created")
        end if
        
        call test_start("Literal creation")
        lit_idx = push_literal(arena, "42", LITERAL_INTEGER, line=2, column=1)
        if (lit_idx > 0 .and. arena%size == 2) then
            call test_pass()
        else
            call test_fail("Literal not created")
        end if
        
        call test_start("Assignment creation")
        assign_idx = push_assignment(arena, id_idx, lit_idx, line=3, column=1)
        if (assign_idx > 0 .and. arena%size == 3) then
            call test_pass()
        else
            call test_fail("Assignment not created")
        end if
        
        call test_start("Binary operation creation")
        binop_idx = push_binary_op(arena, id_idx, lit_idx, "+", line=4, column=3)
        if (binop_idx > 0 .and. arena%size == 4) then
            call test_pass()
        else
            call test_fail("Binary op not created")
        end if
        
        call test_start("Program node creation")
        allocate(body(1))
        body(1) = assign_idx
        block
            integer :: prog_idx
            prog_idx = push_program(arena, "test_prog", body)
            if (prog_idx > 0 .and. arena%size == 5) then
                call test_pass()
            else
                call test_fail("Program not created")
            end if
        end block
        
        call test_start("Complex literal creation")
        block
            integer :: cmplx_idx
            integer :: real_idx, imag_idx
            real_idx = push_literal(arena, "1.0", LITERAL_REAL)
            imag_idx = push_literal(arena, "2.0", LITERAL_REAL)
            cmplx_idx = push_complex_literal(arena, real_idx, imag_idx)
            if (cmplx_idx > 0) then
                call test_pass()
            else
                call test_fail("Complex literal not created")
            end if
        end block
        
        call test_start("Array literal creation")
        block
            integer :: arr_idx
            integer, allocatable :: elements(:)
            allocate(elements(3))
            elements(1) = push_literal(arena, "1", LITERAL_INTEGER)
            elements(2) = push_literal(arena, "2", LITERAL_INTEGER)
            elements(3) = push_literal(arena, "3", LITERAL_INTEGER)
            arr_idx = push_array_literal(arena, elements)
            if (arr_idx > 0) then
                call test_pass()
            else
                call test_fail("Array literal not created")
            end if
        end block
        
        call test_start("Subroutine call creation")
        block
            integer :: call_idx
            integer, allocatable :: args(:)
            allocate(args(2))
            args(1) = push_identifier(arena, "x")
            args(2) = push_identifier(arena, "y")
            call_idx = push_subroutine_call(arena, "process", args)
            if (call_idx > 0) then
                call test_pass()
            else
                call test_fail("Subroutine call not created")
            end if
        end block
        
        call test_start("Stop statement creation")
        block
            integer :: stop_idx
            stop_idx = push_stop(arena)
            if (stop_idx > 0) then
                call test_pass()
            else
                call test_fail("Stop not created")
            end if
        end block
        
        call test_start("Return statement creation")
        block
            integer :: ret_idx
            ret_idx = push_return(arena)
            if (ret_idx > 0) then
                call test_pass()
            else
                call test_fail("Return not created")
            end if
        end block
    end subroutine test_node_creation
    
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
    
end program test_ast_factory_direct