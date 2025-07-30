program test_codegen_core_comprehensive
    use codegen_core
    use ast_core
    use ast_factory
    implicit none
    
    integer :: test_count, pass_count
    
    test_count = 0
    pass_count = 0
    
    print *, "=== Codegen Core Comprehensive Tests ==="
    
    ! Test basic node code generation
    call test_literal_generation()
    call test_identifier_generation()
    call test_assignment_generation()
    call test_binary_op_generation()
    
    ! Test polymorphic generation
    call test_polymorphic_generation()
    
    ! Test edge cases
    call test_invalid_node_index()
    call test_empty_arena()
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "All codegen core tests passed!"
        stop 0
    else
        print *, "Some codegen core tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_literal_generation()
        type(ast_arena_t) :: arena
        integer :: lit_idx
        character(len=:), allocatable :: code
        
        call test_start("Literal code generation")
        
        arena = create_ast_stack()
        lit_idx = push_literal(arena, "42", LITERAL_INTEGER)
        
        code = generate_code_from_arena(arena, lit_idx)
        
        if (allocated(code) .and. len(code) > 0) then
            if (index(code, "42") > 0) then
                call test_pass()
            else
                call test_fail("Generated code missing literal value")
            end if
        else
            call test_fail("No code generated for literal")
        end if
    end subroutine test_literal_generation
    
    subroutine test_identifier_generation()
        type(ast_arena_t) :: arena
        integer :: id_idx
        character(len=:), allocatable :: code
        
        call test_start("Identifier code generation")
        
        arena = create_ast_stack()
        id_idx = push_identifier(arena, "test_var")
        
        code = generate_code_from_arena(arena, id_idx)
        
        if (allocated(code) .and. len(code) > 0) then
            if (index(code, "test_var") > 0) then
                call test_pass()
            else
                call test_fail("Generated code missing identifier name")
            end if
        else
            call test_fail("No code generated for identifier")
        end if
    end subroutine test_identifier_generation
    
    subroutine test_assignment_generation()
        type(ast_arena_t) :: arena
        integer :: id_idx, lit_idx, assign_idx
        character(len=:), allocatable :: code
        
        call test_start("Assignment code generation")
        
        arena = create_ast_stack()
        id_idx = push_identifier(arena, "x")
        lit_idx = push_literal(arena, "10", LITERAL_INTEGER)
        assign_idx = push_assignment(arena, id_idx, lit_idx)
        
        code = generate_code_from_arena(arena, assign_idx)
        
        if (allocated(code) .and. len(code) > 0) then
            if (index(code, "x") > 0 .and. index(code, "10") > 0) then
                call test_pass()
            else
                call test_fail("Assignment code missing variable or value")
            end if
        else
            call test_fail("No code generated for assignment")
        end if
    end subroutine test_assignment_generation
    
    subroutine test_binary_op_generation()
        type(ast_arena_t) :: arena
        integer :: left_idx, right_idx, binop_idx
        character(len=:), allocatable :: code
        
        call test_start("Binary operation code generation")
        
        arena = create_ast_stack()
        left_idx = push_identifier(arena, "a")
        right_idx = push_identifier(arena, "b")
        binop_idx = push_binary_op(arena, left_idx, right_idx, "+")
        
        code = generate_code_from_arena(arena, binop_idx)
        
        if (allocated(code) .and. len(code) > 0) then
            if (index(code, "a") > 0 .and. index(code, "b") > 0 .and. &
                index(code, "+") > 0) then
                call test_pass()
            else
                call test_fail("Binary op code missing operands or operator")
            end if
        else
            call test_fail("No code generated for binary operation")
        end if
    end subroutine test_binary_op_generation
    
    subroutine test_polymorphic_generation()
        type(ast_arena_t) :: arena
        integer :: lit_idx
        character(len=:), allocatable :: code
        
        call test_start("Polymorphic code generation")
        
        arena = create_ast_stack()
        lit_idx = push_literal(arena, "3.14", LITERAL_REAL)
        
        code = generate_code_polymorphic(arena, lit_idx)
        
        if (allocated(code) .and. len(code) > 0) then
            if (index(code, "3.14") > 0) then
                call test_pass()
            else
                call test_fail("Polymorphic code missing literal value")
            end if
        else
            call test_fail("No code generated polymorphically")
        end if
    end subroutine test_polymorphic_generation
    
    subroutine test_invalid_node_index()
        type(ast_arena_t) :: arena
        character(len=:), allocatable :: code
        
        call test_start("Invalid node index handling")
        
        arena = create_ast_stack()
        
        ! Test negative index
        code = generate_code_from_arena(arena, -1)
        if (.not. allocated(code) .or. len(code) == 0) then
            call test_pass()
        else
            call test_fail("Should not generate code for invalid index")
        end if
    end subroutine test_invalid_node_index
    
    subroutine test_empty_arena()
        type(ast_arena_t) :: arena
        character(len=:), allocatable :: code
        
        call test_start("Empty arena handling")
        
        arena = create_ast_stack()
        
        ! Test index beyond arena size
        code = generate_code_from_arena(arena, 1)
        if (.not. allocated(code) .or. len(code) == 0) then
            call test_pass()
        else
            call test_fail("Should not generate code from empty arena")
        end if
    end subroutine test_empty_arena
    
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
    
end program test_codegen_core_comprehensive