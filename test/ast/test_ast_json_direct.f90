program test_ast_json_direct
    use ast_json
    use ast_types
    use json_module
    implicit none
    
    integer :: test_count, pass_count
    
    test_count = 0
    pass_count = 0
    
    print *, "=== AST JSON Direct Tests ==="
    
    ! Test JSON serialization of AST nodes
    call test_literal_to_json()
    call test_identifier_to_json()
    call test_assignment_to_json()
    call test_program_to_json()
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "All AST JSON tests passed!"
        stop 0
    else
        print *, "Some AST JSON tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_literal_to_json()
        type(literal_node) :: lit
        type(json_core) :: json
        type(json_value), pointer :: root, result
        character(len=:), allocatable :: json_str
        
        call test_start("Literal to JSON")
        
        ! Create a literal node
        lit%value = "42"
        lit%kind = LITERAL_INTEGER
        lit%line = 1
        lit%column = 5
        
        ! Convert to JSON
        call json%initialize()
        call json%create_object(root, '')
        call literal_to_json(lit, json, root)
        
        ! Convert to string
        call json%print(root, json_str)
        
        if (index(json_str, '"type":"literal"') > 0 .and. &
            index(json_str, '"value":"42"') > 0 .and. &
            index(json_str, '"literal_kind":"integer"') > 0) then
            call test_pass()
        else
            call test_fail("JSON missing expected fields")
        end if
        
        call json%destroy(root)
    end subroutine test_literal_to_json
    
    subroutine test_identifier_to_json()
        type(identifier_node) :: ident
        type(json_core) :: json
        type(json_value), pointer :: root
        character(len=:), allocatable :: json_str
        
        call test_start("Identifier to JSON")
        
        ! Create identifier node
        ident%name = "my_var"
        ident%line = 2
        ident%column = 10
        
        ! Convert to JSON
        call json%initialize()
        call json%create_object(root, '')
        call identifier_to_json(ident, json, root)
        
        call json%print(root, json_str)
        
        if (index(json_str, '"type":"identifier"') > 0 .and. &
            index(json_str, '"name":"my_var"') > 0) then
            call test_pass()
        else
            call test_fail("Identifier JSON incorrect")
        end if
        
        call json%destroy(root)
    end subroutine test_identifier_to_json
    
    subroutine test_assignment_to_json()
        type(assignment_node) :: assign
        type(identifier_node), target :: lhs
        type(literal_node), target :: rhs
        type(ast_node_wrapper) :: lhs_wrap, rhs_wrap
        type(json_core) :: json
        type(json_value), pointer :: root
        character(len=:), allocatable :: json_str
        
        call test_start("Assignment to JSON")
        
        ! Create assignment: x = 10
        lhs%name = "x"
        lhs%line = 3
        lhs%column = 1
        
        rhs%value = "10"
        rhs%kind = LITERAL_INTEGER
        rhs%line = 3
        rhs%column = 5
        
        lhs_wrap%node => lhs
        rhs_wrap%node => rhs
        
        assign%left = lhs_wrap
        assign%right = rhs_wrap
        assign%line = 3
        assign%column = 3
        
        ! Convert to JSON
        call json%initialize()
        call json%create_object(root, '')
        call assignment_to_json(assign, json, root)
        
        call json%print(root, json_str)
        
        if (index(json_str, '"type":"assignment"') > 0) then
            call test_pass()
        else
            call test_fail("Assignment JSON incorrect")
        end if
        
        call json%destroy(root)
    end subroutine test_assignment_to_json
    
    subroutine test_program_to_json()
        type(program_node) :: prog
        type(json_core) :: json
        type(json_value), pointer :: root
        character(len=:), allocatable :: json_str
        
        call test_start("Program to JSON")
        
        ! Create empty program
        prog%name = "test_prog"
        prog%line = 1
        prog%column = 1
        allocate(prog%body(0))
        
        ! Convert to JSON
        call json%initialize()
        call json%create_object(root, '')
        call program_to_json(prog, json, root)
        
        call json%print(root, json_str)
        
        if (index(json_str, '"type":"program"') > 0 .and. &
            index(json_str, '"name":"test_prog"') > 0 .and. &
            index(json_str, '"body":[]') > 0) then
            call test_pass()
        else
            call test_fail("Program JSON incorrect")
        end if
        
        call json%destroy(root)
    end subroutine test_program_to_json
    
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
    
end program test_ast_json_direct