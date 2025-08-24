program test_ast_modular_structure
    ! Test that verifies the modular AST structure works correctly
    ! This test will initially FAIL until we implement the split modules
    
    use ast_base, only: ast_node
    use ast_core, only: ast_arena_t, create_ast_arena
    use ast_nodes_core, only: program_node, assignment_node, identifier_node, literal_node
    use ast_nodes_control, only: if_node, do_loop_node
    use ast_nodes_procedure, only: function_def_node, subroutine_call_node
    use ast_nodes_data, only: declaration_node, derived_type_node
    use ast_nodes_io, only: print_statement_node, write_statement_node
    implicit none
    
    logical :: all_tests_passed = .true.
    
    print *, "=== AST Modular Structure Tests ==="
    
    ! Test 1: Basic arena creation from ast_arena module
    if (.not. test_arena_creation()) all_tests_passed = .false.
    
    ! Test 2: Core node types from ast_nodes_core
    if (.not. test_core_nodes()) all_tests_passed = .false.
    
    ! Test 3: Control flow nodes from ast_nodes_control  
    if (.not. test_control_nodes()) all_tests_passed = .false.
    
    ! Test 4: Procedure nodes from ast_nodes_procedure
    if (.not. test_procedure_nodes()) all_tests_passed = .false.
    
    ! Test 5: Data nodes from ast_nodes_data
    if (.not. test_data_nodes()) all_tests_passed = .false.
    
    ! Test 6: I/O nodes from ast_nodes_io
    if (.not. test_io_nodes()) all_tests_passed = .false.
    
    ! Test 7: Cross-module integration
    if (.not. test_cross_module_integration()) all_tests_passed = .false.
    
    if (all_tests_passed) then
        print *, "All AST modular structure tests passed!"
    else
        print *, "Some AST modular structure tests failed!"
        stop 1
    end if

contains

    logical function test_arena_creation()
        type(ast_arena_t) :: arena
        test_arena_creation = .true.
        print *, "Testing arena creation from ast_arena module..."
        
        arena = create_ast_arena()
        
        print *, "  DEBUG: Arena size=", arena%size, "capacity=", arena%capacity
        
        if (arena%size /= 0) then
            print *, "  FAIL: Expected arena size 0, got", arena%size
            test_arena_creation = .false.
            return
        end if
        
        if (arena%capacity <= 0) then
            print *, "  FAIL: Expected positive capacity, got", arena%capacity
            test_arena_creation = .false.
            return
        end if
        
        print *, "  PASS: Arena creation from dedicated module"
    end function test_arena_creation

    logical function test_core_nodes()
        type(program_node) :: prog
        type(assignment_node) :: assign
        type(identifier_node) :: ident
        type(literal_node) :: lit
        test_core_nodes = .true.
        print *, "Testing core nodes from ast_nodes_core module..."
        
        ! Test node creation - these should be importable from ast_nodes_core
        prog%name = "test_program"
        assign%operator = "="
        ident%name = "x" 
        lit%value = "42"
        lit%literal_type = "integer"
        
        print *, "  PASS: Core AST nodes accessible from dedicated module"
    end function test_core_nodes

    logical function test_control_nodes()
        type(if_node) :: if_stmt
        type(do_loop_node) :: do_stmt
        test_control_nodes = .true.
        print *, "Testing control nodes from ast_nodes_control module..."
        
        ! Test control flow node creation
        ! These should be importable from ast_nodes_control
        
        print *, "  PASS: Control flow nodes accessible from dedicated module"
    end function test_control_nodes

    logical function test_procedure_nodes()
        type(function_def_node) :: func
        type(subroutine_call_node) :: call_stmt
        test_procedure_nodes = .true.
        print *, "Testing procedure nodes from ast_nodes_procedure module..."
        
        ! Test procedure node creation
        func%name = "test_func"
        call_stmt%name = "test_sub"
        
        print *, "  PASS: Procedure nodes accessible from dedicated module"
    end function test_procedure_nodes

    logical function test_data_nodes()
        type(declaration_node) :: decl
        type(derived_type_node) :: dtype
        test_data_nodes = .true.
        print *, "Testing data nodes from ast_nodes_data module..."
        
        ! Test data structure node creation
        decl%var_name = "test_var"
        decl%type_name = "integer"
        dtype%name = "test_type"
        
        print *, "  PASS: Data structure nodes accessible from dedicated module"
    end function test_data_nodes

    logical function test_io_nodes()
        type(print_statement_node) :: print_stmt
        type(write_statement_node) :: write_stmt
        test_io_nodes = .true.
        print *, "Testing I/O nodes from ast_nodes_io module..."
        
        ! Test I/O node creation
        
        print *, "  PASS: I/O nodes accessible from dedicated module"
    end function test_io_nodes

    logical function test_cross_module_integration()
        type(ast_arena_t) :: arena
        type(program_node) :: prog
        integer :: prog_index
        test_cross_module_integration = .true.
        print *, "Testing cross-module integration..."
        
        ! Test that we can use arena from ast_arena with nodes from ast_nodes_*
        arena = create_ast_arena()
        prog%name = "integration_test"
        
        ! This should work - pushing a node from ast_nodes_core into arena from ast_arena
        call arena%push(prog, "program")
        
        if (arena%size /= 1) then
            print *, "  FAIL: Expected arena size 1 after push, got", arena%size
            test_cross_module_integration = .false.
            return
        end if
        
        print *, "  PASS: Cross-module integration works"
    end function test_cross_module_integration

end program test_ast_modular_structure