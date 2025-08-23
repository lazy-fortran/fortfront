module test_visitor_coverage_types
    use ast_core
    use ast_visitor
    implicit none
    
    ! Custom visitor to track include statements
    type, extends(ast_visitor_t) :: include_tracking_visitor_t
        integer :: include_count = 0
    contains
        procedure :: visit_program => track_program
        procedure :: visit_assignment => track_assignment
        procedure :: visit_binary_op => track_binary_op
        procedure :: visit_function_def => track_function_def
        procedure :: visit_subroutine_def => track_subroutine_def
        procedure :: visit_call_or_subscript => track_call_or_subscript
        procedure :: visit_subroutine_call => track_subroutine_call
        procedure :: visit_identifier => track_identifier
        procedure :: visit_literal => track_literal
        procedure :: visit_declaration => track_declaration
        procedure :: visit_print_statement => track_print_statement
        procedure :: visit_if => track_if
        procedure :: visit_do_loop => track_do_loop
        procedure :: visit_do_while => track_do_while
        procedure :: visit_select_case => track_select_case
        procedure :: visit_derived_type => track_derived_type
        procedure :: visit_interface_block => track_interface_block
        procedure :: visit_module => track_module
        procedure :: visit_use_statement => track_use_statement
        procedure :: visit_include_statement => track_include_statement
    end type include_tracking_visitor_t
    
contains
    
    ! Minimal implementations for all visitor methods
    subroutine track_program(this, node)
        class(include_tracking_visitor_t), intent(inout) :: this
        class(program_node), intent(in) :: node
    end subroutine track_program
    
    subroutine track_assignment(this, node)
        class(include_tracking_visitor_t), intent(inout) :: this
        class(assignment_node), intent(in) :: node
    end subroutine track_assignment
    
    subroutine track_binary_op(this, node)
        class(include_tracking_visitor_t), intent(inout) :: this
        class(binary_op_node), intent(in) :: node
    end subroutine track_binary_op
    
    subroutine track_function_def(this, node)
        class(include_tracking_visitor_t), intent(inout) :: this
        class(function_def_node), intent(in) :: node
    end subroutine track_function_def
    
    subroutine track_subroutine_def(this, node)
        class(include_tracking_visitor_t), intent(inout) :: this
        class(subroutine_def_node), intent(in) :: node
    end subroutine track_subroutine_def
    
    subroutine track_call_or_subscript(this, node)
        class(include_tracking_visitor_t), intent(inout) :: this
        class(call_or_subscript_node), intent(in) :: node
    end subroutine track_call_or_subscript
    
    subroutine track_subroutine_call(this, node)
        class(include_tracking_visitor_t), intent(inout) :: this
        class(subroutine_call_node), intent(in) :: node
    end subroutine track_subroutine_call
    
    subroutine track_identifier(this, node)
        class(include_tracking_visitor_t), intent(inout) :: this
        class(identifier_node), intent(in) :: node
    end subroutine track_identifier
    
    subroutine track_literal(this, node)
        class(include_tracking_visitor_t), intent(inout) :: this
        class(literal_node), intent(in) :: node
    end subroutine track_literal
    
    subroutine track_declaration(this, node)
        class(include_tracking_visitor_t), intent(inout) :: this
        class(declaration_node), intent(in) :: node
    end subroutine track_declaration
    
    subroutine track_print_statement(this, node)
        class(include_tracking_visitor_t), intent(inout) :: this
        class(print_statement_node), intent(in) :: node
    end subroutine track_print_statement
    
    subroutine track_if(this, node)
        class(include_tracking_visitor_t), intent(inout) :: this
        class(if_node), intent(in) :: node
    end subroutine track_if
    
    subroutine track_do_loop(this, node)
        class(include_tracking_visitor_t), intent(inout) :: this
        class(do_loop_node), intent(in) :: node
    end subroutine track_do_loop
    
    subroutine track_do_while(this, node)
        class(include_tracking_visitor_t), intent(inout) :: this
        class(do_while_node), intent(in) :: node
    end subroutine track_do_while
    
    subroutine track_select_case(this, node)
        class(include_tracking_visitor_t), intent(inout) :: this
        class(select_case_node), intent(in) :: node
    end subroutine track_select_case
    
    subroutine track_derived_type(this, node)
        class(include_tracking_visitor_t), intent(inout) :: this
        class(derived_type_node), intent(in) :: node
    end subroutine track_derived_type
    
    subroutine track_interface_block(this, node)
        class(include_tracking_visitor_t), intent(inout) :: this
        class(interface_block_node), intent(in) :: node
    end subroutine track_interface_block
    
    subroutine track_module(this, node)
        class(include_tracking_visitor_t), intent(inout) :: this
        class(module_node), intent(in) :: node
    end subroutine track_module
    
    subroutine track_use_statement(this, node)
        class(include_tracking_visitor_t), intent(inout) :: this
        class(use_statement_node), intent(in) :: node
    end subroutine track_use_statement
    
    subroutine track_include_statement(this, node)
        class(include_tracking_visitor_t), intent(inout) :: this
        class(include_statement_node), intent(in) :: node
        this%include_count = this%include_count + 1
    end subroutine track_include_statement
    
end module test_visitor_coverage_types

program test_visitor_coverage
    use fortfront
    use ast_core
    use ast_visitor
    use ast_traversal
    use ast_factory
    use test_visitor_coverage_types
    implicit none
    
    logical :: all_tests_passed
    
    print *, "=== Visitor Pattern Coverage Tests ==="
    print *
    
    all_tests_passed = .true.
    
    if (.not. test_include_statement_visitor()) all_tests_passed = .false.
    if (.not. test_debug_visitor_output()) all_tests_passed = .false.
    
    print *
    if (all_tests_passed) then
        print *, "All visitor coverage tests passed!"
    else
        print *, "Some visitor coverage tests failed!"
        stop 1
    end if
    
contains
    
    logical function test_include_statement_visitor()
        type(ast_arena_t) :: arena
        integer :: inc_index
        type(include_tracking_visitor_t) :: visitor
        type(include_statement_node) :: inc_node
        
        test_include_statement_visitor = .true.
        print *, "Testing include_statement visitor..."
        
        arena = create_ast_arena()
        
        ! Create an include statement node
        inc_index = push_include_statement(arena, "mymodule.inc", 1, 1)
        
        visitor = include_tracking_visitor_t()
        call traverse_preorder(arena, inc_index, visitor)
        
        if (visitor%include_count > 0) then
            print *, "  PASS: Include statement visitor called"
        else
            print *, "  FAIL: Include statement visitor not called"
            test_include_statement_visitor = .false.
        end if
        
    end function test_include_statement_visitor
    
    logical function test_debug_visitor_output()
        type(ast_arena_t) :: arena
        integer :: prog_index, inc_index, use_index
        type(debug_visitor_t) :: visitor
        type(program_node) :: prog
        type(include_statement_node) :: inc_node
        type(use_statement_node) :: use_node
        
        test_debug_visitor_output = .true.
        print *, "Testing debug visitor with include/use statements..."
        
        arena = create_ast_arena()
        
        ! Build a small AST with include and use statements
        use_index = push_use_statement(arena, "iso_fortran_env", [character(len=1)::], [character(len=1)::], .false., 1, 1)
        inc_index = push_include_statement(arena, "common.inc", 1, 1)
        
        prog_index = push_program(arena, "test_prog", [use_index, inc_index], 1, 1)
        
        visitor = debug_visitor_t()
        call traverse_preorder(arena, prog_index, visitor)
        
        if (allocated(visitor%output)) then
            print *, "  Debug output: ", trim(visitor%output)
            if (index(visitor%output, "include_statement") > 0 .and. &
                index(visitor%output, "use_statement") > 0) then
                print *, "  PASS: Debug visitor output contains expected nodes"
            else
                print *, "  FAIL: Debug visitor output missing expected nodes"
                test_debug_visitor_output = .false.
            end if
        else
            print *, "  FAIL: No debug output generated"
            test_debug_visitor_output = .false.
        end if
        
    end function test_debug_visitor_output
    
end program test_visitor_coverage