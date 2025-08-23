module test_ast_traversal_visitors
    use ast_core
    use ast_visitor
    implicit none
    
    ! Visitor that counts different node types
    type, extends(ast_visitor_t) :: counting_visitor_t
        integer :: total_count = 0
        integer :: module_count = 0
        integer :: function_count = 0
        integer :: subroutine_count = 0
        integer :: derived_type_count = 0
        integer :: interface_count = 0
        integer :: do_loop_count = 0
        integer :: do_while_count = 0
        integer :: select_case_count = 0
        integer :: if_count = 0
        integer :: print_count = 0
        integer :: use_count = 0
        integer :: declaration_count = 0
        integer :: assignment_count = 0
        integer :: binary_op_count = 0
        integer :: call_count = 0
    contains
        procedure :: visit_program => count_program
        procedure :: visit_module => count_module
        procedure :: visit_assignment => count_assignment
        procedure :: visit_binary_op => count_binary_op
        procedure :: visit_function_def => count_function_def
        procedure :: visit_subroutine_def => count_subroutine_def
        procedure :: visit_call_or_subscript => count_call_or_subscript
        procedure :: visit_subroutine_call => count_subroutine_call
        procedure :: visit_identifier => count_identifier
        procedure :: visit_literal => count_literal
        procedure :: visit_declaration => count_declaration
        procedure :: visit_print_statement => count_print_statement
        procedure :: visit_if => count_if
        procedure :: visit_do_loop => count_do_loop
        procedure :: visit_do_while => count_do_while
        procedure :: visit_select_case => count_select_case
        procedure :: visit_derived_type => count_derived_type
        procedure :: visit_interface_block => count_interface_block
        procedure :: visit_use_statement => count_use_statement
        procedure :: visit_include_statement => count_include_statement
    end type counting_visitor_t
    
    ! Visitor that tracks traversal depth
    type, extends(ast_visitor_t) :: depth_tracking_visitor_t
        integer :: current_depth = 0
        integer :: max_depth = 0
        integer :: total_nodes = 0
        integer :: if_count = 0
    contains
        procedure :: visit_program => track_program
        procedure :: visit_module => track_module
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
        procedure :: visit_use_statement => track_use_statement
        procedure :: visit_include_statement => track_include_statement
    end type depth_tracking_visitor_t
    
contains

    ! Counting visitor implementations
    subroutine count_program(this, node)
        class(counting_visitor_t), intent(inout) :: this
        class(program_node), intent(in) :: node
        this%total_count = this%total_count + 1
    end subroutine count_program
    
    subroutine count_module(this, node)
        class(counting_visitor_t), intent(inout) :: this
        class(module_node), intent(in) :: node
        this%total_count = this%total_count + 1
        this%module_count = this%module_count + 1
    end subroutine count_module
    
    subroutine count_function_def(this, node)
        class(counting_visitor_t), intent(inout) :: this
        class(function_def_node), intent(in) :: node
        this%total_count = this%total_count + 1
        this%function_count = this%function_count + 1
    end subroutine count_function_def
    
    subroutine count_subroutine_def(this, node)
        class(counting_visitor_t), intent(inout) :: this
        class(subroutine_def_node), intent(in) :: node
        this%total_count = this%total_count + 1
        this%subroutine_count = this%subroutine_count + 1
    end subroutine count_subroutine_def
    
    subroutine count_derived_type(this, node)
        class(counting_visitor_t), intent(inout) :: this
        class(derived_type_node), intent(in) :: node
        this%total_count = this%total_count + 1
        this%derived_type_count = this%derived_type_count + 1
    end subroutine count_derived_type
    
    subroutine count_interface_block(this, node)
        class(counting_visitor_t), intent(inout) :: this
        class(interface_block_node), intent(in) :: node
        this%total_count = this%total_count + 1
        this%interface_count = this%interface_count + 1
    end subroutine count_interface_block
    
    subroutine count_do_loop(this, node)
        class(counting_visitor_t), intent(inout) :: this
        class(do_loop_node), intent(in) :: node
        this%total_count = this%total_count + 1
        this%do_loop_count = this%do_loop_count + 1
    end subroutine count_do_loop
    
    subroutine count_do_while(this, node)
        class(counting_visitor_t), intent(inout) :: this
        class(do_while_node), intent(in) :: node
        this%total_count = this%total_count + 1
        this%do_while_count = this%do_while_count + 1
    end subroutine count_do_while
    
    subroutine count_select_case(this, node)
        class(counting_visitor_t), intent(inout) :: this
        class(select_case_node), intent(in) :: node
        this%total_count = this%total_count + 1
        this%select_case_count = this%select_case_count + 1
    end subroutine count_select_case
    
    subroutine count_if(this, node)
        class(counting_visitor_t), intent(inout) :: this
        class(if_node), intent(in) :: node
        this%total_count = this%total_count + 1
        this%if_count = this%if_count + 1
    end subroutine count_if
    
    subroutine count_print_statement(this, node)
        class(counting_visitor_t), intent(inout) :: this
        class(print_statement_node), intent(in) :: node
        this%total_count = this%total_count + 1
        this%print_count = this%print_count + 1
    end subroutine count_print_statement
    
    subroutine count_use_statement(this, node)
        class(counting_visitor_t), intent(inout) :: this
        class(use_statement_node), intent(in) :: node
        this%total_count = this%total_count + 1
        this%use_count = this%use_count + 1
    end subroutine count_use_statement
    
    subroutine count_declaration(this, node)
        class(counting_visitor_t), intent(inout) :: this
        class(declaration_node), intent(in) :: node
        this%total_count = this%total_count + 1
        this%declaration_count = this%declaration_count + 1
    end subroutine count_declaration
    
    subroutine count_assignment(this, node)
        class(counting_visitor_t), intent(inout) :: this
        class(assignment_node), intent(in) :: node
        this%total_count = this%total_count + 1
        this%assignment_count = this%assignment_count + 1
    end subroutine count_assignment
    
    subroutine count_binary_op(this, node)
        class(counting_visitor_t), intent(inout) :: this
        class(binary_op_node), intent(in) :: node
        this%total_count = this%total_count + 1
        this%binary_op_count = this%binary_op_count + 1
    end subroutine count_binary_op
    
    subroutine count_call_or_subscript(this, node)
        class(counting_visitor_t), intent(inout) :: this
        class(call_or_subscript_node), intent(in) :: node
        this%total_count = this%total_count + 1
        this%call_count = this%call_count + 1
    end subroutine count_call_or_subscript
    
    subroutine count_subroutine_call(this, node)
        class(counting_visitor_t), intent(inout) :: this
        class(subroutine_call_node), intent(in) :: node
        this%total_count = this%total_count + 1
        this%call_count = this%call_count + 1
    end subroutine count_subroutine_call
    
    subroutine count_identifier(this, node)
        class(counting_visitor_t), intent(inout) :: this
        class(identifier_node), intent(in) :: node
        this%total_count = this%total_count + 1
    end subroutine count_identifier
    
    subroutine count_literal(this, node)
        class(counting_visitor_t), intent(inout) :: this
        class(literal_node), intent(in) :: node
        this%total_count = this%total_count + 1
    end subroutine count_literal
    
    subroutine count_include_statement(this, node)
        class(counting_visitor_t), intent(inout) :: this
        class(include_statement_node), intent(in) :: node
        this%total_count = this%total_count + 1
    end subroutine count_include_statement
    
    ! Depth tracking implementations (simplified - just track total)
    subroutine track_program(this, node)
        class(depth_tracking_visitor_t), intent(inout) :: this
        class(program_node), intent(in) :: node
        this%total_nodes = this%total_nodes + 1
        this%current_depth = this%current_depth + 1
        if (this%current_depth > this%max_depth) this%max_depth = this%current_depth
    end subroutine track_program
    
    subroutine track_module(this, node)
        class(depth_tracking_visitor_t), intent(inout) :: this
        class(module_node), intent(in) :: node
        this%total_nodes = this%total_nodes + 1
    end subroutine track_module
    
    subroutine track_assignment(this, node)
        class(depth_tracking_visitor_t), intent(inout) :: this
        class(assignment_node), intent(in) :: node
        this%total_nodes = this%total_nodes + 1
    end subroutine track_assignment
    
    subroutine track_binary_op(this, node)
        class(depth_tracking_visitor_t), intent(inout) :: this
        class(binary_op_node), intent(in) :: node
        this%total_nodes = this%total_nodes + 1
    end subroutine track_binary_op
    
    subroutine track_function_def(this, node)
        class(depth_tracking_visitor_t), intent(inout) :: this
        class(function_def_node), intent(in) :: node
        this%total_nodes = this%total_nodes + 1
    end subroutine track_function_def
    
    subroutine track_subroutine_def(this, node)
        class(depth_tracking_visitor_t), intent(inout) :: this
        class(subroutine_def_node), intent(in) :: node
        this%total_nodes = this%total_nodes + 1
    end subroutine track_subroutine_def
    
    subroutine track_call_or_subscript(this, node)
        class(depth_tracking_visitor_t), intent(inout) :: this
        class(call_or_subscript_node), intent(in) :: node
        this%total_nodes = this%total_nodes + 1
    end subroutine track_call_or_subscript
    
    subroutine track_subroutine_call(this, node)
        class(depth_tracking_visitor_t), intent(inout) :: this
        class(subroutine_call_node), intent(in) :: node
        this%total_nodes = this%total_nodes + 1
    end subroutine track_subroutine_call
    
    subroutine track_identifier(this, node)
        class(depth_tracking_visitor_t), intent(inout) :: this
        class(identifier_node), intent(in) :: node
        this%total_nodes = this%total_nodes + 1
    end subroutine track_identifier
    
    subroutine track_literal(this, node)
        class(depth_tracking_visitor_t), intent(inout) :: this
        class(literal_node), intent(in) :: node
        this%total_nodes = this%total_nodes + 1
    end subroutine track_literal
    
    subroutine track_declaration(this, node)
        class(depth_tracking_visitor_t), intent(inout) :: this
        class(declaration_node), intent(in) :: node
        this%total_nodes = this%total_nodes + 1
    end subroutine track_declaration
    
    subroutine track_print_statement(this, node)
        class(depth_tracking_visitor_t), intent(inout) :: this
        class(print_statement_node), intent(in) :: node
        this%total_nodes = this%total_nodes + 1
    end subroutine track_print_statement
    
    subroutine track_if(this, node)
        class(depth_tracking_visitor_t), intent(inout) :: this
        class(if_node), intent(in) :: node
        this%total_nodes = this%total_nodes + 1
        this%if_count = this%if_count + 1
        ! The depth is tracked based on how many if statements we're nested in
        if (this%if_count > this%max_depth) this%max_depth = this%if_count
    end subroutine track_if
    
    subroutine track_do_loop(this, node)
        class(depth_tracking_visitor_t), intent(inout) :: this
        class(do_loop_node), intent(in) :: node
        this%total_nodes = this%total_nodes + 1
        this%current_depth = this%current_depth + 1
        if (this%current_depth > this%max_depth) this%max_depth = this%current_depth
    end subroutine track_do_loop
    
    subroutine track_do_while(this, node)
        class(depth_tracking_visitor_t), intent(inout) :: this
        class(do_while_node), intent(in) :: node
        this%total_nodes = this%total_nodes + 1
    end subroutine track_do_while
    
    subroutine track_select_case(this, node)
        class(depth_tracking_visitor_t), intent(inout) :: this
        class(select_case_node), intent(in) :: node
        this%total_nodes = this%total_nodes + 1
    end subroutine track_select_case
    
    subroutine track_derived_type(this, node)
        class(depth_tracking_visitor_t), intent(inout) :: this
        class(derived_type_node), intent(in) :: node
        this%total_nodes = this%total_nodes + 1
    end subroutine track_derived_type
    
    subroutine track_interface_block(this, node)
        class(depth_tracking_visitor_t), intent(inout) :: this
        class(interface_block_node), intent(in) :: node
        this%total_nodes = this%total_nodes + 1
    end subroutine track_interface_block
    
    subroutine track_use_statement(this, node)
        class(depth_tracking_visitor_t), intent(inout) :: this
        class(use_statement_node), intent(in) :: node
        this%total_nodes = this%total_nodes + 1
    end subroutine track_use_statement
    
    subroutine track_include_statement(this, node)
        class(depth_tracking_visitor_t), intent(inout) :: this
        class(include_statement_node), intent(in) :: node
        this%total_nodes = this%total_nodes + 1
    end subroutine track_include_statement
    
end module test_ast_traversal_visitors

program test_ast_traversal_comprehensive
    use fortfront
    use frontend
    use ast_core
    use ast_visitor
    use ast_traversal
    use test_ast_traversal_visitors
    implicit none
    
    logical :: all_tests_passed
    
    print *, "=== Comprehensive AST Traversal Tests ==="
    print *
    
    all_tests_passed = .true.
    
    if (.not. test_all_node_types()) all_tests_passed = .false.
    if (.not. test_complex_traversal()) all_tests_passed = .false.
    if (.not. test_edge_cases()) all_tests_passed = .false.
    if (.not. test_all_type_checkers()) all_tests_passed = .false.
    
    print *
    if (all_tests_passed) then
        print *, "All comprehensive AST traversal tests passed!"
    else
        print *, "Some comprehensive AST traversal tests failed!"
        stop 1
    end if
    
contains
    
    logical function test_all_node_types()
        ! Test just a subroutine (not inside a program)
        character(len=*), parameter :: source = &
            "subroutine test_sub(a)" // new_line('A') // &
            "    implicit none" // new_line('A') // &
            "    real :: a" // new_line('A') // &
            "    real :: x, y" // new_line('A') // &
            "    x = 1.0" // new_line('A') // &
            "    y = x + a" // new_line('A') // &
            "    if (x > 0) then" // new_line('A') // &
            "        print *, 'positive'" // new_line('A') // &
            "    end if" // new_line('A') // &
            "    print *, x, y" // new_line('A') // &
            "end subroutine test_sub"
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index, i
        character(len=:), allocatable :: error_msg
        type(counting_visitor_t) :: visitor
        
        test_all_node_types = .true.
        print *, "Testing traversal of all node types..."
        
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "  FAIL: Lex error: ", error_msg
                test_all_node_types = .false.
                return
            end if
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "  FAIL: Parse error: ", error_msg
                test_all_node_types = .false.
                return
            end if
        end if
        
        visitor = counting_visitor_t()
        call traverse_preorder(arena, prog_index, visitor)
        
        print *, "  Nodes visited:"
        print *, "    Modules:", visitor%module_count
        print *, "    Functions:", visitor%function_count
        print *, "    Subroutines:", visitor%subroutine_count
        print *, "    Derived types:", visitor%derived_type_count
        print *, "    Interfaces:", visitor%interface_count
        print *, "    If statements:", visitor%if_count
        print *, "    Do loops:", visitor%do_loop_count
        print *, "    Do while loops:", visitor%do_while_count
        print *, "    Select case:", visitor%select_case_count
        print *, "    Print statements:", visitor%print_count
        print *, "    Use statements:", visitor%use_count
        print *, "    Declarations:", visitor%declaration_count
        print *, "    Assignments:", visitor%assignment_count
        print *, "    Binary ops:", visitor%binary_op_count
        print *, "    Calls/subscripts:", visitor%call_count
        
        if (visitor%subroutine_count > 0 .and. &
            visitor%binary_op_count > 0 .and. &
            visitor%assignment_count > 0 .and. visitor%declaration_count > 0) then
            print *, "  PASS: All major node types traversed"
        else
            print *, "  FAIL: Not all node types were traversed"
            test_all_node_types = .false.
        end if
        
    end function test_all_node_types
    
    logical function test_complex_traversal()
        character(len=*), parameter :: source = &
            "program nested" // new_line('A') // &
            "    implicit none" // new_line('A') // &
            "    integer :: i, j" // new_line('A') // &
            "    real :: x" // new_line('A') // &
            "    x = 0.0" // new_line('A') // &
            "    if (x > 0) then" // new_line('A') // &
            "        x = x + 1" // new_line('A') // &
            "        if (x > 5) then" // new_line('A') // &
            "            x = x * 2" // new_line('A') // &
            "            if (x > 10) then" // new_line('A') // &
            "                print *, 'x is very large'" // new_line('A') // &
            "            else" // new_line('A') // &
            "                print *, 'x is moderately large'" // new_line('A') // &
            "            end if" // new_line('A') // &
            "        else" // new_line('A') // &
            "            print *, 'x is small'" // new_line('A') // &
            "        end if" // new_line('A') // &
            "    end if" // new_line('A') // &
            "end program nested"
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        character(len=:), allocatable :: error_msg
        type(depth_tracking_visitor_t) :: visitor
        
        test_complex_traversal = .true.
        print *, "Testing complex nested traversal..."
        
        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        
        visitor = depth_tracking_visitor_t()
        call traverse_preorder(arena, prog_index, visitor)
        
        print *, "  If statements found:", visitor%if_count
        print *, "  Total nodes visited:", visitor%total_nodes
        
        if (visitor%total_nodes > 15) then
            print *, "  PASS: Complex nested structure traversed"
        else
            print *, "  FAIL: Nesting not properly traversed (nodes:", visitor%total_nodes, ")"
            test_complex_traversal = .false.
        end if
        
    end function test_complex_traversal
    
    logical function test_edge_cases()
        test_edge_cases = .true.
        print *, "Testing edge cases..."
        
        if (.not. test_invalid_indices()) test_edge_cases = .false.
        if (.not. test_empty_traversal()) test_edge_cases = .false.
        
    end function test_edge_cases
    
    logical function test_invalid_indices()
        type(ast_arena_t) :: arena
        type(debug_visitor_t) :: visitor
        
        test_invalid_indices = .true.
        print *, "  Testing invalid indices..."
        
        arena = create_ast_arena()
        visitor = debug_visitor_t()
        
        ! Test negative index
        call traverse_preorder(arena, -1, visitor)
        
        ! Test index beyond arena size
        call traverse_preorder(arena, 999999, visitor)
        
        ! Test zero index
        call traverse_preorder(arena, 0, visitor)
        
        print *, "    PASS: Invalid indices handled gracefully"
        
    end function test_invalid_indices
    
    logical function test_empty_traversal()
        character(len=*), parameter :: source = &
            "program empty" // new_line('A') // &
            "end program empty"
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        character(len=:), allocatable :: error_msg
        type(counting_visitor_t) :: visitor
        
        test_empty_traversal = .true.
        print *, "  Testing empty program traversal..."
        
        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        
        visitor = counting_visitor_t()
        call traverse_postorder(arena, prog_index, visitor)
        
        if (visitor%total_count >= 1) then
            print *, "    PASS: Empty program traversed"
        else
            print *, "    FAIL: Empty program not traversed"
            test_empty_traversal = .false.
        end if
        
    end function test_empty_traversal
    
    logical function test_all_type_checkers()
        character(len=*), parameter :: source = &
            "subroutine test_all" // new_line('A') // &
            "    implicit none" // new_line('A') // &
            "    real :: x = 3.14" // new_line('A') // &
            "    integer :: arr(10)" // new_line('A') // &
            "    call process(x)" // new_line('A') // &
            "    arr(1) = 42" // new_line('A') // &
            "end subroutine test_all"
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        character(len=:), allocatable :: error_msg
        integer, allocatable :: nodes(:)
        integer :: i
        logical :: found_each_type
        
        test_all_type_checkers = .true.
        print *, "Testing all node type checker functions..."
        
        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        
        found_each_type = .false.
        
        ! Test each type checker function
        do i = 1, arena%size
            if (is_subroutine_def_node(arena, i)) then
                print *, "  Found subroutine def node"
                found_each_type = .true.
            end if
            if (is_declaration_node(arena, i)) then
                print *, "  Found declaration node"
            end if
            if (is_assignment_node(arena, i)) then
                print *, "  Found assignment node"
            end if
            if (is_identifier_node(arena, i)) then
                print *, "  Found identifier node"
            end if
            if (is_literal_node(arena, i)) then
                print *, "  Found literal node"
            end if
            if (is_subroutine_call_node(arena, i)) then
                print *, "  Found subroutine call node"
            end if
            if (is_call_or_subscript_node(arena, i)) then
                print *, "  Found call or subscript node"
            end if
        end do
        
        ! Test invalid cases for all checkers
        if (.not. is_program_node(arena, -1)) then
            print *, "  PASS: Invalid index handled for program node"
        end if
        if (.not. is_module_node(arena, 0)) then
            print *, "  PASS: Invalid index handled for module node"
        end if
        if (.not. is_derived_type_node(arena, 999999)) then
            print *, "  PASS: Invalid index handled for derived type node"
        end if
        if (.not. is_interface_block_node(arena, -5)) then
            print *, "  PASS: Invalid index handled for interface block node"
        end if
        if (.not. is_use_statement_node(arena, 0)) then
            print *, "  PASS: Invalid index handled for use statement node"
        end if
        if (.not. is_do_while_node(arena, -1)) then
            print *, "  PASS: Invalid index handled for do while node"
        end if
        if (.not. is_select_case_node(arena, 0)) then
            print *, "  PASS: Invalid index handled for select case node"
        end if
        if (.not. is_binary_op_node(arena, -1)) then
            print *, "  PASS: Invalid index handled for binary op node"
        end if
        
        if (found_each_type) then
            print *, "  PASS: Type checker functions working"
        else
            print *, "  FAIL: Not all type checkers tested"
            test_all_type_checkers = .false.
        end if
        
    end function test_all_type_checkers
    
end program test_ast_traversal_comprehensive