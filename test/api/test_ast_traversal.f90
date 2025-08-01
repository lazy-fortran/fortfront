program test_ast_traversal
    use fortfront
    use frontend
    use ast_core
    use ast_visitor
    use ast_traversal
    implicit none
    
    logical :: all_tests_passed
    
    print *, "=== AST Traversal Tests ==="
    print *
    
    all_tests_passed = .true.
    
    if (.not. test_preorder_traversal()) all_tests_passed = .false.
    if (.not. test_postorder_traversal()) all_tests_passed = .false.
    if (.not. test_node_type_checking()) all_tests_passed = .false.
    if (.not. test_visitor_pattern()) all_tests_passed = .false.
    
    print *
    if (all_tests_passed) then
        print *, "All AST traversal tests passed!"
    else
        print *, "Some AST traversal tests failed!"
        stop 1
    end if
    
contains
    
    logical function test_preorder_traversal()
        character(len=*), parameter :: source = &
            "program test" // new_line('A') // &
            "    implicit none" // new_line('A') // &
            "    integer :: x" // new_line('A') // &
            "    x = 5 + 3" // new_line('A') // &
            "end program test"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        character(len=:), allocatable :: error_msg
        type(debug_visitor_t) :: visitor
        
        test_preorder_traversal = .true.
        print *, "Testing pre-order traversal..."
        
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "  FAIL: Lex error: ", error_msg
                test_preorder_traversal = .false.
                return
            end if
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "  FAIL: Parse error: ", error_msg
                test_preorder_traversal = .false.
                return
            end if
        end if
        
        visitor = debug_visitor_t()
        call traverse_preorder(arena, prog_index, visitor)
        
        if (allocated(visitor%output)) then
            print *, "  PASS: Pre-order traversal completed"
            ! Could check specific output patterns here
        else
            print *, "  FAIL: Pre-order traversal produced no output"
            test_preorder_traversal = .false.
        end if
        
    end function test_preorder_traversal
    
    logical function test_postorder_traversal()
        character(len=*), parameter :: source = &
            "program test" // new_line('A') // &
            "    integer :: y" // new_line('A') // &
            "    y = 10" // new_line('A') // &
            "end program test"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        character(len=:), allocatable :: error_msg
        type(debug_visitor_t) :: visitor
        
        test_postorder_traversal = .true.
        print *, "Testing post-order traversal..."
        
        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        
        visitor = debug_visitor_t()
        call traverse_postorder(arena, prog_index, visitor)
        
        if (allocated(visitor%output)) then
            print *, "  PASS: Post-order traversal completed"
        else
            print *, "  FAIL: Post-order traversal produced no output"
            test_postorder_traversal = .false.
        end if
        
    end function test_postorder_traversal
    
    logical function test_node_type_checking()
        character(len=*), parameter :: source = &
            "program test" // new_line('A') // &
            "    real :: z" // new_line('A') // &
            "    if (z > 0) then" // new_line('A') // &
            "        print *, z" // new_line('A') // &
            "    end if" // new_line('A') // &
            "end program test"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        character(len=:), allocatable :: error_msg
        integer, allocatable :: if_nodes(:), print_nodes(:)
        
        test_node_type_checking = .true.
        print *, "Testing node type checking functions..."
        
        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        
        ! Test program node check
        if (is_program_node(arena, prog_index)) then
            print *, "  PASS: Program node correctly identified"
        else
            print *, "  FAIL: Program node not identified"
            test_node_type_checking = .false.
        end if
        
        ! Find if nodes using arena method
        if_nodes = arena%find_by_type("if")
        if (size(if_nodes) > 0) then
            if (is_if_node(arena, if_nodes(1))) then
                print *, "  PASS: If node correctly identified"
            else
                print *, "  FAIL: If node not identified"
                test_node_type_checking = .false.
            end if
        end if
        
        ! Test print statement node
        print_nodes = arena%find_by_type("print_statement")
        if (size(print_nodes) > 0) then
            if (is_print_statement_node(arena, print_nodes(1))) then
                print *, "  PASS: Print statement node correctly identified"
            else
                print *, "  FAIL: Print statement node not identified"
                test_node_type_checking = .false.
            end if
        end if
        
    end function test_node_type_checking
    
    logical function test_visitor_pattern()
        character(len=*), parameter :: source = &
            "function add(a, b)" // new_line('A') // &
            "    real :: add" // new_line('A') // &
            "    real :: a, b" // new_line('A') // &
            "    add = a + b" // new_line('A') // &
            "end function add"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        character(len=:), allocatable :: error_msg
        type(debug_visitor_t) :: visitor
        
        test_visitor_pattern = .true.
        print *, "Testing visitor pattern with function..."
        
        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        
        visitor = debug_visitor_t()
        call traverse_ast_visitor(arena, prog_index, visitor)
        
        if (allocated(visitor%output)) then
            ! Check if function was visited
            if (index(visitor%output, "function_def") > 0) then
                print *, "  PASS: Function definition visited"
            else
                print *, "  FAIL: Function definition not visited"
                test_visitor_pattern = .false.
            end if
        else
            print *, "  FAIL: Visitor produced no output"
            test_visitor_pattern = .false.
        end if
        
    end function test_visitor_pattern
    
end program test_ast_traversal