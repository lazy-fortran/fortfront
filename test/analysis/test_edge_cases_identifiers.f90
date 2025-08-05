program test_edge_cases_identifiers
    use fortfront, only: lex_source, parse_tokens, create_ast_arena, token_t, ast_arena_t
    use variable_usage_tracker_module, only: get_identifiers_in_subtree
    implicit none
    
    logical :: all_passed
    
    all_passed = .true.
    
    ! Test various edge cases
    if (.not. test_empty_program()) all_passed = .false.
    if (.not. test_unhandled_node_types()) all_passed = .false.
    if (.not. test_deeply_nested_expressions()) all_passed = .false.
    
    if (all_passed) then
        print *, "All edge case tests passed"
        stop 0
    else
        print *, "Some edge case tests failed"
        stop 1
    end if
    
contains

    logical function test_empty_program()
        character(len=:), allocatable :: source_code
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: error_msg
        type(ast_arena_t) :: arena
        character(len=:), allocatable :: identifiers(:)
        integer :: prog_index
        
        test_empty_program = .true.
        
        print *, "Testing empty program..."
        
        source_code = 'program empty' // new_line('A') // 'end program'
        
        call lex_source(source_code, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        
        identifiers = get_identifiers_in_subtree(arena, prog_index)
        
        if (size(identifiers) == 0) then
            print *, "PASS: Empty program returns no identifiers"
        else
            print *, "FAIL: Empty program should return no identifiers"
            test_empty_program = .false.
        end if
        
    end function test_empty_program

    logical function test_unhandled_node_types()
        character(len=:), allocatable :: source_code
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: error_msg
        type(ast_arena_t) :: arena
        character(len=:), allocatable :: identifiers(:)
        integer :: prog_index, i
        
        test_unhandled_node_types = .true.
        
        print *, "Testing various node types..."
        
        ! Test with various statements
        source_code = &
            'program test' // new_line('A') // &
            '  integer :: a, b, c' // new_line('A') // &
            '  real :: x(10)' // new_line('A') // &
            '  a = 1' // new_line('A') // &
            '  b = a + 2' // new_line('A') // &
            '  x(a) = b * c' // new_line('A') // &
            '  call some_sub(a, b)' // new_line('A') // &
            '  write(*,*) a, b, c' // new_line('A') // &
            'end program'
        
        call lex_source(source_code, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        
        ! Test each node
        print *, "Testing identifiers in each node type:"
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                identifiers = get_identifiers_in_subtree(arena, i)
                print '(a,a,a,i0,a)', "  ", arena%entries(i)%node_type, &
                    " node: ", size(identifiers), " identifiers"
                
                ! Check specific nodes that should have identifiers
                select case (arena%entries(i)%node_type)
                case ("identifier")
                    if (size(identifiers) < 1) then
                        print *, "    FAIL: identifier node should return itself"
                        test_unhandled_node_types = .false.
                    end if
                case ("call_statement")
                    if (size(identifiers) < 1) then
                        print *, "    FAIL: call statement should have identifiers"
                        test_unhandled_node_types = .false.
                    end if
                case ("write_statement")
                    if (size(identifiers) < 1) then
                        print *, "    FAIL: write statement should have identifiers"
                        test_unhandled_node_types = .false.
                    end if
                end select
            end if
        end do
        
    end function test_unhandled_node_types

    logical function test_deeply_nested_expressions()
        character(len=:), allocatable :: source_code
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: error_msg
        type(ast_arena_t) :: arena
        character(len=:), allocatable :: identifiers(:)
        integer :: prog_index, i
        
        test_deeply_nested_expressions = .true.
        
        print *, "Testing deeply nested expressions..."
        
        source_code = &
            'program test' // new_line('A') // &
            '  integer :: a, b, c, d' // new_line('A') // &
            '  d = ((a + b) * (c - a)) / (b + c)' // new_line('A') // &
            'end program'
        
        call lex_source(source_code, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        
        ! Find the assignment
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                if (arena%entries(i)%node_type == "assignment") then
                    block
                        use ast_nodes_core, only: assignment_node
                        select type (node => arena%entries(i)%node)
                        type is (assignment_node)
                            identifiers = get_identifiers_in_subtree(arena, node%value_index)
                            print '(a,i0,a)', "Found ", size(identifiers), &
                                " identifiers in nested expression"
                            if (size(identifiers) < 3) then
                                print *, "FAIL: Expected at least 3 identifiers (a, b, c)"
                                test_deeply_nested_expressions = .false.
                            else
                                print *, "PASS: Found all identifiers in nested expression"
                            end if
                        end select
                    end block
                end if
            end if
        end do
        
    end function test_deeply_nested_expressions

end program test_edge_cases_identifiers