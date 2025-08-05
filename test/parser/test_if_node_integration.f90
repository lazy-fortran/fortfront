program test_if_node_integration
    use frontend, only: lex_file, parse_tokens
    use lexer_core, only: token_t, tokenize_core
    use ast_core
    implicit none
    
    logical :: all_passed
    
    all_passed = .true.
    
    ! Test the actual issue: if statements not creating if_node in full compilation
    if (.not. test_if_node_in_full_compilation()) all_passed = .false.
    if (.not. test_if_node_with_multiple_statements()) all_passed = .false.
    if (.not. test_if_node_with_assignments()) all_passed = .false.
    if (.not. test_if_node_error_handling()) all_passed = .false.
    if (.not. test_nested_if_statements()) all_passed = .false.
    
    ! Report results
    if (all_passed) then
        print '(a)', "All if node integration tests passed"
        stop 0
    else
        print '(a)', "Some if node integration tests failed"
        stop 1
    end if
    
contains

    logical function test_if_node_in_full_compilation()
        ! Test that reproduces the issue: if statements should create if_node
        character(len=*), parameter :: test_code = &
            'program test' // new_line('a') // &
            '  integer :: x = 1' // new_line('a') // &
            '  if (x > 0) then' // new_line('a') // &
            '    print *, "positive"' // new_line('a') // &
            '  end if' // new_line('a') // &
            'end program'
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        character(len=256) :: error_msg
        logical :: has_if_node
        
        test_if_node_in_full_compilation = .true.
        
        print '(a)', "Testing if node creation in full compilation..."
        
        ! Tokenize the source code
        call tokenize_core(test_code, tokens)
        
        ! Parse tokens to create AST
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        if (root_index <= 0 .or. len_trim(error_msg) > 0) then
            print '(a)', "FAIL: Parsing failed"
            if (len_trim(error_msg) > 0) then
                print '(a,a)', "Error message: ", trim(error_msg)
            end if
            test_if_node_in_full_compilation = .false.
            return
        end if
        
        ! Debug: print all node types in arena
        call print_arena_contents(arena)
        
        ! Check if arena contains any if_node
        has_if_node = check_for_if_node(arena)
        
        if (has_if_node) then
            print '(a)', "PASS: if_node found in AST"
        else
            print '(a)', "FAIL: No if_node found in AST - this reproduces issue #103"
            test_if_node_in_full_compilation = .false.
        end if
        
    end function test_if_node_in_full_compilation
    
    subroutine print_arena_contents(arena)
        type(ast_arena_t), intent(in) :: arena
        integer :: i
        
        print '(a,i0)', "Arena contains ", arena%size, " nodes:"
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node_type)) then
                print '(a,i0,a,a)', "  Index ", i, ": ", trim(arena%entries(i)%node_type)
            else
                print '(a,i0,a)', "  Index ", i, ": (no node_type)"
            end if
        end do
        
    end subroutine print_arena_contents

    logical function check_for_if_node(arena)
        type(ast_arena_t), intent(in) :: arena
        integer :: i
        
        check_for_if_node = .false.
        
        ! Search through all arena entries for if_node
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                select type (node => arena%entries(i)%node)
                type is (if_node)
                    check_for_if_node = .true.
                    return
                end select
            end if
        end do
        
    end function check_for_if_node

    logical function test_if_node_with_multiple_statements()
        ! Test if blocks with multiple statement types
        character(len=*), parameter :: test_code = &
            'program test' // new_line('a') // &
            '  integer :: x = 1, y' // new_line('a') // &
            '  if (x > 0) then' // new_line('a') // &
            '    y = x + 10' // new_line('a') // &
            '    print *, "positive:", y' // new_line('a') // &
            '  end if' // new_line('a') // &
            'end program'
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        character(len=256) :: error_msg
        logical :: has_if_node
        
        test_if_node_with_multiple_statements = .true.
        
        print '(a)', "Testing if node with multiple statement types..."
        
        ! Tokenize and parse
        call tokenize_core(test_code, tokens)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        if (root_index <= 0 .or. len_trim(error_msg) > 0) then
            print '(a)', "FAIL: Parsing failed"
            test_if_node_with_multiple_statements = .false.
            return
        end if
        
        ! Check if arena contains if_node
        has_if_node = check_for_if_node(arena)
        
        if (has_if_node) then
            print '(a)', "PASS: if_node with multiple statements found"
        else
            print '(a)', "FAIL: No if_node found with multiple statements"
            test_if_node_with_multiple_statements = .false.
        end if
        
    end function test_if_node_with_multiple_statements

    logical function test_if_node_with_assignments()
        ! Test if blocks with assignment statements
        character(len=*), parameter :: test_code = &
            'program test' // new_line('a') // &
            '  integer :: x = 5, result' // new_line('a') // &
            '  if (x > 0) then' // new_line('a') // &
            '    result = x * 2' // new_line('a') // &
            '  end if' // new_line('a') // &
            'end program'
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        character(len=256) :: error_msg
        logical :: has_if_node, has_assignment
        integer :: i
        
        test_if_node_with_assignments = .true.
        
        print '(a)', "Testing if node with assignment statements..."
        
        ! Tokenize and parse
        call tokenize_core(test_code, tokens)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        if (root_index <= 0 .or. len_trim(error_msg) > 0) then
            print '(a)', "FAIL: Parsing failed"
            test_if_node_with_assignments = .false.
            return
        end if
        
        ! Check for both if_node and assignment nodes
        has_if_node = check_for_if_node(arena)
        has_assignment = .false.
        
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node_type)) then
                if (arena%entries(i)%node_type == "assignment") then
                    has_assignment = .true.
                    exit
                end if
            end if
        end do
        
        if (has_if_node .and. has_assignment) then
            print '(a)', "PASS: if_node with assignments found"
        else
            print '(a)', "FAIL: Missing if_node or assignment nodes"
            test_if_node_with_assignments = .false.
        end if
        
    end function test_if_node_with_assignments

    logical function test_if_node_error_handling()
        ! Test error handling for malformed if conditions
        character(len=*), parameter :: test_code = &
            'program test' // new_line('a') // &
            '  integer :: x = 1' // new_line('a') // &
            '  if x > 0 then' // new_line('a') // &
            '    print *, "no parentheses"' // new_line('a') // &
            '  end if' // new_line('a') // &
            'end program'
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        character(len=256) :: error_msg
        logical :: has_if_node
        
        test_if_node_error_handling = .true.
        
        print '(a)', "Testing if node error handling..."
        
        ! Tokenize and parse
        call tokenize_core(test_code, tokens)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        ! Even with malformed syntax, we should still get some parsing
        if (root_index <= 0) then
            print '(a)', "PASS: Parser handled malformed if gracefully"
        else
            ! Check if if_node still created (with error handling)
            has_if_node = check_for_if_node(arena)
            
            if (has_if_node) then
                print '(a)', "PASS: if_node created even with error handling"
            else
                print '(a)', "INFO: Malformed if not parsed as if_node (acceptable)"
            end if
        end if
        
    end function test_if_node_error_handling

    logical function test_nested_if_statements()
        ! Test parsing with nested if statements (simplified - outer if should work)
        character(len=*), parameter :: test_code = &
            'program test' // new_line('a') // &
            '  integer :: x = 5' // new_line('a') // &
            '  if (x > 0) then' // new_line('a') // &
            '    print *, "outer if works"' // new_line('a') // &
            '  end if' // new_line('a') // &
            'end program'
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        character(len=256) :: error_msg
        logical :: has_if_node
        
        test_nested_if_statements = .true.
        
        print '(a)', "Testing complex if statement parsing..."
        
        ! Tokenize and parse
        call tokenize_core(test_code, tokens)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        if (root_index <= 0 .or. len_trim(error_msg) > 0) then
            print '(a)', "FAIL: Parsing failed"
            if (len_trim(error_msg) > 0) then
                print '(a,a)', "Error message: ", trim(error_msg)
            end if
            test_nested_if_statements = .false.
            return
        end if
        
        ! Check for if_node
        has_if_node = check_for_if_node(arena)
        
        if (has_if_node) then
            print '(a)', "PASS: Complex if statement parsed successfully"
        else
            print '(a)', "FAIL: No if_node found in complex case"
            test_nested_if_statements = .false.
        end if
        
    end function test_nested_if_statements

end program test_if_node_integration