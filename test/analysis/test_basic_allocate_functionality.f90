program test_basic_allocate_functionality
    use frontend, only: lex_source, parse_tokens
    use lexer_core, only: token_t
    use ast_core
    use ast_arena
    use variable_usage_tracker_module, only: get_identifiers_in_subtree
    use fortfront, only: find_nodes_by_type
    implicit none
    
    logical :: all_passed
    
    all_passed = .true.
    
    all_passed = all_passed .and. test_simple_allocate()
    all_passed = all_passed .and. test_simple_deallocate()
    all_passed = all_passed .and. test_allocate_with_expressions()
    
    if (all_passed) then
        print '(a)', "All basic allocate functionality tests passed"
        stop 0
    else
        print '(a)', "Some basic allocate functionality tests failed"
        stop 1
    end if
    
contains

    logical function test_simple_allocate()
        ! Test simple allocate statement that should work with current parser
        character(len=*), parameter :: test_code = &
            'program test' // new_line('a') // &
            '  integer, allocatable :: array(:)' // new_line('a') // &
            '  allocate(array(100))' // new_line('a') // &
            'end program'
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        character(len=:), allocatable :: error_msg
        integer, allocatable :: allocate_nodes(:)
        character(len=:), allocatable :: identifiers(:)
        integer :: i
        logical :: found_array
        
        test_simple_allocate = .true.
        
        print '(a)', "Testing simple allocate statement..."
        
        ! Tokenize and parse
        call lex_source(test_code, tokens, error_msg)
        if (error_msg /= "") then
            print '(a)', "FAIL: Lexing error:", error_msg
            test_simple_allocate = .false.
            return
        end if
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        if (root_index <= 0) then
            print '(a)', "FAIL: Parsing failed:", error_msg
            test_simple_allocate = .false.
            return
        end if
        
        ! Find allocate statement nodes in the AST
        allocate_nodes = find_nodes_by_type(arena, "allocate_statement")
        
        if (.not. allocated(allocate_nodes) .or. size(allocate_nodes) == 0) then
            print '(a)', "FAIL: No allocate statement nodes found"
            test_simple_allocate = .false.
            return
        end if
        
        ! Get identifiers from the first allocate statement
        identifiers = get_identifiers_in_subtree(arena, allocate_nodes(1))
        
        if (.not. allocated(identifiers) .or. size(identifiers) == 0) then
            print '(a)', "FAIL: No identifiers found in allocate statement"
            test_simple_allocate = .false.
            return
        end if
        
        print '(a,i0)', "Found ", size(identifiers), " identifiers in allocate statement"
        do i = 1, size(identifiers)
            print '(a,i0,a,a)', "  Identifier ", i, ": ", trim(identifiers(i))
        end do
        
        ! Check if we found 'array'
        found_array = .false.
        do i = 1, size(identifiers)
            if (trim(identifiers(i)) == "array") found_array = .true.
        end do
        
        if (.not. found_array) then
            print '(a)', "FAIL: 'array' identifier not found"
            test_simple_allocate = .false.
        else
            print '(a)', "PASS: Found 'array' identifier"
        end if
    end function test_simple_allocate

    logical function test_simple_deallocate()
        ! Test simple deallocate statement
        character(len=*), parameter :: test_code = &
            'program test' // new_line('a') // &
            '  integer, allocatable :: array(:)' // new_line('a') // &
            '  deallocate(array)' // new_line('a') // &
            'end program'
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        character(len=:), allocatable :: error_msg
        integer, allocatable :: deallocate_nodes(:)
        character(len=:), allocatable :: identifiers(:)
        integer :: i
        logical :: found_array
        
        test_simple_deallocate = .true.
        
        print '(a)', "Testing simple deallocate statement..."
        
        ! Tokenize and parse
        call lex_source(test_code, tokens, error_msg)
        if (error_msg /= "") then
            print '(a)', "FAIL: Lexing error:", error_msg
            test_simple_deallocate = .false.
            return
        end if
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        if (root_index <= 0) then
            print '(a)', "FAIL: Parsing failed:", error_msg
            test_simple_deallocate = .false.
            return
        end if
        
        ! Find deallocate statement nodes in the AST
        deallocate_nodes = find_nodes_by_type(arena, "deallocate_statement")
        
        if (.not. allocated(deallocate_nodes) .or. size(deallocate_nodes) == 0) then
            print '(a)', "FAIL: No deallocate statement nodes found"
            test_simple_deallocate = .false.
            return
        end if
        
        ! Get identifiers from the first deallocate statement
        identifiers = get_identifiers_in_subtree(arena, deallocate_nodes(1))
        
        if (.not. allocated(identifiers) .or. size(identifiers) == 0) then
            print '(a)', "FAIL: No identifiers found in deallocate statement"
            test_simple_deallocate = .false.
            return
        end if
        
        print '(a,i0)', "Found ", size(identifiers), " identifiers in deallocate statement"
        do i = 1, size(identifiers)
            print '(a,i0,a,a)', "  Identifier ", i, ": ", trim(identifiers(i))
        end do
        
        ! Check if we found 'array'
        found_array = .false.
        do i = 1, size(identifiers)
            if (trim(identifiers(i)) == "array") found_array = .true.
        end do
        
        if (.not. found_array) then
            print '(a)', "FAIL: 'array' identifier not found"
            test_simple_deallocate = .false.
        else
            print '(a)', "PASS: Found 'array' identifier"
        end if
    end function test_simple_deallocate

    logical function test_allocate_with_expressions()
        ! Test allocate statement with shape expressions containing variables
        character(len=*), parameter :: test_code = &
            'program test' // new_line('a') // &
            '  integer, allocatable :: matrix(:,:)' // new_line('a') // &
            '  integer :: rows, cols' // new_line('a') // &
            '  allocate(matrix(rows*2, cols+1))' // new_line('a') // &
            'end program'
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        character(len=:), allocatable :: error_msg
        integer, allocatable :: allocate_nodes(:)
        character(len=:), allocatable :: identifiers(:)
        integer :: i
        logical :: found_matrix, found_rows, found_cols
        
        test_allocate_with_expressions = .true.
        
        print '(a)', "Testing allocate with shape expressions..."
        
        ! Tokenize and parse
        call lex_source(test_code, tokens, error_msg)
        if (error_msg /= "") then
            print '(a)', "FAIL: Lexing error:", error_msg
            test_allocate_with_expressions = .false.
            return
        end if
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        if (root_index <= 0) then
            print '(a)', "FAIL: Parsing failed:", error_msg
            test_allocate_with_expressions = .false.
            return
        end if
        
        ! Find allocate statement nodes in the AST
        allocate_nodes = find_nodes_by_type(arena, "allocate_statement")
        
        if (.not. allocated(allocate_nodes) .or. size(allocate_nodes) == 0) then
            print '(a)', "FAIL: No allocate statement nodes found"
            test_allocate_with_expressions = .false.
            return
        end if
        
        ! Get identifiers from the first allocate statement
        identifiers = get_identifiers_in_subtree(arena, allocate_nodes(1))
        
        if (.not. allocated(identifiers) .or. size(identifiers) == 0) then
            print '(a)', "FAIL: No identifiers found in allocate statement"
            test_allocate_with_expressions = .false.
            return
        end if
        
        print '(a,i0)', "Found ", size(identifiers), " identifiers in allocate statement"
        do i = 1, size(identifiers)
            print '(a,i0,a,a)', "  Identifier ", i, ": ", trim(identifiers(i))
        end do
        
        ! Check if we found all expected identifiers
        found_matrix = .false.
        found_rows = .false.
        found_cols = .false.
        do i = 1, size(identifiers)
            if (trim(identifiers(i)) == "matrix") found_matrix = .true.
            if (trim(identifiers(i)) == "rows") found_rows = .true.
            if (trim(identifiers(i)) == "cols") found_cols = .true.
        end do
        
        if (.not. found_matrix) then
            print '(a)', "FAIL: 'matrix' identifier not found"
            test_allocate_with_expressions = .false.
        else
            print '(a)', "PASS: Found 'matrix' identifier"
        end if
        
        if (.not. found_rows) then
            print '(a)', "FAIL: 'rows' identifier not found"
            test_allocate_with_expressions = .false.
        else
            print '(a)', "PASS: Found 'rows' identifier"
        end if
        
        if (.not. found_cols) then
            print '(a)', "FAIL: 'cols' identifier not found"
            test_allocate_with_expressions = .false.
        else
            print '(a)', "PASS: Found 'cols' identifier"
        end if
    end function test_allocate_with_expressions

end program test_basic_allocate_functionality