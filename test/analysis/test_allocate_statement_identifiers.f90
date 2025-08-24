program test_allocate_statement_identifiers
    use frontend, only: lex_source, parse_tokens
    use lexer_core, only: token_t
    use ast_core
    use variable_usage_tracker_module, only: get_identifiers_in_subtree
    use fortfront, only: find_nodes_by_type
    implicit none
    
    logical :: all_passed
    
    all_passed = .true.
    
    all_passed = all_passed .and. test_basic_allocate_with_stat()
    all_passed = all_passed .and. test_basic_deallocate_with_stat()
    all_passed = all_passed .and. test_allocate_with_shape_expressions()
    all_passed = all_passed .and. test_allocate_with_source_and_mold()
    all_passed = all_passed .and. test_allocate_with_errmsg()
    all_passed = all_passed .and. test_complex_allocate_statement()
    
    if (all_passed) then
        print '(a)', "All allocate statement identifier tests passed"
        stop 0
    else
        print '(a)', "Some allocate statement identifier tests failed"
        stop 1
    end if
    
contains

    logical function test_basic_allocate_with_stat()
        ! Test basic allocate statement with stat parameter
        character(len=*), parameter :: test_code = &
            'program test' // new_line('a') // &
            '  integer, allocatable :: array(:)' // new_line('a') // &
            '  integer :: stat' // new_line('a') // &
            '  allocate(array(100), stat=stat)' // new_line('a') // &
            '  if (stat /= 0) return' // new_line('a') // &
            'end program'
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        character(len=:), allocatable :: error_msg
        integer, allocatable :: allocate_nodes(:)
        character(len=:), allocatable :: identifiers(:)
        integer :: i
        logical :: found_array, found_stat
        
        test_basic_allocate_with_stat = .true.
        
        print '(a)', "Testing basic allocate with stat parameter..."
        
        ! Tokenize and parse
        call lex_source(test_code, tokens, error_msg)
        if (error_msg /= "") then
            print '(a)', "FAIL: Lexing error:", error_msg
            test_basic_allocate_with_stat = .false.
            return
        end if
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        if (root_index <= 0) then
            print '(a)', "FAIL: Parsing failed:", error_msg
            test_basic_allocate_with_stat = .false.
            return
        end if
        
        ! Find allocate statement nodes in the AST
        allocate_nodes = find_nodes_by_type(arena, "allocate_statement")
        
        if (.not. allocated(allocate_nodes) .or. size(allocate_nodes) == 0) then
            print '(a)', "FAIL: No allocate statement nodes found"
            test_basic_allocate_with_stat = .false.
            return
        end if
        
        ! Get identifiers from the first allocate statement
        identifiers = get_identifiers_in_subtree(arena, allocate_nodes(1))
        
        if (.not. allocated(identifiers) .or. size(identifiers) == 0) then
            print '(a)', "FAIL: No identifiers found in allocate statement"
            test_basic_allocate_with_stat = .false.
            return
        end if
        
        print '(a,i0)', "Found ", size(identifiers), " identifiers in allocate statement"
        do i = 1, size(identifiers)
            print '(a,i0,a,a)', "  Identifier ", i, ": ", trim(identifiers(i))
        end do
        
        ! Check if we found both 'array' and 'stat'
        found_array = .false.
        found_stat = .false.
        do i = 1, size(identifiers)
            if (trim(identifiers(i)) == "array") found_array = .true.
            if (trim(identifiers(i)) == "stat") found_stat = .true.
        end do
        
        if (.not. found_array) then
            print '(a)', "FAIL: 'array' identifier not found"
            test_basic_allocate_with_stat = .false.
        else
            print '(a)', "PASS: Found 'array' identifier"
        end if
        
        if (.not. found_stat) then
            print '(a)', "FAIL: 'stat' identifier not found"
            test_basic_allocate_with_stat = .false.
        else
            print '(a)', "PASS: Found 'stat' identifier"
        end if
    end function test_basic_allocate_with_stat

    logical function test_basic_deallocate_with_stat()
        ! Test basic deallocate statement with stat parameter
        character(len=*), parameter :: test_code = &
            'program test' // new_line('a') // &
            '  integer, allocatable :: array(:)' // new_line('a') // &
            '  integer :: status' // new_line('a') // &
            '  deallocate(array, stat=status)' // new_line('a') // &
            '  if (status /= 0) print *, "error"' // new_line('a') // &
            'end program'
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        character(len=:), allocatable :: error_msg
        integer, allocatable :: deallocate_nodes(:)
        character(len=:), allocatable :: identifiers(:)
        integer :: i
        logical :: found_array, found_status
        
        test_basic_deallocate_with_stat = .true.
        
        print '(a)', "Testing basic deallocate with stat parameter..."
        
        ! Tokenize and parse
        call lex_source(test_code, tokens, error_msg)
        if (error_msg /= "") then
            print '(a)', "FAIL: Lexing error:", error_msg
            test_basic_deallocate_with_stat = .false.
            return
        end if
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        if (root_index <= 0) then
            print '(a)', "FAIL: Parsing failed:", error_msg
            test_basic_deallocate_with_stat = .false.
            return
        end if
        
        ! Find deallocate statement nodes in the AST
        deallocate_nodes = find_nodes_by_type(arena, "deallocate_statement")
        
        if (.not. allocated(deallocate_nodes) .or. size(deallocate_nodes) == 0) then
            print '(a)', "FAIL: No deallocate statement nodes found"
            test_basic_deallocate_with_stat = .false.
            return
        end if
        
        ! Get identifiers from the first deallocate statement
        identifiers = get_identifiers_in_subtree(arena, deallocate_nodes(1))
        
        if (.not. allocated(identifiers) .or. size(identifiers) == 0) then
            print '(a)', "FAIL: No identifiers found in deallocate statement"
            test_basic_deallocate_with_stat = .false.
            return
        end if
        
        print '(a,i0)', "Found ", size(identifiers), " identifiers in deallocate statement"
        do i = 1, size(identifiers)
            print '(a,i0,a,a)', "  Identifier ", i, ": ", trim(identifiers(i))
        end do
        
        ! Check if we found both 'array' and 'status'
        found_array = .false.
        found_status = .false.
        do i = 1, size(identifiers)
            if (trim(identifiers(i)) == "array") found_array = .true.
            if (trim(identifiers(i)) == "status") found_status = .true.
        end do
        
        if (.not. found_array) then
            print '(a)', "FAIL: 'array' identifier not found"
            test_basic_deallocate_with_stat = .false.
        else
            print '(a)', "PASS: Found 'array' identifier"
        end if
        
        if (.not. found_status) then
            print '(a)', "FAIL: 'status' identifier not found"
            test_basic_deallocate_with_stat = .false.
        else
            print '(a)', "PASS: Found 'status' identifier"
        end if
    end function test_basic_deallocate_with_stat

    logical function test_allocate_with_shape_expressions()
        ! Test allocate statement with shape expressions containing variables
        character(len=*), parameter :: test_code = &
            'program test' // new_line('a') // &
            '  integer, allocatable :: matrix(:,:)' // new_line('a') // &
            '  integer :: rows, cols, stat' // new_line('a') // &
            '  allocate(matrix(rows*2, cols+1), stat=stat)' // new_line('a') // &
            'end program'
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        character(len=:), allocatable :: error_msg
        integer, allocatable :: allocate_nodes(:)
        character(len=:), allocatable :: identifiers(:)
        integer :: i
        logical :: found_matrix, found_rows, found_cols, found_stat
        
        test_allocate_with_shape_expressions = .true.
        
        print '(a)', "Testing allocate with shape expressions..."
        
        ! Tokenize and parse
        call lex_source(test_code, tokens, error_msg)
        if (error_msg /= "") then
            print '(a)', "FAIL: Lexing error:", error_msg
            test_allocate_with_shape_expressions = .false.
            return
        end if
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        if (root_index <= 0) then
            print '(a)', "FAIL: Parsing failed:", error_msg
            test_allocate_with_shape_expressions = .false.
            return
        end if
        
        ! Find allocate statement nodes in the AST
        allocate_nodes = find_nodes_by_type(arena, "allocate_statement")
        
        if (.not. allocated(allocate_nodes) .or. size(allocate_nodes) == 0) then
            print '(a)', "FAIL: No allocate statement nodes found"
            test_allocate_with_shape_expressions = .false.
            return
        end if
        
        ! Get identifiers from the first allocate statement
        identifiers = get_identifiers_in_subtree(arena, allocate_nodes(1))
        
        if (.not. allocated(identifiers) .or. size(identifiers) == 0) then
            print '(a)', "FAIL: No identifiers found in allocate statement"
            test_allocate_with_shape_expressions = .false.
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
        found_stat = .false.
        do i = 1, size(identifiers)
            if (trim(identifiers(i)) == "matrix") found_matrix = .true.
            if (trim(identifiers(i)) == "rows") found_rows = .true.
            if (trim(identifiers(i)) == "cols") found_cols = .true.
            if (trim(identifiers(i)) == "stat") found_stat = .true.
        end do
        
        if (.not. found_matrix) then
            print '(a)', "FAIL: 'matrix' identifier not found"
            test_allocate_with_shape_expressions = .false.
        else
            print '(a)', "PASS: Found 'matrix' identifier"
        end if
        
        if (.not. found_rows) then
            print '(a)', "FAIL: 'rows' identifier not found"
            test_allocate_with_shape_expressions = .false.
        else
            print '(a)', "PASS: Found 'rows' identifier"
        end if
        
        if (.not. found_cols) then
            print '(a)', "FAIL: 'cols' identifier not found"
            test_allocate_with_shape_expressions = .false.
        else
            print '(a)', "PASS: Found 'cols' identifier"
        end if
        
        if (.not. found_stat) then
            print '(a)', "FAIL: 'stat' identifier not found"
            test_allocate_with_shape_expressions = .false.
        else
            print '(a)', "PASS: Found 'stat' identifier"
        end if
    end function test_allocate_with_shape_expressions

    logical function test_allocate_with_source_and_mold()
        ! Test allocate statement with source and mold expressions
        character(len=*), parameter :: test_code = &
            'program test' // new_line('a') // &
            '  integer, allocatable :: array1(:), array2(:)' // new_line('a') // &
            '  integer :: template(10), stat' // new_line('a') // &
            '  allocate(array1, source=template, stat=stat)' // new_line('a') // &
            '  allocate(array2, mold=array1, stat=stat)' // new_line('a') // &
            'end program'
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        character(len=:), allocatable :: error_msg
        integer, allocatable :: allocate_nodes(:)
        character(len=:), allocatable :: identifiers(:)
        integer :: i
        logical :: found_template, found_array1, found_stat
        
        test_allocate_with_source_and_mold = .true.
        
        print '(a)', "Testing allocate with source and mold..."
        
        ! Tokenize and parse
        call lex_source(test_code, tokens, error_msg)
        if (error_msg /= "") then
            print '(a)', "FAIL: Lexing error:", error_msg
            test_allocate_with_source_and_mold = .false.
            return
        end if
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        if (root_index <= 0) then
            print '(a)', "FAIL: Parsing failed:", error_msg
            test_allocate_with_source_and_mold = .false.
            return
        end if
        
        ! Find allocate statement nodes in the AST
        allocate_nodes = find_nodes_by_type(arena, "allocate_statement")
        
        if (.not. allocated(allocate_nodes) .or. size(allocate_nodes) < 2) then
            print '(a)', "FAIL: Expected 2 allocate statement nodes, found:", size(allocate_nodes)
            test_allocate_with_source_and_mold = .false.
            return
        end if
        
        ! Test first allocate statement (source)
        identifiers = get_identifiers_in_subtree(arena, allocate_nodes(1))
        
        if (.not. allocated(identifiers) .or. size(identifiers) == 0) then
            print '(a)', "FAIL: No identifiers found in first allocate statement"
            test_allocate_with_source_and_mold = .false.
            return
        end if
        
        print '(a,i0)', "Found ", size(identifiers), " identifiers in first allocate statement"
        do i = 1, size(identifiers)
            print '(a,i0,a,a)', "  Identifier ", i, ": ", trim(identifiers(i))
        end do
        
        ! Check first allocate statement
        found_array1 = .false.
        found_template = .false.
        found_stat = .false.
        do i = 1, size(identifiers)
            if (trim(identifiers(i)) == "array1") found_array1 = .true.
            if (trim(identifiers(i)) == "template") found_template = .true.
            if (trim(identifiers(i)) == "stat") found_stat = .true.
        end do
        
        if (.not. found_array1) then
            print '(a)', "FAIL: 'array1' identifier not found in first allocate"
            test_allocate_with_source_and_mold = .false.
        else
            print '(a)', "PASS: Found 'array1' identifier in first allocate"
        end if
        
        if (.not. found_template) then
            print '(a)', "FAIL: 'template' identifier not found in first allocate"
            test_allocate_with_source_and_mold = .false.
        else
            print '(a)', "PASS: Found 'template' identifier in first allocate"
        end if
        
        if (.not. found_stat) then  
            print '(a)', "FAIL: 'stat' identifier not found in first allocate"
            test_allocate_with_source_and_mold = .false.
        else
            print '(a)', "PASS: Found 'stat' identifier in first allocate"
        end if
    end function test_allocate_with_source_and_mold

    logical function test_allocate_with_errmsg()
        ! Test allocate statement with errmsg parameter
        character(len=*), parameter :: test_code = &
            'program test' // new_line('a') // &
            '  integer, allocatable :: array(:)' // new_line('a') // &
            '  integer :: stat' // new_line('a') // &
            '  character(len=100) :: error_message' // new_line('a') // &
            '  allocate(array(1000), stat=stat, errmsg=error_message)' // new_line('a') // &
            'end program'
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        character(len=:), allocatable :: error_msg
        integer, allocatable :: allocate_nodes(:)
        character(len=:), allocatable :: identifiers(:)
        integer :: i
        logical :: found_array, found_stat, found_error_message
        
        test_allocate_with_errmsg = .true.
        
        print '(a)', "Testing allocate with errmsg parameter..."
        
        ! Tokenize and parse
        call lex_source(test_code, tokens, error_msg)
        if (error_msg /= "") then
            print '(a)', "FAIL: Lexing error:", error_msg
            test_allocate_with_errmsg = .false.
            return
        end if
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        if (root_index <= 0) then
            print '(a)', "FAIL: Parsing failed:", error_msg
            test_allocate_with_errmsg = .false.
            return
        end if
        
        ! Find allocate statement nodes in the AST
        allocate_nodes = find_nodes_by_type(arena, "allocate_statement")
        
        if (.not. allocated(allocate_nodes) .or. size(allocate_nodes) == 0) then
            print '(a)', "FAIL: No allocate statement nodes found"
            test_allocate_with_errmsg = .false.
            return
        end if
        
        ! Get identifiers from the first allocate statement
        identifiers = get_identifiers_in_subtree(arena, allocate_nodes(1))
        
        if (.not. allocated(identifiers) .or. size(identifiers) == 0) then
            print '(a)', "FAIL: No identifiers found in allocate statement"
            test_allocate_with_errmsg = .false.
            return
        end if
        
        print '(a,i0)', "Found ", size(identifiers), " identifiers in allocate statement"
        do i = 1, size(identifiers)
            print '(a,i0,a,a)', "  Identifier ", i, ": ", trim(identifiers(i))
        end do
        
        ! Check if we found all expected identifiers
        found_array = .false.
        found_stat = .false.
        found_error_message = .false.
        do i = 1, size(identifiers)
            if (trim(identifiers(i)) == "array") found_array = .true.
            if (trim(identifiers(i)) == "stat") found_stat = .true.
            if (trim(identifiers(i)) == "error_message") found_error_message = .true.
        end do
        
        if (.not. found_array) then
            print '(a)', "FAIL: 'array' identifier not found"
            test_allocate_with_errmsg = .false.
        else
            print '(a)', "PASS: Found 'array' identifier"
        end if
        
        if (.not. found_stat) then
            print '(a)', "FAIL: 'stat' identifier not found"
            test_allocate_with_errmsg = .false.
        else
            print '(a)', "PASS: Found 'stat' identifier"
        end if
        
        if (.not. found_error_message) then
            print '(a)', "FAIL: 'error_message' identifier not found"
            test_allocate_with_errmsg = .false.
        else
            print '(a)', "PASS: Found 'error_message' identifier"
        end if
    end function test_allocate_with_errmsg

    logical function test_complex_allocate_statement()
        ! Test complex allocate statement with multiple variables and expressions
        character(len=*), parameter :: test_code = &
            'program test' // new_line('a') // &
            '  integer, allocatable :: arr1(:), arr2(:,:)' // new_line('a') // &
            '  integer :: n, m, status' // new_line('a') // &
            '  character(len=200) :: msg' // new_line('a') // &
            '  allocate(arr1(n*2), arr2(m+1, n-1), stat=status, errmsg=msg)' // new_line('a') // &
            'end program'
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        character(len=:), allocatable :: error_msg
        integer, allocatable :: allocate_nodes(:)
        character(len=:), allocatable :: identifiers(:)
        integer :: i, expected_count
        logical :: found_arr1, found_arr2, found_n, found_m, found_status, found_msg
        
        test_complex_allocate_statement = .true.
        
        print '(a)', "Testing complex allocate statement..."
        
        ! Tokenize and parse
        call lex_source(test_code, tokens, error_msg)
        if (error_msg /= "") then
            print '(a)', "FAIL: Lexing error:", error_msg
            test_complex_allocate_statement = .false.
            return
        end if
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        if (root_index <= 0) then
            print '(a)', "FAIL: Parsing failed:", error_msg
            test_complex_allocate_statement = .false.
            return
        end if
        
        ! Find allocate statement nodes in the AST
        allocate_nodes = find_nodes_by_type(arena, "allocate_statement")
        
        if (.not. allocated(allocate_nodes) .or. size(allocate_nodes) == 0) then
            print '(a)', "FAIL: No allocate statement nodes found"
            test_complex_allocate_statement = .false.
            return
        end if
        
        ! Get identifiers from the first allocate statement
        identifiers = get_identifiers_in_subtree(arena, allocate_nodes(1))
        
        if (.not. allocated(identifiers) .or. size(identifiers) == 0) then
            print '(a)', "FAIL: No identifiers found in allocate statement"
            test_complex_allocate_statement = .false.
            return
        end if
        
        print '(a,i0)', "Found ", size(identifiers), " identifiers in complex allocate statement"
        do i = 1, size(identifiers)
            print '(a,i0,a,a)', "  Identifier ", i, ": ", trim(identifiers(i))
        end do
        
        ! Check if we found all expected identifiers
        found_arr1 = .false.
        found_arr2 = .false.
        found_n = .false.
        found_m = .false.
        found_status = .false.
        found_msg = .false.
        do i = 1, size(identifiers)
            if (trim(identifiers(i)) == "arr1") found_arr1 = .true.
            if (trim(identifiers(i)) == "arr2") found_arr2 = .true.
            if (trim(identifiers(i)) == "n") found_n = .true.
            if (trim(identifiers(i)) == "m") found_m = .true.
            if (trim(identifiers(i)) == "status") found_status = .true.
            if (trim(identifiers(i)) == "msg") found_msg = .true.
        end do
        
        ! Note: 'n' appears multiple times so we might have duplicates
        expected_count = 6  ! arr1, arr2, n (possibly twice), m, status, msg
        
        if (.not. found_arr1) then
            print '(a)', "FAIL: 'arr1' identifier not found"
            test_complex_allocate_statement = .false.
        else
            print '(a)', "PASS: Found 'arr1' identifier"
        end if
        
        if (.not. found_arr2) then
            print '(a)', "FAIL: 'arr2' identifier not found"
            test_complex_allocate_statement = .false.
        else
            print '(a)', "PASS: Found 'arr2' identifier"
        end if
        
        if (.not. found_n) then
            print '(a)', "FAIL: 'n' identifier not found"
            test_complex_allocate_statement = .false.
        else
            print '(a)', "PASS: Found 'n' identifier"
        end if
        
        if (.not. found_m) then
            print '(a)', "FAIL: 'm' identifier not found"
            test_complex_allocate_statement = .false.
        else
            print '(a)', "PASS: Found 'm' identifier"
        end if
        
        if (.not. found_status) then
            print '(a)', "FAIL: 'status' identifier not found"
            test_complex_allocate_statement = .false.
        else
            print '(a)', "PASS: Found 'status' identifier"
        end if
        
        if (.not. found_msg) then
            print '(a)', "FAIL: 'msg' identifier not found"
            test_complex_allocate_statement = .false.
        else
            print '(a)', "PASS: Found 'msg' identifier"
        end if
        
        ! Should have at least the 6 unique identifiers
        if (size(identifiers) < expected_count) then
            print '(a,i0,a,i0)', "FAIL: Expected at least ", expected_count, " identifiers, got ", size(identifiers)
            test_complex_allocate_statement = .false.
        else
            print '(a,i0)', "PASS: Found expected number of identifiers: ", size(identifiers)
        end if
    end function test_complex_allocate_statement

end program test_allocate_statement_identifiers