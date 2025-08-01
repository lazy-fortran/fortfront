program test_allocate_deallocate_parsing
    use fortfront
    use frontend
    use ast_core
    use lexer_core, only: token_t
    implicit none

    logical :: all_tests_passed

    print *, "=== Allocate/Deallocate Parsing Tests ==="
    print *
    
    all_tests_passed = .true.
    
    ! Run all tests
    if (.not. test_simple_allocate()) all_tests_passed = .false.
    if (.not. test_allocate_with_shape()) all_tests_passed = .false.
    if (.not. test_allocate_with_stat()) all_tests_passed = .false.
    if (.not. test_simple_deallocate()) all_tests_passed = .false.
    if (.not. test_deallocate_with_stat()) all_tests_passed = .false.
    
    print *
    if (all_tests_passed) then
        print *, "All allocate/deallocate parsing tests passed!"
    else
        print *, "Some tests failed"
        stop 1
    end if

contains

    logical function test_simple_allocate()
        character(len=*), parameter :: source = "allocate(arr)"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        character(len=:), allocatable :: error_msg
        
        test_simple_allocate = .true.
        print *, "Testing simple allocate..."
        
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "  FAIL: Lex error: ", error_msg
                test_simple_allocate = .false.
                return
            end if
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "  FAIL: Parse error: ", error_msg
                test_simple_allocate = .false.
                return
            end if
        end if
        
        ! Check if allocate statement node exists
        block
            integer, allocatable :: allocate_nodes(:)
            integer :: i
            
            
            allocate_nodes = find_nodes_by_type(arena, "allocate_statement")
            if (size(allocate_nodes) > 0) then
                print *, "  PASS: Simple allocate parsed"
            else
                print *, "  FAIL: No allocate statement found"
                test_simple_allocate = .false.
            end if
        end block
        
    end function test_simple_allocate

    logical function test_allocate_with_shape()
        character(len=*), parameter :: source = "allocate(matrix(10, 20))"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        character(len=:), allocatable :: error_msg
        
        test_allocate_with_shape = .true.
        print *, "Testing allocate with shape..."
        
        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        
        block
            integer, allocatable :: allocate_nodes(:)
            allocate_nodes = find_nodes_by_type(arena, "allocate_statement")
            if (size(allocate_nodes) > 0) then
                print *, "  PASS: Allocate with shape parsed"
            else
                print *, "  FAIL: No allocate statement found"
                test_allocate_with_shape = .false.
            end if
        end block
        
    end function test_allocate_with_shape

    logical function test_allocate_with_stat()
        character(len=*), parameter :: source = "allocate(arr(100), stat=ierr)"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        character(len=:), allocatable :: error_msg
        
        test_allocate_with_stat = .true.
        print *, "Testing allocate with stat..."
        
        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        
        block
            integer, allocatable :: allocate_nodes(:)
            allocate_nodes = find_nodes_by_type(arena, "allocate_statement")
            if (size(allocate_nodes) > 0) then
                print *, "  PASS: Allocate with stat parsed"
            else
                print *, "  FAIL: No allocate statement found"
                test_allocate_with_stat = .false.
            end if
        end block
        
    end function test_allocate_with_stat

    logical function test_simple_deallocate()
        character(len=*), parameter :: source = "deallocate(arr)"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        character(len=:), allocatable :: error_msg
        
        test_simple_deallocate = .true.
        print *, "Testing simple deallocate..."
        
        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        
        block
            integer, allocatable :: deallocate_nodes(:)
            deallocate_nodes = find_nodes_by_type(arena, "deallocate_statement")
            if (size(deallocate_nodes) > 0) then
                print *, "  PASS: Simple deallocate parsed"
            else
                print *, "  FAIL: No deallocate statement found"
                test_simple_deallocate = .false.
            end if
        end block
        
    end function test_simple_deallocate

    logical function test_deallocate_with_stat()
        character(len=*), parameter :: source = "deallocate(arr, stat=ierr)"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        character(len=:), allocatable :: error_msg
        
        test_deallocate_with_stat = .true.
        print *, "Testing deallocate with stat..."
        
        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        
        block
            integer, allocatable :: deallocate_nodes(:)
            deallocate_nodes = find_nodes_by_type(arena, "deallocate_statement")
            if (size(deallocate_nodes) > 0) then
                print *, "  PASS: Deallocate with stat parsed"
            else
                print *, "  FAIL: No deallocate statement found"
                test_deallocate_with_stat = .false.
            end if
        end block
        
    end function test_deallocate_with_stat

end program test_allocate_deallocate_parsing