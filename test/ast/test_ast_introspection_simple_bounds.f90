program test_ast_introspection_simple_bounds
    use fortfront
    implicit none

    logical :: all_passed = .true.

    print *, "Testing AST Introspection Simple Bounds..."

    if (.not. test_simple_bounds()) all_passed = .false.

    if (all_passed) then
        print *, "All AST introspection simple bounds tests passed!"
        stop 0
    else
        print *, "Some AST introspection simple bounds tests failed!"
        stop 1
    end if

contains

    ! Test simple bounds checking without complex arena operations
    logical function test_simple_bounds()
        character(len=*), parameter :: source = "x = 42"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        character(len=:), allocatable :: error_msg
        class(ast_node), allocatable :: node
        
        test_simple_bounds = .true.
        print *, "Testing simple bounds checking..."

        ! Create arena with content
        arena = create_ast_arena()
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  FAIL: Lexing failed"
            test_simple_bounds = .false.
            return
        end if
        
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  FAIL: Parsing failed"
            test_simple_bounds = .false.
            return
        end if

        print *, "  Arena created successfully with size: ", arena%size

        ! Test valid index first
        node = get_node(arena, root_index)
        if (.not. allocated(node)) then
            print *, "  FAIL: Valid index should return allocated node"
            test_simple_bounds = .false.
        else
            print *, "  PASS: Valid index returns allocated node"
        end if

        ! Test negative index - this should hit line 36 bounds check
        node = get_node(arena, -1)
        if (allocated(node)) then
            print *, "  FAIL: Negative index should return unallocated"
            test_simple_bounds = .false.
        else
            print *, "  PASS: Negative index returns unallocated"
        end if

        ! Test zero index - this should hit line 36 bounds check
        node = get_node(arena, 0)
        if (allocated(node)) then
            print *, "  FAIL: Zero index should return unallocated"
            test_simple_bounds = .false.
        else
            print *, "  PASS: Zero index returns unallocated"
        end if

        print *, "  Simple bounds: ", merge("PASS", "FAIL", test_simple_bounds)
    end function test_simple_bounds

end program test_ast_introspection_simple_bounds