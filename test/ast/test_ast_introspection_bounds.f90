program test_ast_introspection_bounds
    use fortfront
    implicit none

    logical :: all_passed = .true.

    print *, "Testing AST Introspection Bounds Checking..."

    if (.not. test_get_node_bounds()) all_passed = .false.
    if (.not. test_get_node_type_kind_bounds()) all_passed = .false.
    if (.not. test_get_node_type_details_bounds()) all_passed = .false.

    if (all_passed) then
        print *, "All AST introspection bounds tests passed!"
        stop 0
    else
        print *, "Some AST introspection bounds tests failed!"
        stop 1
    end if

contains

    ! Test get_node bounds checking
    logical function test_get_node_bounds()
        character(len=*), parameter :: source = "x = 42"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        character(len=:), allocatable :: error_msg
        class(ast_node), allocatable :: node
        
        test_get_node_bounds = .true.
        print *, "Testing get_node bounds checking..."

        ! Create arena with content
        arena = create_ast_arena()
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  FAIL: Lexing failed"
            test_get_node_bounds = .false.
            return
        end if
        
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  FAIL: Parsing failed"
            test_get_node_bounds = .false.
            return
        end if

        ! Test valid index first (should work)
        node = get_node(arena, root_index)
        if (.not. allocated(node)) then
            print *, "  FAIL: Valid index should return allocated node"
            test_get_node_bounds = .false.
        else
            print *, "  PASS: Valid index returns allocated node"
        end if

        ! Test negative index (should return unallocated)
        node = get_node(arena, -1)
        if (allocated(node)) then
            print *, "  FAIL: Negative index should return unallocated"
            test_get_node_bounds = .false.
        else
            print *, "  PASS: Negative index returns unallocated"
        end if

        ! Test zero index (should return unallocated)
        node = get_node(arena, 0)
        if (allocated(node)) then
            print *, "  FAIL: Zero index should return unallocated"
            test_get_node_bounds = .false.
        else
            print *, "  PASS: Zero index returns unallocated"
        end if

        ! Test beyond bounds (should return unallocated)
        node = get_node(arena, arena%size + 1)
        if (allocated(node)) then
            print *, "  FAIL: Index beyond bounds should return unallocated"
            test_get_node_bounds = .false.
        else
            print *, "  PASS: Index beyond bounds returns unallocated"
        end if

        print *, "  get_node bounds: ", merge("PASS", "FAIL", test_get_node_bounds)
    end function test_get_node_bounds

    ! Test get_node_type_kind bounds checking
    logical function test_get_node_type_kind_bounds()
        character(len=*), parameter :: source = "x = 42"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        character(len=:), allocatable :: error_msg
        integer :: type_kind
        
        test_get_node_type_kind_bounds = .true.
        print *, "Testing get_node_type_kind bounds checking..."

        ! Create arena with content
        arena = create_ast_arena()
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  FAIL: Lexing failed"
            test_get_node_type_kind_bounds = .false.
            return
        end if
        
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  FAIL: Parsing failed"
            test_get_node_type_kind_bounds = .false.
            return
        end if

        ! Test valid index (should return 0 since no semantic analysis)
        type_kind = get_node_type_kind(arena, root_index)
        if (type_kind == 0) then
            print *, "  PASS: Valid index returns 0 (no semantic info)"
        else
            print *, "  INFO: Valid index returns ", type_kind
        end if

        ! Test negative index (should return 0)
        type_kind = get_node_type_kind(arena, -1)
        if (type_kind == 0) then
            print *, "  PASS: Negative index returns 0"
        else
            print *, "  FAIL: Negative index should return 0, got ", type_kind
            test_get_node_type_kind_bounds = .false.
        end if

        ! Test zero index (should return 0)
        type_kind = get_node_type_kind(arena, 0)
        if (type_kind == 0) then
            print *, "  PASS: Zero index returns 0"
        else
            print *, "  FAIL: Zero index should return 0, got ", type_kind
            test_get_node_type_kind_bounds = .false.
        end if

        ! Test beyond bounds (should return 0)
        type_kind = get_node_type_kind(arena, arena%size + 1)
        if (type_kind == 0) then
            print *, "  PASS: Index beyond bounds returns 0"
        else
            print *, "  FAIL: Index beyond bounds should return 0, got ", type_kind
            test_get_node_type_kind_bounds = .false.
        end if

        print *, "  get_node_type_kind bounds: ", &
                 merge("PASS", "FAIL", test_get_node_type_kind_bounds)
    end function test_get_node_type_kind_bounds

    ! Test get_node_type_details bounds checking
    logical function test_get_node_type_details_bounds()
        character(len=*), parameter :: source = "x = 42"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        character(len=:), allocatable :: error_msg
        integer :: kind, type_size
        logical :: is_allocatable, is_pointer, found
        
        test_get_node_type_details_bounds = .true.
        print *, "Testing get_node_type_details bounds checking..."

        ! Create arena with content
        arena = create_ast_arena()
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  FAIL: Lexing failed"
            test_get_node_type_details_bounds = .false.
            return
        end if
        
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  FAIL: Parsing failed"
            test_get_node_type_details_bounds = .false.
            return
        end if

        ! Test valid index (should set found=false since no semantic analysis)
        call get_node_type_details(arena, root_index, kind, type_size, &
                                 is_allocatable, is_pointer, found)
        if (.not. found) then
            print *, "  PASS: Valid index sets found=false (no semantic info)"
        else
            print *, "  INFO: Valid index sets found=true (unexpected)"
        end if

        ! Test negative index (should set found=false)
        call get_node_type_details(arena, -1, kind, type_size, &
                                 is_allocatable, is_pointer, found)
        if (.not. found) then
            print *, "  PASS: Negative index sets found=false"
        else
            print *, "  FAIL: Negative index should set found=false"
            test_get_node_type_details_bounds = .false.
        end if

        ! Test zero index (should set found=false)
        call get_node_type_details(arena, 0, kind, type_size, &
                                 is_allocatable, is_pointer, found)
        if (.not. found) then
            print *, "  PASS: Zero index sets found=false"
        else
            print *, "  FAIL: Zero index should set found=false"
            test_get_node_type_details_bounds = .false.
        end if

        ! Test beyond bounds (should set found=false)
        call get_node_type_details(arena, arena%size + 1, kind, type_size, &
                                 is_allocatable, is_pointer, found)
        if (.not. found) then
            print *, "  PASS: Index beyond bounds sets found=false"
        else
            print *, "  FAIL: Index beyond bounds should set found=false"
            test_get_node_type_details_bounds = .false.
        end if

        print *, "  get_node_type_details bounds: ", &
                 merge("PASS", "FAIL", test_get_node_type_details_bounds)
    end function test_get_node_type_details_bounds

end program test_ast_introspection_bounds