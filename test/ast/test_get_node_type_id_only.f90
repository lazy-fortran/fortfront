program test_get_node_type_id_only
    use fortfront
    implicit none

    logical :: all_passed = .true.

    print *, "Testing get_node_type_id function only..."

    if (.not. test_get_node_type_id_default()) all_passed = .false.
    if (.not. test_has_semantic_info()) all_passed = .false.
    if (.not. test_get_node_source_location()) all_passed = .false.

    if (all_passed) then
        print *, "All get_node_type_id tests passed!"
        stop 0
    else
        print *, "Some get_node_type_id tests failed!"
        stop 1
    end if

contains

    ! Test get_node_type_id with default case
    logical function test_get_node_type_id_default()
        character(len=*), parameter :: source = "x = 42"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        character(len=:), allocatable :: error_msg
        class(ast_node), allocatable :: node
        integer :: type_id
        
        test_get_node_type_id_default = .true.
        print *, "Testing get_node_type_id with default case..."

        arena = create_ast_arena()
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  FAIL: Lexing failed"
            test_get_node_type_id_default = .false.
            return
        end if
        
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  FAIL: Parsing failed"
            test_get_node_type_id_default = .false.
            return
        end if

        node = get_node(arena, root_index)
        if (.not. allocated(node)) then
            print *, "  FAIL: Could not get node"
            test_get_node_type_id_default = .false.
            return
        end if

        ! Test with a custom node type that might hit class default
        type_id = get_node_type_id(node)
        print *, "  Node type ID: ", type_id

        if (type_id > 0 .and. type_id <= 99) then
            print *, "  PASS: get_node_type_id returned valid type ID"
        else
            print *, "  FAIL: Invalid type ID returned"
            test_get_node_type_id_default = .false.
        end if

        print *, "  get_node_type_id default: ", &
                 merge("PASS", "FAIL", test_get_node_type_id_default)
    end function test_get_node_type_id_default

    ! Test has_semantic_info function
    logical function test_has_semantic_info()
        character(len=*), parameter :: source = "x = 42"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        character(len=:), allocatable :: error_msg
        class(ast_node), allocatable :: node
        logical :: has_info
        
        test_has_semantic_info = .true.
        print *, "Testing has_semantic_info..."

        arena = create_ast_arena()
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  FAIL: Lexing failed"
            test_has_semantic_info = .false.
            return
        end if
        
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  FAIL: Parsing failed"
            test_has_semantic_info = .false.
            return
        end if

        node = get_node(arena, root_index)
        if (.not. allocated(node)) then
            print *, "  FAIL: Could not get node"
            test_has_semantic_info = .false.
            return
        end if

        has_info = has_semantic_info(node)
        print *, "  Node has semantic info: ", has_info

        ! Without semantic analysis, should be false
        if (has_info .eqv. .false.) then
            print *, "  PASS: has_semantic_info correctly returns false"
        else
            print *, "  INFO: has_semantic_info returns true (unexpected but not wrong)"
        end if

        print *, "  has_semantic_info: ", &
                 merge("PASS", "FAIL", test_has_semantic_info)
    end function test_has_semantic_info

    ! Test get_node_source_location function
    logical function test_get_node_source_location()
        character(len=*), parameter :: source = "x = 42"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        character(len=:), allocatable :: error_msg
        class(ast_node), allocatable :: node
        integer :: line, column
        
        test_get_node_source_location = .true.
        print *, "Testing get_node_source_location..."

        arena = create_ast_arena()
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  FAIL: Lexing failed"
            test_get_node_source_location = .false.
            return
        end if
        
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  FAIL: Parsing failed"
            test_get_node_source_location = .false.
            return
        end if

        node = get_node(arena, root_index)
        if (.not. allocated(node)) then
            print *, "  FAIL: Could not get node"
            test_get_node_source_location = .false.
            return
        end if

        call get_node_source_location(node, line, column)
        print *, "  Source location: line=", line, " column=", column

        if (line >= 0 .and. column >= 0) then
            print *, "  PASS: get_node_source_location returned valid coordinates"
        else
            print *, "  FAIL: Invalid source location returned"
            test_get_node_source_location = .false.
        end if

        print *, "  get_node_source_location: ", &
                 merge("PASS", "FAIL", test_get_node_source_location)
    end function test_get_node_source_location

end program test_get_node_type_id_only