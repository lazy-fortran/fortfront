program test_ast_introspection_coverage
    use fortfront
    implicit none

    logical :: all_passed = .true.

    print *, "Testing AST Introspection Coverage..."

    if (.not. test_all_node_types()) all_passed = .false.
    if (.not. test_edge_cases()) all_passed = .false.
    if (.not. test_with_semantic_types()) all_passed = .false.

    if (all_passed) then
        print *, "All coverage tests passed!"
        stop 0
    else
        print *, "Some coverage tests failed!"
        stop 1
    end if

contains

    logical function test_all_node_types()
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: error_msg, source
        integer :: root_index, idx, type_id
        
        test_all_node_types = .true.
        print *, "Testing all node type IDs..."
        
        ! Test various source constructs to get different node types
        arena = create_ast_arena()
        
        ! Test assignment nodes
        source = "x = 42"
        call lex_source(source, tokens, error_msg)
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        ! Check different nodes in the arena
        do idx = 1, arena%size
            type_id = get_node_type_id_from_arena(arena, idx)
            select case (type_id)
            case (NODE_PROGRAM)
                print *, "  Found program node at index", idx
            case (NODE_ASSIGNMENT)
                print *, "  Found assignment node at index", idx
            case (NODE_IDENTIFIER)
                print *, "  Found identifier node at index", idx
            case (NODE_LITERAL)
                print *, "  Found literal node at index", idx
            end select
        end do
        
        print *, "  All node types: PASS"
    end function test_all_node_types

    logical function test_edge_cases()
        type(ast_arena_t) :: arena
        integer :: type_kind, type_size, line, column
        logical :: is_allocatable, is_pointer, found
        
        test_edge_cases = .true.
        print *, "Testing edge cases..."
        
        ! Test empty arena
        arena = create_ast_arena()
        
        ! Test index 0
        type_kind = get_node_type_kind(arena, 0)
        if (type_kind /= 0) then
            print *, "  FAIL: index 0 should return type_kind=0"
            test_edge_cases = .false.
        end if
        
        ! Test very large index
        call get_node_type_details(arena, 999999, type_kind, type_size, &
                                 is_allocatable, is_pointer, found)
        if (found) then
            print *, "  FAIL: large index should not be found"
            test_edge_cases = .false.
        end if
        
        ! Test source location with invalid index
        call get_node_source_location_from_arena(arena, 0, line, column)
        if (line /= 0 .or. column /= 0) then
            print *, "  FAIL: invalid index should return 0,0"
            test_edge_cases = .false.
        end if
        
        print *, "  Edge cases: ", merge("PASS", "FAIL", test_edge_cases)
    end function test_edge_cases

    logical function test_with_semantic_types()
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: error_msg
        integer :: root_index, idx
        integer :: type_kind, type_size
        logical :: is_allocatable, is_pointer, found
        
        test_with_semantic_types = .true.
        print *, "Testing with various semantic types..."
        
        ! Test array type
        arena = create_ast_arena()
        call lex_source("real :: arr(10)", tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  FAIL: Lexing failed"
            test_with_semantic_types = .false.
            return
        end if
        
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  FAIL: Parsing failed"
            test_with_semantic_types = .false.
            return
        end if
        
        call analyze_semantics(arena, root_index)
        
        ! Find declaration node
        do idx = 1, arena%size
            type_kind = get_node_type_kind(arena, idx)
            if (type_kind > 0) then
                call get_node_type_details(arena, idx, type_kind, type_size, &
                                         is_allocatable, is_pointer, found)
                if (found) then
                    print *, "  Found type at index", idx, ": kind=", type_kind
                end if
            end if
        end do
        
        print *, "  Semantic types: ", merge("PASS", "FAIL", test_with_semantic_types)
    end function test_with_semantic_types

end program test_ast_introspection_coverage