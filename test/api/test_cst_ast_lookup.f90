program test_cst_ast_lookup
    use fortfront, only: cst_arena_t, cst_handle_t, cst_node_t, &
                         create_cst_arena, create_cst_node, get_cst_node_for_ast, &
                         CST_IDENTIFIER
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== CST AST Lookup API Tests ==='
    print *

    if (.not. test_ast_lookup_map()) all_passed = .false.
    if (.not. test_ast_lookup_stale_map_fallback()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All CST AST lookup tests passed!'
        stop 0
    else
        print *, 'Some CST AST lookup tests failed!'
        stop 1
    end if

contains

    logical function test_ast_lookup_map()
        type(cst_arena_t) :: arena
        type(cst_handle_t) :: handle_x
        type(cst_handle_t) :: handle_y
        type(cst_node_t) :: node_x
        type(cst_node_t) :: node_y
        integer :: cst_index

        test_ast_lookup_map = .true.
        print *, 'Testing get_cst_node_for_ast with arena link map...'

        arena = create_cst_arena(4)
        node_x = create_cst_node(CST_IDENTIFIER, 0, 0, 'x')
        node_x%ast_link = 10
        handle_x = arena%push(node_x)

        node_y = create_cst_node(CST_IDENTIFIER, 0, 0, 'y')
        node_y%ast_link = 2000
        handle_y = arena%push(node_y)

        cst_index = get_cst_node_for_ast(arena, 10)
        if (cst_index /= handle_x%index) then
            print *, '  FAIL: expected CST index for AST 10'
            test_ast_lookup_map = .false.
            return
        end if

        cst_index = get_cst_node_for_ast(arena, 2000)
        if (cst_index /= handle_y%index) then
            print *, '  FAIL: expected CST index for AST 2000'
            test_ast_lookup_map = .false.
            return
        end if

        print *, '  PASS: get_cst_node_for_ast map lookup'
    end function test_ast_lookup_map

    logical function test_ast_lookup_stale_map_fallback()
        type(cst_arena_t) :: arena
        type(cst_handle_t) :: handle_x
        integer :: cst_index

        test_ast_lookup_stale_map_fallback = .true.
        print *, 'Testing get_cst_node_for_ast stale-map fallback...'

        arena = create_cst_arena(4)
        handle_x = arena%push(create_cst_node(CST_IDENTIFIER, 0, 0, 'x'))

        call arena%link_ast(handle_x%index, 10)

        arena%nodes(handle_x%index)%ast_link = 99

        cst_index = get_cst_node_for_ast(arena, 10)
        if (cst_index /= 0) then
            print *, '  FAIL: expected missing CST index for AST 10'
            test_ast_lookup_stale_map_fallback = .false.
            return
        end if

        cst_index = get_cst_node_for_ast(arena, 99)
        if (cst_index /= handle_x%index) then
            print *, '  FAIL: expected CST index for AST 99 via scan fallback'
            test_ast_lookup_stale_map_fallback = .false.
            return
        end if

        print *, '  PASS: stale-map fallback'
    end function test_ast_lookup_stale_map_fallback

end program test_cst_ast_lookup
