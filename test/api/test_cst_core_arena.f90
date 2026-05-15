program test_cst_core_arena
    use cst_arena, only: cst_arena_t, cst_handle_t, create_cst_arena
    use cst_core, only: add_child_to_cst_node, add_leading_trivia, &
                        add_trailing_trivia, create_cst_node, create_trivia, &
                        get_node_kind_name, is_trivia_kind, set_cst_node_text, &
                        validate_cst_node, validate_trivia
    use cst_nodes, only: cst_node_t, trivia_t, CST_ASSIGNMENT, CST_COMMENT, &
                         CST_IDENTIFIER, CST_NEWLINE, CST_WHITESPACE
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== CST Core and Arena Tests ==='
    print *

    if (.not. test_node_creation_and_trivia()) all_passed = .false.
    if (.not. test_arena_growth_handles_and_clear()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All CST core and arena tests passed!'
        stop 0
    else
        print *, 'Some CST core and arena tests failed!'
        stop 1
    end if

contains

    logical function test_node_creation_and_trivia()
        type(cst_node_t) :: node
        type(cst_node_t) :: copied
        type(trivia_t) :: leading
        type(trivia_t) :: trailing

        test_node_creation_and_trivia = .true.
        print *, 'Testing CST node creation, mutation, and validation...'

        node = create_cst_node(CST_ASSIGNMENT, 4, 9, 'x = 1')
        call add_child_to_cst_node(node, 2)
        call add_child_to_cst_node(node, 3)
        call set_cst_node_text(node, 'x = 42')

        leading = create_trivia(CST_COMMENT, '! keep me', 1, 9)
        trailing = create_trivia(CST_WHITESPACE, ' ', 10, 10)
        call add_leading_trivia(node, leading)
        call add_trailing_trivia(node, trailing)

        if (.not. validate_cst_node(node)) then
            print *, '  FAIL: expected constructed CST node to validate'
            test_node_creation_and_trivia = .false.
            return
        end if

        if (.not. validate_trivia(leading)) then
            print *, '  FAIL: expected comment trivia to validate'
            test_node_creation_and_trivia = .false.
            return
        end if

        if (.not. is_trivia_kind(CST_COMMENT) .or. &
            .not. is_trivia_kind(CST_NEWLINE) .or. &
            .not. is_trivia_kind(CST_WHITESPACE)) then
            print *, '  FAIL: expected trivia kind detection for trivia constants'
            test_node_creation_and_trivia = .false.
            return
        end if

        if (is_trivia_kind(CST_IDENTIFIER)) then
            print *, '  FAIL: identifier kind must not be trivia'
            test_node_creation_and_trivia = .false.
            return
        end if

        if (get_node_kind_name(CST_ASSIGNMENT) /= 'ASSIGNMENT') then
            print *, '  FAIL: unexpected assignment kind name'
            test_node_creation_and_trivia = .false.
            return
        end if

        copied = node
        if (.not. allocated(copied%children) .or. size(copied%children) /= 2) then
            print *, '  FAIL: deep copy lost children'
            test_node_creation_and_trivia = .false.
            return
        end if
        if (.not. allocated(copied%leading_trivia) .or. &
            trim(copied%leading_trivia(1)%text) /= '! keep me') then
            print *, '  FAIL: deep copy lost leading trivia'
            test_node_creation_and_trivia = .false.
            return
        end if
        if (.not. allocated(copied%text) .or. copied%text /= 'x = 42') then
            print *, '  FAIL: deep copy lost node text'
            test_node_creation_and_trivia = .false.
            return
        end if

        print *, '  PASS: CST node creation, mutation, validation, and copy'
    end function test_node_creation_and_trivia

    logical function test_arena_growth_handles_and_clear()
        type(cst_arena_t) :: arena
        type(cst_handle_t) :: first
        type(cst_handle_t) :: second
        type(cst_handle_t) :: third
        type(cst_node_t) :: fetched

        test_arena_growth_handles_and_clear = .true.
        print *, 'Testing CST arena growth, handles, links, and clear...'

        arena = create_cst_arena(1)
        first = arena%push(create_cst_node(CST_IDENTIFIER, 1, 1, 'x'))
        second = arena%push(create_cst_node(CST_IDENTIFIER, 5, 5, 'y'))
        third = arena%push(create_cst_node(CST_IDENTIFIER, 9, 9, 'z'))

        if (arena%size /= 3) then
            print *, '  FAIL: arena size should track pushed nodes'
            test_arena_growth_handles_and_clear = .false.
            return
        end if
        if (arena%capacity < 3) then
            print *, '  FAIL: arena did not grow beyond initial capacity'
            test_arena_growth_handles_and_clear = .false.
            return
        end if

        if (.not. arena%is_valid_handle(first) .or. &
            .not. arena%is_valid_handle(second) .or. &
            .not. arena%is_valid_handle(third)) then
            print *, '  FAIL: pushed handles should be valid'
            test_arena_growth_handles_and_clear = .false.
            return
        end if

        fetched = arena%get(second)
        if (.not. allocated(fetched%text) .or. fetched%text /= 'y') then
            print *, '  FAIL: arena get returned the wrong node'
            test_arena_growth_handles_and_clear = .false.
            return
        end if

        call arena%link_ast(third%index, 42)
        if (.not. allocated(arena%ast_to_cst)) then
            print *, '  FAIL: AST lookup map was not allocated'
            test_arena_growth_handles_and_clear = .false.
            return
        end if
        if (arena%ast_to_cst(42) /= third%index) then
            print *, '  FAIL: AST lookup map did not record link'
            test_arena_growth_handles_and_clear = .false.
            return
        end if

        call arena%clear()
        if (arena%size /= 0) then
            print *, '  FAIL: clear should reset arena size'
            test_arena_growth_handles_and_clear = .false.
            return
        end if
        if (arena%is_valid_handle(first)) then
            print *, '  FAIL: clear should invalidate old handles'
            test_arena_growth_handles_and_clear = .false.
            return
        end if
        if (allocated(arena%ast_to_cst)) then
            print *, '  FAIL: clear should release AST lookup map'
            test_arena_growth_handles_and_clear = .false.
            return
        end if

        print *, '  PASS: CST arena growth, handles, links, and clear'
    end function test_arena_growth_handles_and_clear

end program test_cst_core_arena
