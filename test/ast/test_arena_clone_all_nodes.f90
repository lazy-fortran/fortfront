program test_arena_clone_all_nodes
    ! Test verified deep clone for ast_arena_t covering all node types.
    ! Issue #2842: Provide verified deep clone for ast_arena_t.

    use ast_arena_modern, only: ast_arena_t, create_ast_arena, destroy_ast_arena
    use ast_arena_clone, only: clone_arena, clone_subtree, clone_result_t
    use ast_factory, only: push_program, push_assignment, push_identifier, &
        push_literal, push_binary_op, push_array_literal, &
        push_component_access, push_range_subscript, push_complex_literal, &
        push_pointer_assignment, push_subroutine_call
    use ast_base, only: LITERAL_INTEGER, LITERAL_REAL, &
        LITERAL_LOGICAL
    use ast_nodes_core, only: identifier_node
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Arena Clone All Nodes Tests (Issue #2842) ==='
    print *

    if (.not. test_clone_arena_basic()) all_passed = .false.
    if (.not. test_clone_arena_independence()) all_passed = .false.
    if (.not. test_clone_arena_empty()) all_passed = .false.
    if (.not. test_clone_arena_source_text()) all_passed = .false.
    if (.not. test_clone_arena_child_indices()) all_passed = .false.
    if (.not. test_clone_subtree_basic()) all_passed = .false.
    if (.not. test_clone_subtree_single_node()) all_passed = .false.
    if (.not. test_clone_subtree_invalid_root()) all_passed = .false.
    if (.not. test_clone_core_nodes_emit()) all_passed = .false.
    if (.not. test_clone_nested_allocatables()) all_passed = .false.
    if (.not. test_clone_preserves_node_types()) all_passed = .false.
    if (.not. test_clone_large_program()) all_passed = .false.
    if (.not. test_clone_lazy_fortran_program()) all_passed = .false.
    if (.not. test_clone_subtree_from_parsed()) all_passed = .false.
    if (.not. test_clone_multiple_independent()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All arena clone tests passed!'
        stop 0
    else
        print *, 'Some arena clone tests failed!'
        stop 1
    end if

contains

    ! -----------------------------------------------------------------------
    ! Basic full arena clone
    ! -----------------------------------------------------------------------
    logical function test_clone_arena_basic()
        test_clone_arena_basic = .true.
        print *, 'Testing basic clone_arena...'

        block
            type(ast_arena_t) :: original, cloned
            integer :: id_idx, lit_idx, assign_idx, prog_idx
            integer, allocatable :: body_indices(:)

            original = create_ast_arena()
            id_idx = push_identifier(original, "x", 1, 1)
            lit_idx = push_literal(original, "42", LITERAL_INTEGER, 1, 5)
            assign_idx = push_assignment(original, id_idx, lit_idx, 1, 1)
            body_indices = [assign_idx]
            prog_idx = push_program(original, "main", body_indices, 1, 1)

            cloned = clone_arena(original)

            if (cloned%compat_size /= original%compat_size) then
                print *, '  FAIL: compat_size mismatch'
                test_clone_arena_basic = .false.
                return
            end if

            if (.not. allocated(cloned%entries(prog_idx)%node)) then
                print *, '  FAIL: program node missing in clone'
                test_clone_arena_basic = .false.
                return
            end if

            print *, '  PASS: basic clone_arena'
        end block
    end function test_clone_arena_basic

    ! -----------------------------------------------------------------------
    ! Clone independence: destroying original does not affect clone
    ! -----------------------------------------------------------------------
    logical function test_clone_arena_independence()
        test_clone_arena_independence = .true.
        print *, 'Testing clone independence...'

        block
            type(ast_arena_t) :: original, cloned
            integer :: id_idx, lit_idx, assign_idx, prog_idx
            integer, allocatable :: body_indices(:)
            integer :: saved_size

            original = create_ast_arena()
            id_idx = push_identifier(original, "val", 1, 1)
            lit_idx = push_literal(original, "99", LITERAL_INTEGER, 1, 5)
            assign_idx = push_assignment(original, id_idx, lit_idx, 1, 1)
            body_indices = [assign_idx]
            prog_idx = push_program(original, "demo", body_indices, 1, 1)

            cloned = clone_arena(original)
            saved_size = cloned%compat_size
            call destroy_ast_arena(original)

            if (cloned%compat_size /= saved_size) then
                print *, '  FAIL: size changed after destroy'
                test_clone_arena_independence = .false.
                return
            end if

            if (.not. allocated(cloned%entries(id_idx)%node)) then
                print *, '  FAIL: node lost after destroy'
                test_clone_arena_independence = .false.
                return
            end if

            print *, '  PASS: clone independence'
        end block
    end function test_clone_arena_independence

    ! -----------------------------------------------------------------------
    ! Clone empty arena
    ! -----------------------------------------------------------------------
    logical function test_clone_arena_empty()
        test_clone_arena_empty = .true.
        print *, 'Testing empty arena clone...'

        block
            type(ast_arena_t) :: original, cloned

            original = create_ast_arena()
            cloned = clone_arena(original)

            if (cloned%compat_size /= 0) then
                print *, '  FAIL: empty clone has nodes'
                test_clone_arena_empty = .false.
                return
            end if

            print *, '  PASS: empty arena clone'
        end block
    end function test_clone_arena_empty

    ! -----------------------------------------------------------------------
    ! Clone preserves source_text
    ! -----------------------------------------------------------------------
    logical function test_clone_arena_source_text()
        test_clone_arena_source_text = .true.
        print *, 'Testing source_text preservation...'

        block
            type(ast_arena_t) :: original, cloned

            original = create_ast_arena()
            original%source_text = 'program test' // new_line('a') // 'end program'

            cloned = clone_arena(original)

            if (.not. allocated(cloned%source_text)) then
                print *, '  FAIL: source_text not allocated'
                test_clone_arena_source_text = .false.
                return
            end if

            if (cloned%source_text /= original%source_text) then
                print *, '  FAIL: source_text mismatch'
                test_clone_arena_source_text = .false.
                return
            end if

            cloned%source_text = 'modified'
            if (original%source_text == 'modified') then
                print *, '  FAIL: source_text not independent'
                test_clone_arena_source_text = .false.
                return
            end if

            print *, '  PASS: source_text preservation'
        end block
    end function test_clone_arena_source_text

    ! -----------------------------------------------------------------------
    ! Clone preserves child_indices
    ! -----------------------------------------------------------------------
    logical function test_clone_arena_child_indices()
        test_clone_arena_child_indices = .true.
        print *, 'Testing child_indices preservation...'

        block
            type(ast_arena_t) :: original, cloned
            integer :: id_idx, lit_idx, assign_idx, prog_idx
            integer, allocatable :: body_indices(:)

            original = create_ast_arena()
            id_idx = push_identifier(original, "a", 1, 1)
            lit_idx = push_literal(original, "1", LITERAL_INTEGER, 1, 3)
            assign_idx = push_assignment(original, id_idx, lit_idx, 1, 1)
            body_indices = [assign_idx]
            prog_idx = push_program(original, "prog", body_indices, 1, 1)

            cloned = clone_arena(original)

            if (allocated(original%entries(prog_idx)%child_indices)) then
                if (.not. allocated(cloned%entries(prog_idx)%child_indices)) then
                    print *, '  FAIL: child_indices lost'
                    test_clone_arena_child_indices = .false.
                    return
                end if
                if (cloned%entries(prog_idx)%child_count /= &
                    original%entries(prog_idx)%child_count) then
                    print *, '  FAIL: child_count mismatch'
                    test_clone_arena_child_indices = .false.
                    return
                end if
            end if

            print *, '  PASS: child_indices preservation'
        end block
    end function test_clone_arena_child_indices

    ! -----------------------------------------------------------------------
    ! Basic subtree clone
    ! -----------------------------------------------------------------------
    logical function test_clone_subtree_basic()
        test_clone_subtree_basic = .true.
        print *, 'Testing basic clone_subtree...'

        block
            type(ast_arena_t) :: original
            type(clone_result_t) :: result
            integer :: id1, id2, lit1, lit2, binop, assign, prog
            integer, allocatable :: body_indices(:)

            original = create_ast_arena()
            id1 = push_identifier(original, "x", 1, 1)
            id2 = push_identifier(original, "y", 1, 5)
            lit1 = push_literal(original, "1", LITERAL_INTEGER, 1, 7)
            lit2 = push_literal(original, "2", LITERAL_INTEGER, 1, 9)
            binop = push_binary_op(original, lit1, lit2, "+", 1, 7)
            assign = push_assignment(original, id1, binop, 1, 1)
            body_indices = [assign]
            prog = push_program(original, "main", body_indices, 1, 1)

            result = clone_subtree(original, prog)

            if (result%root_index <= 0) then
                print *, '  FAIL: root_index not set'
                test_clone_subtree_basic = .false.
                return
            end if

            if (result%cloned_arena%compat_size < 1) then
                print *, '  FAIL: cloned arena empty'
                test_clone_subtree_basic = .false.
                return
            end if

            print *, '  PASS: basic clone_subtree'
        end block
    end function test_clone_subtree_basic

    ! -----------------------------------------------------------------------
    ! Subtree clone of single node (leaf)
    ! -----------------------------------------------------------------------
    logical function test_clone_subtree_single_node()
        test_clone_subtree_single_node = .true.
        print *, 'Testing single-node clone_subtree...'

        block
            type(ast_arena_t) :: original
            type(clone_result_t) :: result
            integer :: id_idx

            original = create_ast_arena()
            id_idx = push_identifier(original, "leaf", 1, 1)

            result = clone_subtree(original, id_idx)

            if (result%root_index <= 0) then
                print *, '  FAIL: single node root not set'
                test_clone_subtree_single_node = .false.
                return
            end if

            if (result%cloned_arena%compat_size /= 1) then
                print *, '  FAIL: expected 1 node, got', &
                    result%cloned_arena%compat_size
                test_clone_subtree_single_node = .false.
                return
            end if

            print *, '  PASS: single-node clone_subtree'
        end block
    end function test_clone_subtree_single_node

    ! -----------------------------------------------------------------------
    ! Subtree clone with invalid root returns empty arena
    ! -----------------------------------------------------------------------
    logical function test_clone_subtree_invalid_root()
        test_clone_subtree_invalid_root = .true.
        print *, 'Testing invalid root clone_subtree...'

        block
            type(ast_arena_t) :: original
            type(clone_result_t) :: result
            integer :: id_idx

            original = create_ast_arena()
            id_idx = push_identifier(original, "x", 1, 1)

            result = clone_subtree(original, 999)

            if (result%root_index /= 0) then
                print *, '  FAIL: invalid root should give root_index=0'
                test_clone_subtree_invalid_root = .false.
                return
            end if

            print *, '  PASS: invalid root clone_subtree'
        end block
    end function test_clone_subtree_invalid_root

    ! -----------------------------------------------------------------------
    ! Clone core node types and verify emit equality
    ! -----------------------------------------------------------------------
    logical function test_clone_core_nodes_emit()
        test_clone_core_nodes_emit = .true.
        print *, 'Testing clone emit equality for core nodes...'

        block
            type(ast_arena_t) :: original, cloned
            integer :: idx1, idx2, idx3, idx4, idx5, idx6, idx7, idx8, &
                idx9, idx10, prog_idx
            integer, allocatable :: body(:)

            original = create_ast_arena()

            idx1 = push_identifier(original, "x", 1, 1)
            idx2 = push_literal(original, "42", LITERAL_INTEGER, 1, 5)
            idx3 = push_literal(original, "3.14", LITERAL_REAL, 1, 10)
            idx4 = push_binary_op(original, idx1, idx2, "+", 1, 1)
            idx5 = push_assignment(original, idx1, idx4, 1, 1)
            idx6 = push_array_literal(original, [idx2], 1, 1)
            idx7 = push_component_access(original, idx1, "field", 1, 1)
            idx8 = push_range_subscript(original, idx1, idx2, idx3, 1, 1)
            idx9 = push_complex_literal(original, idx2, idx3, 1, 1)
            idx10 = push_pointer_assignment(original, idx1, idx7, 1, 1)

            body = [idx1, idx2, idx3, idx4, idx5, idx6, idx7, idx8, idx9, idx10]
            prog_idx = push_program(original, "core_test", body, 1, 1)

            cloned = clone_arena(original)

            if (.not. verify_emit_equality(original, cloned, prog_idx)) then
                print *, '  FAIL: core nodes emit mismatch'
                test_clone_core_nodes_emit = .false.
                return
            end if

            print *, '  PASS: core nodes emit equality'
        end block
    end function test_clone_core_nodes_emit

    ! -----------------------------------------------------------------------
    ! Clone nodes with nested allocatable components
    ! -----------------------------------------------------------------------
    logical function test_clone_nested_allocatables()
        test_clone_nested_allocatables = .true.
        print *, 'Testing nested allocatable deep copy...'

        block
            type(ast_arena_t) :: original, cloned
            integer :: id1, id2, lit1, binop, assign, prog_idx
            integer, allocatable :: body(:)

            original = create_ast_arena()

            id1 = push_identifier(original, "x", 1, 1)
            id2 = push_identifier(original, "y", 1, 5)
            lit1 = push_literal(original, "1", LITERAL_INTEGER, 1, 7)
            binop = push_binary_op(original, id1, lit1, "+", 1, 1)
            assign = push_assignment(original, id2, binop, 1, 1)
            body = [assign]
            prog_idx = push_program(original, "nested_test", body, 1, 1)

            cloned = clone_arena(original)

            ! Modify original node data - clone must be independent
            select type (n => original%entries(id1)%node)
                type is (identifier_node)
                    n%name = "modified_x"
            class default
            end select

            select type (n => cloned%entries(id1)%node)
                type is (identifier_node)
                    if (n%name == "modified_x") then
                        print *, '  FAIL: allocatable not deep copied'
                        test_clone_nested_allocatables = .false.
                        return
                    end if
            class default
            end select

            print *, '  PASS: nested allocatable deep copy'
        end block
    end function test_clone_nested_allocatables

    ! -----------------------------------------------------------------------
    ! Clone preserves node_type strings for all entries
    ! -----------------------------------------------------------------------
    logical function test_clone_preserves_node_types()
        test_clone_preserves_node_types = .true.
        print *, 'Testing node_type preservation in clone...'

        block
            type(ast_arena_t) :: original, cloned
            integer :: id_idx, lit_idx, assign_idx, prog_idx
            integer, allocatable :: body_indices(:)
            integer :: idx

            original = create_ast_arena()
            id_idx = push_identifier(original, "x", 1, 1)
            lit_idx = push_literal(original, "42", LITERAL_INTEGER, 1, 5)
            assign_idx = push_assignment(original, id_idx, lit_idx, 1, 1)
            body_indices = [assign_idx]
            prog_idx = push_program(original, "main", body_indices, 1, 1)

            cloned = clone_arena(original)

            do idx = 1, original%compat_size
                if (allocated(original%entries(idx)%node_type)) then
                    if (.not. allocated(cloned%entries(idx)%node_type)) then
                        print *, '  FAIL: node_type lost at index', idx
                        test_clone_preserves_node_types = .false.
                        return
                    end if
                    if (cloned%entries(idx)%node_type /= &
                        original%entries(idx)%node_type) then
                        print *, '  FAIL: node_type mismatch at index', idx
                        test_clone_preserves_node_types = .false.
                        return
                    end if
                end if
            end do

            print *, '  PASS: node_type preservation'
        end block
    end function test_clone_preserves_node_types

    ! -----------------------------------------------------------------------
    ! Clone large parsed program and verify emit equality
    ! -----------------------------------------------------------------------
    logical function test_clone_large_program()
        test_clone_large_program = .true.
        print *, 'Testing clone of large parsed program...'

        block
            type(ast_arena_t) :: original, cloned
            type(token_t), allocatable :: tokens(:)
            character(len=:), allocatable :: source
            character(len=:), allocatable :: emit_orig, emit_clone
            character(len=:), allocatable :: error_msg
            integer :: prog_idx

            call read_example('examples/f90/call_graph_module_program_scopes.f90', &
                source)

            call lex_source(source, tokens, error_msg)
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: lex error:', trim(error_msg)
                test_clone_large_program = .false.
                return
            end if

            original = create_ast_arena()
            call parse_tokens(tokens, original, prog_idx, error_msg)
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: parse error:', trim(error_msg)
                test_clone_large_program = .false.
                return
            end if

            call emit_fortran(original, prog_idx, emit_orig)

            cloned = clone_arena(original)
            call destroy_ast_arena(original)

            call emit_fortran(cloned, prog_idx, emit_clone)

            if (trim(emit_clone) /= trim(emit_orig)) then
                print *, '  FAIL: emit mismatch after clone'
                print *, '    original len:', len_trim(emit_orig)
                print *, '    clone len:   ', len_trim(emit_clone)
                test_clone_large_program = .false.
                return
            end if

            print *, '  PASS: large program clone'
        end block
    end function test_clone_large_program

    ! -----------------------------------------------------------------------
    ! Clone a Lazy Fortran program (exercises type inference nodes)
    ! -----------------------------------------------------------------------
    logical function test_clone_lazy_fortran_program()
        test_clone_lazy_fortran_program = .true.
        print *, 'Testing clone of Lazy Fortran program...'

        block
            type(ast_arena_t) :: original, cloned
            type(token_t), allocatable :: tokens(:)
            character(len=:), allocatable :: source
            character(len=:), allocatable :: emit_orig, emit_clone
            character(len=:), allocatable :: error_msg
            integer :: prog_idx

            call read_example('examples/lf/basic_function.lf', source)

            call lex_source(source, tokens, error_msg)
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: lex error:', trim(error_msg)
                test_clone_lazy_fortran_program = .false.
                return
            end if

            original = create_ast_arena()
            call parse_tokens(tokens, original, prog_idx, error_msg)
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: parse error:', trim(error_msg)
                test_clone_lazy_fortran_program = .false.
                return
            end if

            call emit_fortran(original, prog_idx, emit_orig)

            cloned = clone_arena(original)

            call emit_fortran(cloned, prog_idx, emit_clone)

            if (trim(emit_clone) /= trim(emit_orig)) then
                print *, '  FAIL: emit mismatch after clone'
                test_clone_lazy_fortran_program = .false.
                return
            end if

            print *, '  PASS: Lazy Fortran program clone'
        end block
    end function test_clone_lazy_fortran_program

    ! -----------------------------------------------------------------------
    ! Clone subtree from a parsed program
    ! -----------------------------------------------------------------------
    logical function test_clone_subtree_from_parsed()
        test_clone_subtree_from_parsed = .true.
        print *, 'Testing clone_subtree from parsed program...'

        block
            type(ast_arena_t) :: original
            type(clone_result_t) :: result
            type(token_t), allocatable :: tokens(:)
            character(len=:), allocatable :: source
            character(len=:), allocatable :: error_msg
            integer :: prog_idx

            call read_example('examples/f90/call_graph_module_program_scopes.f90', &
                source)

            call lex_source(source, tokens, error_msg)
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: lex error:', trim(error_msg)
                test_clone_subtree_from_parsed = .false.
                return
            end if

            original = create_ast_arena()
            call parse_tokens(tokens, original, prog_idx, error_msg)
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: parse error:', trim(error_msg)
                test_clone_subtree_from_parsed = .false.
                return
            end if

            result = clone_subtree(original, prog_idx)

            if (result%root_index <= 0) then
                print *, '  FAIL: subtree root not set'
                test_clone_subtree_from_parsed = .false.
                return
            end if

            if (result%cloned_arena%compat_size < original%compat_size / 2) then
                print *, '  FAIL: subtree too small:', &
                    result%cloned_arena%compat_size, 'vs', &
                    original%compat_size
                test_clone_subtree_from_parsed = .false.
                return
            end if

            print *, '  PASS: clone_subtree from parsed program'
        end block
    end function test_clone_subtree_from_parsed

    ! -----------------------------------------------------------------------
    ! Multiple independent clones from same original
    ! -----------------------------------------------------------------------
    logical function test_clone_multiple_independent()
        test_clone_multiple_independent = .true.
        print *, 'Testing multiple independent clones...'

        block
            type(ast_arena_t) :: original, clone1, clone2, clone3
            integer :: id_idx, lit_idx, assign_idx, prog_idx
            integer, allocatable :: body_indices(:)
            character(len=:), allocatable :: emit1, emit2, emit3

            original = create_ast_arena()
            id_idx = push_identifier(original, "x", 1, 1)
            lit_idx = push_literal(original, "42", LITERAL_INTEGER, 1, 5)
            assign_idx = push_assignment(original, id_idx, lit_idx, 1, 1)
            body_indices = [assign_idx]
            prog_idx = push_program(original, "main", body_indices, 1, 1)

            clone1 = clone_arena(original)
            clone2 = clone_arena(original)
            clone3 = clone_arena(original)

            call emit_fortran(clone1, prog_idx, emit1)
            call emit_fortran(clone2, prog_idx, emit2)
            call emit_fortran(clone3, prog_idx, emit3)

            if (trim(emit1) /= trim(emit2) .or. trim(emit2) /= trim(emit3)) then
                print *, '  FAIL: multiple clones produce different output'
                test_clone_multiple_independent = .false.
                return
            end if

            print *, '  PASS: multiple independent clones'
        end block
    end function test_clone_multiple_independent

    ! -----------------------------------------------------------------------
    ! Helper: verify emit(original) == emit(cloned)
    ! -----------------------------------------------------------------------
    logical function verify_emit_equality(original, cloned, prog_idx)
        type(ast_arena_t), intent(in) :: original, cloned
        integer, intent(in) :: prog_idx
        character(len=:), allocatable :: emit_orig, emit_clone

        call emit_fortran(original, prog_idx, emit_orig)
        call emit_fortran(cloned, prog_idx, emit_clone)

        verify_emit_equality = (trim(emit_orig) == trim(emit_clone))
    end function verify_emit_equality

    include '../common/read_example.inc'

end program test_arena_clone_all_nodes
