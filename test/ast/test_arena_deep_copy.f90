program test_arena_deep_copy
    ! Test that copying ast_arena_t performs a deep copy of entries,
    ! source_text, and source_line_starts.
    ! Issue #2840: AST arena copy drops entries.

    use ast_arena_modern, only: ast_arena_t, create_ast_arena, destroy_ast_arena
    use ast_factory, only: push_program, push_assignment, push_identifier, &
        push_literal
    use ast_base, only: LITERAL_INTEGER
    use lexer_core, only: token_t
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Arena Deep Copy Tests (Issue #2840) ==='
    print *

    if (.not. test_arena_copy_preserves_entries()) all_passed = .false.
    if (.not. test_copy_independent_of_original()) all_passed = .false.
    if (.not. test_copy_source_text_and_lines()) all_passed = .false.
    if (.not. test_copy_empty_arena()) all_passed = .false.
    if (.not. test_copy_preserves_node_types()) all_passed = .false.
    if (.not. test_copy_preserves_child_indices()) all_passed = .false.
    if (.not. test_parse_emit_roundtrip()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All arena deep copy tests passed!'
        stop 0
    else
        print *, 'Some arena deep copy tests failed!'
        stop 1
    end if

contains

    ! Test that arena copy preserves entries (nodes, node_type, parent, depth)
    logical function test_arena_copy_preserves_entries()
        test_arena_copy_preserves_entries = .true.
        print *, 'Testing arena copy preserves entries...'

        block
            type(ast_arena_t) :: original, copy
            integer :: id_idx, lit_idx, assign_idx, prog_idx
            integer, allocatable :: body_indices(:)
            logical :: entries_before, entries_after

            original = create_ast_arena()

            id_idx = push_identifier(original, "x", 1, 1)
            lit_idx = push_literal(original, "42", LITERAL_INTEGER, 1, 5)
            assign_idx = push_assignment(original, id_idx, lit_idx, 1, 1)
            body_indices = [assign_idx]
            prog_idx = push_program(original, "test_prog", body_indices, 1, 1)

            entries_before = allocated(original%entries)
            print *, '    DIAG: entries_before=', entries_before, ' compat_size=', original%compat_size
            copy = original
            entries_after = allocated(copy%entries)
            print *, '    DIAG: entries_after=', entries_after, ' compat_size=', copy%compat_size

            ! CRITICAL: entries must be allocated in copy
            ! This is the core issue #2840 - base assignment drops extension components
            if (.not. entries_before) then
                print *, '  FAIL: original entries not allocated (setup error)'
                test_arena_copy_preserves_entries = .false.
                return
            end if

            if (.not. entries_after) then
                print *, '  FAIL: entries not allocated in copy (issue #2840)'
                test_arena_copy_preserves_entries = .false.
                return
            end if

            ! Verify compat_size
            if (copy%compat_size /= original%compat_size) then
                print *, '  FAIL: compat_size mismatch'
                print *, '    original:', original%compat_size, 'copy:', copy%compat_size
                test_arena_copy_preserves_entries = .false.
                return
            end if

            ! Verify nodes present
            if (.not. allocated(copy%entries(id_idx)%node)) then
                print *, '  FAIL: identifier node lost'
                test_arena_copy_preserves_entries = .false.
                return
            end if

            if (.not. allocated(copy%entries(lit_idx)%node)) then
                print *, '  FAIL: literal node lost'
                test_arena_copy_preserves_entries = .false.
                return
            end if

            if (.not. allocated(copy%entries(assign_idx)%node)) then
                print *, '  FAIL: assignment node lost'
                test_arena_copy_preserves_entries = .false.
                return
            end if

            if (.not. allocated(copy%entries(prog_idx)%node)) then
                print *, '  FAIL: program node lost'
                test_arena_copy_preserves_entries = .false.
                return
            end if

            ! Verify node_type strings
            if (.not. allocated(copy%entries(id_idx)%node_type)) then
                print *, '  FAIL: identifier node_type lost'
                test_arena_copy_preserves_entries = .false.
                return
            end if

            if (copy%entries(id_idx)%node_type /= &
                original%entries(id_idx)%node_type) then
                print *, '  FAIL: identifier node_type mismatch'
                test_arena_copy_preserves_entries = .false.
                return
            end if

            ! Verify parent/depth
            if (copy%entries(assign_idx)%parent_index /= &
                original%entries(assign_idx)%parent_index) then
                print *, '  FAIL: parent_index mismatch'
                test_arena_copy_preserves_entries = .false.
                return
            end if

            if (copy%entries(assign_idx)%depth /= &
                original%entries(assign_idx)%depth) then
                print *, '  FAIL: depth mismatch'
                test_arena_copy_preserves_entries = .false.
                return
            end if

            print *, '  PASS: arena copy preserves entries'
        end block
    end function test_arena_copy_preserves_entries

    ! Test that copy is independent: destroying original does not affect copy
    logical function test_copy_independent_of_original()
        test_copy_independent_of_original = .true.
        print *, 'Testing copy independence from original...'

        block
            type(ast_arena_t) :: original, copy
            integer :: id_idx, lit_idx, assign_idx, prog_idx
            integer, allocatable :: body_indices(:)
            integer :: saved_compat_size

            original = create_ast_arena()

            id_idx = push_identifier(original, "val", 1, 1)
            lit_idx = push_literal(original, "99", LITERAL_INTEGER, 1, 5)
            assign_idx = push_assignment(original, id_idx, lit_idx, 1, 1)
            body_indices = [assign_idx]
            prog_idx = push_program(original, "demo", body_indices, 1, 1)

            saved_compat_size = original%compat_size
            copy = original
            call destroy_ast_arena(original)

            ! Copy must still have all nodes
            if (copy%compat_size /= saved_compat_size) then
                print *, '  FAIL: compat_size changed after destroy'
                test_copy_independent_of_original = .false.
                return
            end if

            if (.not. allocated(copy%entries)) then
                print *, '  FAIL: entries deallocated after destroy'
                test_copy_independent_of_original = .false.
                return
            end if

            if (.not. allocated(copy%entries(id_idx)%node)) then
                print *, '  FAIL: identifier gone after destroy'
                test_copy_independent_of_original = .false.
                return
            end if

            if (.not. allocated(copy%entries(lit_idx)%node)) then
                print *, '  FAIL: literal gone after destroy'
                test_copy_independent_of_original = .false.
                return
            end if

            if (.not. allocated(copy%entries(prog_idx)%node)) then
                print *, '  FAIL: program gone after destroy'
                test_copy_independent_of_original = .false.
                return
            end if

            if (.not. allocated(copy%entries(id_idx)%node_type)) then
                print *, '  FAIL: node_type string gone after destroy'
                test_copy_independent_of_original = .false.
                return
            end if

            print *, '  PASS: copy independent of original'
        end block
    end function test_copy_independent_of_original

    ! Test source_text and source_line_starts are deep copied
    logical function test_copy_source_text_and_lines()
        test_copy_source_text_and_lines = .true.
        print *, 'Testing source_text and source_line_starts copy...'

        block
            type(ast_arena_t) :: original, copy
            character(len=50) :: test_source
            integer, allocatable :: line_starts(:)

            original = create_ast_arena()

            test_source = 'program test_prog' // new_line('a') // &
                'integer :: x' // new_line('a') // &
                'x = 42'
            original%source_text = trim(test_source)

            allocate (line_starts(4))
            line_starts = [1, 18, 32, 42]
            original%source_line_starts = line_starts

            copy = original

            if (.not. allocated(copy%source_text)) then
                print *, '  FAIL: source_text not allocated'
                test_copy_source_text_and_lines = .false.
                return
            end if

            if (copy%source_text /= original%source_text) then
                print *, '  FAIL: source_text mismatch'
                test_copy_source_text_and_lines = .false.
                return
            end if

            if (.not. allocated(copy%source_line_starts)) then
                print *, '  FAIL: source_line_starts not allocated'
                test_copy_source_text_and_lines = .false.
                return
            end if

            if (size(copy%source_line_starts) /= &
                size(original%source_line_starts)) then
                print *, '  FAIL: source_line_starts size mismatch'
                test_copy_source_text_and_lines = .false.
                return
            end if

            if (any(copy%source_line_starts /= &
                original%source_line_starts)) then
                print *, '  FAIL: source_line_starts values mismatch'
                test_copy_source_text_and_lines = .false.
                return
            end if

            ! Verify independence: modify copy source_text, original unchanged
            copy%source_text = 'modified'
            if (original%source_text == 'modified') then
                print *, '  FAIL: source_text not independent'
                test_copy_source_text_and_lines = .false.
                return
            end if

            print *, '  PASS: source_text and source_line_starts copied'
        end block
    end function test_copy_source_text_and_lines

    ! Test copying an empty arena
    logical function test_copy_empty_arena()
        test_copy_empty_arena = .true.
        print *, 'Testing empty arena copy...'

        block
            type(ast_arena_t) :: original, copy

            original = create_ast_arena()
            copy = original

            if (copy%compat_size /= 0) then
                print *, '  FAIL: empty copy has non-zero compat_size'
                test_copy_empty_arena = .false.
                return
            end if

            print *, '  PASS: empty arena copy'
        end block
    end function test_copy_empty_arena

    ! Test that node_type strings are preserved for all node types
    logical function test_copy_preserves_node_types()
        test_copy_preserves_node_types = .true.
        print *, 'Testing node_type strings preserved in copy...'

        block
            type(ast_arena_t) :: original, copy
            integer :: id_idx, lit_idx, assign_idx, prog_idx
            integer, allocatable :: body_indices(:)
            integer :: idx

            original = create_ast_arena()

            id_idx = push_identifier(original, "x", 1, 1)
            lit_idx = push_literal(original, "42", LITERAL_INTEGER, 1, 5)
            assign_idx = push_assignment(original, id_idx, lit_idx, 1, 1)
            body_indices = [assign_idx]
            prog_idx = push_program(original, "main", body_indices, 1, 1)

            copy = original

            ! Check every populated entry
            do idx = 1, original%compat_size
                if (allocated(original%entries(idx)%node_type)) then
                    if (.not. allocated(copy%entries(idx)%node_type)) then
                        print *, '  FAIL: node_type lost at index', idx
                        test_copy_preserves_node_types = .false.
                        return
                    end if
                    if (copy%entries(idx)%node_type /= &
                        original%entries(idx)%node_type) then
                        print *, '  FAIL: node_type mismatch at index', idx
                        test_copy_preserves_node_types = .false.
                        return
                    end if
                end if
            end do

            print *, '  PASS: node_type strings preserved'
        end block
    end function test_copy_preserves_node_types

    ! Test that child_indices arrays are deep copied
    logical function test_copy_preserves_child_indices()
        test_copy_preserves_child_indices = .true.
        print *, 'Testing child_indices deep copy...'

        block
            type(ast_arena_t) :: original, copy
            integer :: id_idx, lit_idx, assign_idx, prog_idx
            integer, allocatable :: body_indices(:)

            original = create_ast_arena()

            id_idx = push_identifier(original, "a", 1, 1)
            lit_idx = push_literal(original, "1", LITERAL_INTEGER, 1, 3)
            assign_idx = push_assignment(original, id_idx, lit_idx, 1, 1)
            body_indices = [assign_idx]
            prog_idx = push_program(original, "prog", body_indices, 1, 1)

            copy = original

            ! If original has child_indices, copy must too
            if (allocated(original%entries(prog_idx)%child_indices)) then
                if (.not. allocated(copy%entries(prog_idx)%child_indices)) then
                    print *, '  FAIL: child_indices lost in copy'
                    test_copy_preserves_child_indices = .false.
                    return
                end if
                if (copy%entries(prog_idx)%child_count /= &
                    original%entries(prog_idx)%child_count) then
                    print *, '  FAIL: child_count mismatch'
                    test_copy_preserves_child_indices = .false.
                    return
                end if
            end if

            print *, '  PASS: child_indices deep copied'
        end block
    end function test_copy_preserves_child_indices

    ! Test parse -> emit round-trip with arena copy (MISSING-PARSE-EMIT-ROUNDTRIP)
    ! Reads a real example, parses, copies arena, destroys original, emits copy,
    ! and verifies emitted output matches original emission.
    logical function test_parse_emit_roundtrip()
        test_parse_emit_roundtrip = .true.
        print *, 'Testing parse/emit round-trip with arena copy...'

        block
            type(ast_arena_t) :: original, copy
            type(token_t), allocatable :: tokens(:)
            character(len=:), allocatable :: source
            character(len=:), allocatable :: emit_original, emit_copy
            character(len=:), allocatable :: error_msg
            integer :: prog_idx
            integer :: saved_compat_size, idx

            call read_example('examples/f90/call_graph_module_program_scopes.f90', &
                source)

            call lex_source(source, tokens, error_msg)
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: lex error:', trim(error_msg)
                test_parse_emit_roundtrip = .false.
                return
            end if

            original = create_ast_arena()
            call parse_tokens(tokens, original, prog_idx, error_msg)
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: parse error:', trim(error_msg)
                test_parse_emit_roundtrip = .false.
                return
            end if

            if (original%compat_size == 0) then
                print *, '  FAIL: parse produced empty arena'
                test_parse_emit_roundtrip = .false.
                return
            end if

            saved_compat_size = original%compat_size

            call emit_fortran(original, prog_idx, emit_original)

            copy = original
            call destroy_ast_arena(original)

            if (copy%compat_size /= saved_compat_size) then
                print *, '  FAIL: compat_size lost after destroy'
                print *, '    expected:', saved_compat_size, ' got:', copy%compat_size
                test_parse_emit_roundtrip = .false.
                return
            end if

            if (.not. allocated(copy%entries)) then
                print *, '  FAIL: entries deallocated after destroy'
                test_parse_emit_roundtrip = .false.
                return
            end if

            do idx = 1, copy%compat_size
                if (allocated(copy%entries(idx)%node)) then
                    if (.not. allocated(copy%entries(idx)%node_type)) then
                        print *, '  FAIL: node_type missing at index', idx
                        test_parse_emit_roundtrip = .false.
                        return
                    end if
                end if
            end do

            call emit_fortran(copy, prog_idx, emit_copy)

            if (.not. allocated(emit_original)) then
                print *, '  FAIL: original emit produced no output'
                test_parse_emit_roundtrip = .false.
                return
            end if

            if (.not. allocated(emit_copy)) then
                print *, '  FAIL: copy emit produced no output'
                test_parse_emit_roundtrip = .false.
                return
            end if

            if (trim(emit_copy) /= trim(emit_original)) then
                print *, '  FAIL: emit mismatch after arena copy'
                print *, '    original length:', len_trim(emit_original)
                print *, '    copy length:    ', len_trim(emit_copy)
                test_parse_emit_roundtrip = .false.
                return
            end if

            print *, '  PASS: parse/emit round-trip with arena copy'
        end block
    end function test_parse_emit_roundtrip

    include '../common/read_example.inc'

end program test_arena_deep_copy
