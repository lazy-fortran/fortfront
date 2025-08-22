program test_string_allocatable_inference
    use semantic_analyzer, only: analyze_program, create_semantic_context, semantic_context_t
    use ast_core, only: ast_arena_t, create_ast_arena, assignment_node, identifier_node
    use lexer_core
    use parser_dispatcher_module, only: parse_statement_dispatcher
    use type_system_unified, only: mono_type_t, TCHAR
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== String Allocatable Inference Tests ==='
    print *

    ! Test string length change detection
    if (.not. test_string_length_change_detection()) all_passed = .false.
    if (.not. test_single_string_assignment()) all_passed = .false.
    if (.not. test_multiple_string_lengths()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All string allocatable inference tests passed!'
        stop 0
    else
        print *, 'Some string allocatable inference tests failed!'
        stop 1
    end if

contains

    logical function test_string_length_change_detection()
        test_string_length_change_detection = .true.
        print *, 'Testing string length change detection...'

        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            type(semantic_context_t) :: ctx
            integer :: stmt1_index, stmt2_index
            type(mono_type_t) :: inferred_type

            ! Create arena and context
            arena = create_ast_arena()
            ctx = create_semantic_context()

            ! Parse first assignment: s = "a"
            call tokenize_core('s = "a"', tokens)
            stmt1_index = parse_statement_dispatcher(tokens, arena)
            if (stmt1_index <= 0) then
                print *, 'FAIL: Could not parse first assignment'
                test_string_length_change_detection = .false.
                return
            end if

            ! Analyze first assignment
            inferred_type = ctx%infer(arena, stmt1_index)

            ! Parse second assignment: s = "ab"  
            call tokenize_core('s = "ab"', tokens)
            stmt2_index = parse_statement_dispatcher(tokens, arena)
            if (stmt2_index <= 0) then
                print *, 'FAIL: Could not parse second assignment'
                test_string_length_change_detection = .false.
                return
            end if

            ! Analyze second assignment - this should detect length change
            inferred_type = ctx%infer(arena, stmt2_index)

            ! Get the variable from the assignment target
            select type (stmt => arena%entries(stmt2_index)%node)
            type is (assignment_node)
                if (stmt%target_index > 0) then
                    select type (target => arena%entries(stmt%target_index)%node)
                    type is (identifier_node)
                        if (allocated(target%inferred_type)) then
                            ! Check if the type has allocatable flag set
                            if (.not. target%inferred_type%alloc_info%needs_allocatable_string) then
                                print *, 'FAIL: String length change not detected - needs_allocatable_string flag not set'
                                test_string_length_change_detection = .false.
                                return
                            end if
                        else
                            print *, 'FAIL: No inferred type on target'
                            test_string_length_change_detection = .false.
                            return
                        end if
                    class default
                        print *, 'FAIL: Target is not identifier'
                        test_string_length_change_detection = .false.
                        return
                    end select
                else
                    print *, 'FAIL: No target index'
                    test_string_length_change_detection = .false.
                    return
                end if
            class default
                print *, 'FAIL: Statement is not assignment'
                test_string_length_change_detection = .false.
                return
            end select

            print *, 'PASS: String length change detected correctly'
        end block
    end function test_string_length_change_detection

    logical function test_single_string_assignment()
        test_single_string_assignment = .true.
        print *, 'Testing single string assignment...'

        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            type(semantic_context_t) :: ctx
            integer :: stmt_index
            type(mono_type_t) :: inferred_type

            ! Create arena and context
            arena = create_ast_arena()
            ctx = create_semantic_context()

            ! Parse assignment: s = "hello"
            call tokenize_core('s = "hello"', tokens)
            stmt_index = parse_statement_dispatcher(tokens, arena)
            if (stmt_index <= 0) then
                print *, 'FAIL: Could not parse assignment'
                test_single_string_assignment = .false.
                return
            end if

            ! Analyze assignment
            inferred_type = ctx%infer(arena, stmt_index)

            ! Verify type is character with correct length
            if (inferred_type%kind /= TCHAR) then
                print *, 'FAIL: Type is not character'
                test_single_string_assignment = .false.
                return
            end if

            if (inferred_type%size /= 5) then
                print *, 'FAIL: String length incorrect, expected 5, got:', inferred_type%size
                test_single_string_assignment = .false.
                return
            end if

            ! Single assignment should NOT need allocatable
            if (inferred_type%alloc_info%needs_allocatable_string) then
                print *, 'FAIL: Single assignment should not need allocatable'
                test_single_string_assignment = .false.
                return
            end if

            print *, 'PASS: Single string assignment correct'
        end block
    end function test_single_string_assignment

    logical function test_multiple_string_lengths()
        test_multiple_string_lengths = .true.
        print *, 'Testing multiple string lengths...'

        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            type(semantic_context_t) :: ctx
            integer :: stmt1_index, stmt2_index, stmt3_index
            type(mono_type_t) :: inferred_type

            ! Create arena and context
            arena = create_ast_arena()
            ctx = create_semantic_context()

            ! Parse three assignments with different lengths
            call tokenize_core('s = "a"', tokens)
            stmt1_index = parse_statement_dispatcher(tokens, arena)
            inferred_type = ctx%infer(arena, stmt1_index)

            call tokenize_core('s = "hello"', tokens)
            stmt2_index = parse_statement_dispatcher(tokens, arena)
            inferred_type = ctx%infer(arena, stmt2_index)

            call tokenize_core('s = "world!"', tokens)
            stmt3_index = parse_statement_dispatcher(tokens, arena)
            inferred_type = ctx%infer(arena, stmt3_index)

            ! Check final variable type has allocatable flag
            select type (stmt => arena%entries(stmt3_index)%node)
            type is (assignment_node)
                select type (target => arena%entries(stmt%target_index)%node)
                type is (identifier_node)
                    if (.not. allocated(target%inferred_type)) then
                        print *, 'FAIL: No inferred type'
                        test_multiple_string_lengths = .false.
                        return
                    end if

                    if (.not. target%inferred_type%alloc_info%needs_allocatable_string) then
                        print *, 'FAIL: Multiple length changes should need allocatable'
                        test_multiple_string_lengths = .false.
                        return
                    end if
                end select
            end select

            print *, 'PASS: Multiple string lengths handled correctly'
        end block
    end function test_multiple_string_lengths

end program test_string_allocatable_inference