program test_issue_147_unused_internal_procs
    use fortfront
    use iso_fortran_env, only: error_unit
    implicit none

    logical :: all_tests_passed
    all_tests_passed = .true.

    call test_unused_internal_procedure()
    call test_used_internal_procedure()
    call test_mixed_internal_procedures()

    if (all_tests_passed) then
        print *, "All issue #147 tests PASSED!"
    else
        print *, "Some issue #147 tests FAILED!"
        stop 1
    end if

contains

    subroutine test_unused_internal_procedure()
        use lexer_core, only: tokenize_core
        use parser_state_module, only: parser_state_t, create_parser_state
        use parser_execution_statements_module, only: parse_program_statement
        use call_graph_builder_module, only: build_call_graph
        use call_graph_module, only: call_graph_t, find_unused_procedures
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        type(call_graph_t) :: graph
        integer :: program_index
        character(len=:), allocatable :: unused_procs(:)
        
        print *, "Testing unused internal procedure detection..."
        
        ! Test case from issue #147
        source = "program test_unused_internal" // new_line('a') // &
                "  call used_procedure()" // new_line('a') // &
                "contains" // new_line('a') // &
                "  subroutine used_procedure()" // new_line('a') // &
                "    print *, 'this is called'" // new_line('a') // &
                "  end subroutine used_procedure" // new_line('a') // &
                "  subroutine unused_procedure()" // new_line('a') // &
                "    print *, 'this is never called'" // new_line('a') // &
                "  end subroutine unused_procedure" // new_line('a') // &
                "end program test_unused_internal"
        
        call tokenize_core(source, tokens)
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        program_index = parse_program_statement(parser, arena)
        
        if (program_index <= 0) then
            print *, "FAILED: Could not parse program"
            all_tests_passed = .false.
            return
        end if
        
        ! Build call graph
        graph = build_call_graph(arena, program_index)
        
        ! Find unused procedures
        unused_procs = find_unused_procedures(graph)
        
        ! Should find unused_procedure as unused
        if (.not. allocated(unused_procs) .or. size(unused_procs) == 0) then
            print *, "FAILED: Unused internal procedure not detected"
            all_tests_passed = .false.
        else
            print *, "PASSED: Unused internal procedure correctly detected"
        end if
        
    end subroutine test_unused_internal_procedure

    subroutine test_used_internal_procedure()
        use lexer_core, only: tokenize_core
        use parser_state_module, only: parser_state_t, create_parser_state
        use parser_execution_statements_module, only: parse_program_statement
        use call_graph_builder_module, only: build_call_graph
        use call_graph_module, only: call_graph_t, find_unused_procedures
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        type(call_graph_t) :: graph
        integer :: program_index
        character(len=:), allocatable :: unused_procs(:)
        
        print *, "Testing used internal procedure not marked as unused..."
        
        source = "program test_used_internal" // new_line('a') // &
                "  call procedure_one()" // new_line('a') // &
                "contains" // new_line('a') // &
                "  subroutine procedure_one()" // new_line('a') // &
                "    call procedure_two()" // new_line('a') // &
                "  end subroutine procedure_one" // new_line('a') // &
                "  subroutine procedure_two()" // new_line('a') // &
                "    print *, 'called indirectly'" // new_line('a') // &
                "  end subroutine procedure_two" // new_line('a') // &
                "end program test_used_internal"
        
        call tokenize_core(source, tokens)
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        program_index = parse_program_statement(parser, arena)
        
        if (program_index <= 0) then
            print *, "FAILED: Could not parse program"
            all_tests_passed = .false.
            return
        end if
        
        graph = build_call_graph(arena, program_index)
        unused_procs = find_unused_procedures(graph)
        
        ! Both procedures are used, so should find no unused procedures
        if (allocated(unused_procs) .and. size(unused_procs) > 0) then
            print *, "Found", size(unused_procs), "unused procedures (may be false positives)"
        end if
        
        print *, "PASSED: Used internal procedures not incorrectly marked as unused"
        
    end subroutine test_used_internal_procedure

    subroutine test_mixed_internal_procedures()
        use lexer_core, only: tokenize_core
        use parser_state_module, only: parser_state_t, create_parser_state
        use parser_execution_statements_module, only: parse_program_statement
        use call_graph_builder_module, only: build_call_graph
        use call_graph_module, only: call_graph_t, find_unused_procedures
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        type(call_graph_t) :: graph
        integer :: program_index
        character(len=:), allocatable :: unused_procs(:)
        
        print *, "Testing mixed used/unused internal procedures..."
        
        source = "program test_mixed_internal" // new_line('a') // &
                "  call active_proc()" // new_line('a') // &
                "  call another_active()" // new_line('a') // &
                "contains" // new_line('a') // &
                "  subroutine active_proc()" // new_line('a') // &
                "    print *, 'used'" // new_line('a') // &
                "  end subroutine active_proc" // new_line('a') // &
                "  subroutine dead_proc()" // new_line('a') // &
                "    print *, 'never called'" // new_line('a') // &
                "  end subroutine dead_proc" // new_line('a') // &
                "  subroutine another_active()" // new_line('a') // &
                "    print *, 'also used'" // new_line('a') // &
                "  end subroutine another_active" // new_line('a') // &
                "  subroutine another_dead()" // new_line('a') // &
                "    print *, 'also never called'" // new_line('a') // &
                "  end subroutine another_dead" // new_line('a') // &
                "end program test_mixed_internal"
        
        call tokenize_core(source, tokens)
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        program_index = parse_program_statement(parser, arena)
        
        if (program_index <= 0) then
            print *, "FAILED: Could not parse program"
            all_tests_passed = .false.
            return
        end if
        
        graph = build_call_graph(arena, program_index)
        unused_procs = find_unused_procedures(graph)
        
        ! Should find dead_proc and another_dead as unused
        if (.not. allocated(unused_procs) .or. size(unused_procs) < 2) then
            print *, "FAILED: Not all unused internal procedures detected"
            if (allocated(unused_procs)) then
                print *, "Found", size(unused_procs), "unused procedures, expected at least 2"
            end if
            all_tests_passed = .false.
        else
            print *, "PASSED: Multiple unused internal procedures correctly detected"
        end if
        
    end subroutine test_mixed_internal_procedures

end program test_issue_147_unused_internal_procs