program test_cfg_new_features
    use fortfront
    use control_flow_graph_module, only: EDGE_SUCCESS_PATH, EDGE_FAILURE_PATH, EDGE_ERROR_HANDLING
    use iso_fortran_env, only: error_unit
    implicit none

    logical :: all_tests_passed
    all_tests_passed = .true.

    call test_new_edge_types()
    call test_enhanced_cfg_analysis()

    if (all_tests_passed) then
        print *, "All CFG new features tests PASSED!"
    else
        error stop "Some CFG new features tests FAILED!"
    end if

contains

    subroutine test_new_edge_types()
        type(control_flow_graph_t) :: cfg
        integer :: block1, block2, block3
        integer :: i
        logical :: found_success_edge, found_failure_edge, found_error_edge
        
        print *, "Testing new edge types..."
        
        ! Create a simple CFG with new edge types
        cfg = create_control_flow_graph("test_proc")
        
        ! Add blocks
        block1 = add_basic_block(cfg, "entry", is_entry=.true.)
        block2 = add_basic_block(cfg, "success")
        block3 = add_basic_block(cfg, "failure")
        
        ! Add edges with new types
        call add_cfg_edge(cfg, block1, block2, EDGE_SUCCESS_PATH, "allocation-success")
        call add_cfg_edge(cfg, block1, block3, EDGE_FAILURE_PATH, "allocation-failure")
        call add_cfg_edge(cfg, block3, block2, EDGE_ERROR_HANDLING, "error-recovery")
        
        ! Verify edge types are stored correctly
        found_success_edge = .false.
        found_failure_edge = .false.
        found_error_edge = .false.
        
        do i = 1, cfg%edge_count
            select case (cfg%edges(i)%edge_type)
            case (EDGE_SUCCESS_PATH)
                found_success_edge = .true.
            case (EDGE_FAILURE_PATH)
                found_failure_edge = .true.
            case (EDGE_ERROR_HANDLING)
                found_error_edge = .true.
            end select
        end do
        
        if (.not. found_success_edge .or. .not. found_failure_edge .or. .not. found_error_edge) then
            print *, "FAILED: New edge types not properly stored"
            all_tests_passed = .false.
            return
        end if
        
        ! Test reachability analysis
        call find_reachable_blocks(cfg)
        
        if (.not. is_block_reachable(cfg, block2) .or. .not. is_block_reachable(cfg, block3)) then
            print *, "FAILED: Reachability analysis failed with new edge types"
            all_tests_passed = .false.
            return
        end if
        
        print *, "PASSED: New edge types test"
        
    end subroutine test_new_edge_types

    subroutine test_enhanced_cfg_analysis()
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        type(control_flow_graph_t) :: cfg
        integer, allocatable :: all_blocks(:)
        
        print *, "Testing enhanced CFG analysis..."
        
        ! Simple program that should create a basic CFG
        source = "program test" // new_line('a') // &
                "implicit none" // new_line('a') // &
                "integer :: x" // new_line('a') // &
                "x = 1" // new_line('a') // &
                "if (x > 0) then" // new_line('a') // &
                "    print *, 'positive'" // new_line('a') // &
                "end if" // new_line('a') // &
                "end program test"
        
        ! Lex and parse
        call lex_source(source, tokens, error_msg)
        if (error_msg /= "") then
            print *, "FAILED: Lexing error:", error_msg
            all_tests_passed = .false.
            return
        end if
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        if (root_index <= 0) then
            print *, "FAILED: Parse failed:", error_msg
            all_tests_passed = .false.
            return
        end if
        
        ! Build CFG with enhanced capabilities
        cfg = build_cfg_from_arena(arena, root_index)
        
        ! Verify the CFG was built correctly
        all_blocks = get_cfg_all_blocks(cfg)
        if (size(all_blocks) < 3) then
            print *, "FAILED: Expected at least 3 blocks, got", size(all_blocks)
            all_tests_passed = .false.
            return
        end if
        
        ! Test CFG analysis functions
        call find_reachable_blocks(cfg)
        
        ! All blocks should be reachable in this simple case
        block
            integer :: i
            do i = 1, size(all_blocks)
                if (.not. is_block_reachable(cfg, all_blocks(i))) then
                    print *, "FAILED: Block", all_blocks(i), "is not reachable"
                    all_tests_passed = .false.
                    return
                end if
            end do
        end block
        
        print *, "PASSED: Enhanced CFG analysis test"
        
    end subroutine test_enhanced_cfg_analysis

end program test_cfg_new_features