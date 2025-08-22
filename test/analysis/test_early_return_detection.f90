program test_early_return_detection
    use fortfront
    use control_flow_graph_module
    use iso_fortran_env, only: error_unit
    implicit none

    logical :: all_tests_passed = .true.

    print *, "Testing early return pattern detection..."

    call test_basic_early_return()
    call test_conditional_early_return()
    call test_multiple_early_returns()

    if (all_tests_passed) then
        print *, "All early return detection tests PASSED!"
    else
        print *, "Some early return detection tests FAILED!"
        stop 1
    end if

contains

    subroutine test_basic_early_return()
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        type(control_flow_graph_t) :: cfg
        integer, allocatable :: unreachable_blocks(:)
        
        print *, "Testing basic early return pattern..."
        
        ! Test case from issue #161
        source = "function validate(x) result(valid)" // new_line('a') // &
                "implicit none" // new_line('a') // &
                "integer, intent(in) :: x" // new_line('a') // &
                "logical :: valid" // new_line('a') // &
                "if (x < 0) then" // new_line('a') // &
                "    valid = .false." // new_line('a') // &
                "    return" // new_line('a') // &
                "end if" // new_line('a') // &
                "valid = .true." // new_line('a') // &  ! Conditionally unreachable
                "end function validate"
        
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
        
        ! Build CFG
        cfg = build_cfg_from_arena(arena, root_index)
        
        ! Find conditionally unreachable code
        unreachable_blocks = find_conditionally_unreachable_code(cfg)
        
        ! Should detect that "valid = .true." is unreachable on the early return path
        if (size(unreachable_blocks) == 0) then
            print *, "FAILED: Early return pattern not detected as " // &
                     "conditionally unreachable"
            all_tests_passed = .false.
            return
        end if
        
        print *, "PASSED: Found", size(unreachable_blocks), &
                 "conditionally unreachable blocks"
        
    end subroutine test_basic_early_return

    subroutine test_conditional_early_return()
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        type(control_flow_graph_t) :: cfg
        integer, allocatable :: unreachable_blocks(:)
        
        print *, "Testing conditional early return pattern..."
        
        source = "subroutine process(status)" // new_line('a') // &
                "implicit none" // new_line('a') // &
                "integer, intent(inout) :: status" // new_line('a') // &
                "if (status < 0) then" // new_line('a') // &
                "    print *, 'Error occurred'" // new_line('a') // &
                "    return" // new_line('a') // &
                "end if" // new_line('a') // &
                "if (status == 0) then" // new_line('a') // &
                "    print *, 'Nothing to do'" // new_line('a') // &
                "    return" // new_line('a') // &
                "end if" // new_line('a') // &
                "print *, 'Processing...'" // new_line('a') // &  ! Unreachable
                "status = 1" // new_line('a') // &                ! Unreachable
                "end subroutine process"
        
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
        
        ! Build CFG
        cfg = build_cfg_from_arena(arena, root_index)
        
        ! Find conditionally unreachable code
        unreachable_blocks = find_conditionally_unreachable_code(cfg)
        
        ! Should detect processing statements as conditionally unreachable
        if (size(unreachable_blocks) == 0) then
            print *, "FAILED: Multiple early return patterns not detected"
            all_tests_passed = .false.
            return
        end if
        
        print *, "PASSED: Found", size(unreachable_blocks), &
                 "conditionally unreachable blocks"
        
    end subroutine test_conditional_early_return

    subroutine test_multiple_early_returns()
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        type(control_flow_graph_t) :: cfg
        integer, allocatable :: unreachable_blocks(:)
        
        print *, "Testing multiple early return patterns..."
        
        source = "function analyze(data) result(result_code)" // new_line('a') // &
                "implicit none" // new_line('a') // &
                "integer, intent(in) :: data" // new_line('a') // &
                "integer :: result_code" // new_line('a') // &
                "if (data == 0) then" // new_line('a') // &
                "    result_code = -1" // new_line('a') // &
                "    return" // new_line('a') // &
                "end if" // new_line('a') // &
                "if (data < 0) then" // new_line('a') // &
                "    result_code = -2" // new_line('a') // &
                "    return" // new_line('a') // &
                "end if" // new_line('a') // &
                "if (data > 100) then" // new_line('a') // &
                "    result_code = -3" // new_line('a') // &
                "    return" // new_line('a') // &
                "end if" // new_line('a') // &
                "result_code = data * 2" // new_line('a') // &  ! Unreachable
                "end function analyze"
        
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
        
        ! Build CFG
        cfg = build_cfg_from_arena(arena, root_index)
        
        ! Find conditionally unreachable code
        unreachable_blocks = find_conditionally_unreachable_code(cfg)
        
        ! Should detect final statement as conditionally unreachable
        if (size(unreachable_blocks) == 0) then
            print *, "FAILED: Complex early return pattern not detected"
            all_tests_passed = .false.
            return
        end if
        
        print *, "PASSED: Found", size(unreachable_blocks), &
                 "conditionally unreachable blocks"
        
    end subroutine test_multiple_early_returns

end program test_early_return_detection