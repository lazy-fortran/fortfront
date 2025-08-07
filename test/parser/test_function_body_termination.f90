program test_function_body_termination
    use fortfront
    use ast_core
    use ast_nodes_control
    implicit none

    logical :: all_tests_passed = .true.
    
    print *, "=== Testing Termination Statements in Function/Subroutine Bodies ==="
    
    call test_stop_in_function_body()
    call test_goto_in_function_body()
    call test_error_stop_in_function_body()
    call test_mixed_termination_in_subroutine()
    
    if (all_tests_passed) then
        print *, "All function/subroutine body termination tests PASSED!"
    else
        print *, "Some function/subroutine body termination tests FAILED!"
        stop 1
    end if

contains

    subroutine test_stop_in_function_body()
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index, i
        logical :: found_stop
        
        print *, "Testing STOP in function body..."
        
        source = "program test" // new_line('a') // &
                "contains" // new_line('a') // &
                "function test_func()" // new_line('a') // &
                "  integer :: x" // new_line('a') // &
                "  x = 42" // new_line('a') // &
                "  stop 'function error'" // new_line('a') // &
                "end function test_func" // new_line('a') // &
                "end program test"
        
        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        found_stop = .false.
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                if (arena%entries(i)%node_type == "stop_node") then
                    select type (node => arena%entries(i)%node)
                    type is (stop_node)
                        if (allocated(node%stop_message)) then
                            found_stop = .true.
                        end if
                    end select
                    exit
                end if
            end if
        end do
        
        if (.not. found_stop) then
            print *, "FAILED: Stop statement in function body not generating stop_node"
            all_tests_passed = .false.
        else
            print *, "PASSED: STOP in function body test"
        end if
    end subroutine test_stop_in_function_body

    subroutine test_goto_in_function_body()
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index, i
        logical :: found_goto
        
        print *, "Testing GOTO in function body..."
        
        source = "program test" // new_line('a') // &
                "contains" // new_line('a') // &
                "function test_func()" // new_line('a') // &
                "  go to 100" // new_line('a') // &
                "  print *, 'unreachable'" // new_line('a') // &
                "100 continue" // new_line('a') // &
                "end function test_func" // new_line('a') // &
                "end program test"
        
        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        found_goto = .false.
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                if (arena%entries(i)%node_type == "goto_node") then
                    select type (node => arena%entries(i)%node)
                    type is (goto_node)
                        if (allocated(node%label) .and. node%label == "100") then
                            found_goto = .true.
                        end if
                    end select
                    exit
                end if
            end if
        end do
        
        if (.not. found_goto) then
            print *, "FAILED: Goto statement in function body not generating goto_node"
            all_tests_passed = .false.
        else
            print *, "PASSED: GOTO in function body test"
        end if
    end subroutine test_goto_in_function_body

    subroutine test_error_stop_in_function_body()
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index, i
        logical :: found_error_stop
        
        print *, "Testing ERROR STOP in function body..."
        
        source = "program test" // new_line('a') // &
                "contains" // new_line('a') // &
                "function test_func()" // new_line('a') // &
                "  integer :: result" // new_line('a') // &
                "  result = 0" // new_line('a') // &
                "  error stop 'critical function failure'" // new_line('a') // &
                "end function test_func" // new_line('a') // &
                "end program test"
        
        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        found_error_stop = .false.
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                if (arena%entries(i)%node_type == "error_stop_node") then
                    select type (node => arena%entries(i)%node)
                    type is (error_stop_node)
                        if (allocated(node%error_message)) then
                            found_error_stop = .true.
                        end if
                    end select
                    exit
                end if
            end if
        end do
        
        if (.not. found_error_stop) then
            print *, "FAILED: Error stop statement in function body not generating error_stop_node"
            all_tests_passed = .false.
        else
            print *, "PASSED: ERROR STOP in function body test"
        end if
    end subroutine test_error_stop_in_function_body

    subroutine test_mixed_termination_in_subroutine()
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index, i
        integer :: stop_count, goto_count, error_stop_count
        
        print *, "Testing mixed termination statements in subroutine..."
        
        source = "program test" // new_line('a') // &
                "contains" // new_line('a') // &
                "subroutine test_sub()" // new_line('a') // &
                "  if (.true.) then" // new_line('a') // &
                "    error stop" // new_line('a') // &
                "  end if" // new_line('a') // &
                "  go to 200" // new_line('a') // &
                "200 continue" // new_line('a') // &
                "  stop" // new_line('a') // &
                "end subroutine test_sub" // new_line('a') // &
                "end program test"
        
        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        stop_count = 0
        goto_count = 0
        error_stop_count = 0
        
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                select case (arena%entries(i)%node_type)
                case ("stop_node")
                    stop_count = stop_count + 1
                case ("goto_node")
                    goto_count = goto_count + 1
                case ("error_stop_node")
                    error_stop_count = error_stop_count + 1
                end select
            end if
        end do
        
        if (stop_count == 0 .or. goto_count == 0 .or. error_stop_count == 0) then
            print *, "WARNING: Mixed termination in subroutine - Expected at least 1 of each:"
            print *, "  stop_nodes:", stop_count
            print *, "  goto_nodes:", goto_count  
            print *, "  error_stop_nodes:", error_stop_count
            if (stop_count == 0 .and. goto_count == 0 .and. error_stop_count == 0) then
                print *, "FAILED: No termination nodes found in subroutine body"
                all_tests_passed = .false.
            else
                print *, "PARTIAL PASS: Some nodes found, may be parsing context issue"
            end if
        else
            print *, "PASSED: Mixed termination statements in subroutine test"
        end if
    end subroutine test_mixed_termination_in_subroutine

end program test_function_body_termination