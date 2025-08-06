program test_goto_error_stop_nodes
    use fortfront
    use iso_fortran_env, only: error_unit
    implicit none

    logical :: all_tests_passed
    all_tests_passed = .true.

    call test_goto_statement_parsing()
    call test_error_stop_statement_parsing()
    call test_goto_with_label_parsing()
    call test_error_stop_with_code_parsing()

    if (all_tests_passed) then
        print *, "All goto and error stop node tests PASSED!"
    else
        error stop "Some goto and error stop node tests FAILED!"
    end if

contains

    subroutine test_goto_statement_parsing()
        use lexer_core, only: tokenize_core
        use parser_dispatcher_module, only: parse_statement_dispatcher
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        logical :: goto_found
        integer :: i
        
        print *, "Testing goto statement parsing..."
        
        ! Test simple goto statement directly
        source = "go to 10"
        
        call tokenize_core(source, tokens)
        arena = create_ast_arena()
        root_index = parse_statement_dispatcher(tokens, arena)
        
        if (root_index <= 0) then
            print *, "FAILED: Parse failed"
            all_tests_passed = .false.
            return
        end if
        
        ! Look for goto_node in the arena
        goto_found = .false.
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                select type (node => arena%entries(i)%node)
                type is (goto_node)
                    goto_found = .true.
                    print *, "Found goto_node with label:", node%label
                    if (.not. allocated(node%label)) then
                        print *, "FAILED: goto_node has no label"
                        all_tests_passed = .false.
                        return
                    end if
                    if (node%label /= "10") then
                        print *, "FAILED: Expected label '10', got '", node%label, "'"
                        all_tests_passed = .false.
                        return
                    end if
                    exit
                end select
            end if
        end do
        
        if (goto_found) then
            print *, "PASSED: goto statement test"
        else
            print *, "FAILED: No goto_node found in AST"
            print *, "Available node types:"
            do i = 1, arena%size
                if (allocated(arena%entries(i)%node)) then
                    print *, "  - ", arena%entries(i)%node_type
                end if
            end do
            all_tests_passed = .false.
        end if
        
    end subroutine test_goto_statement_parsing

    subroutine test_error_stop_statement_parsing()
        use lexer_core, only: tokenize_core
        use parser_dispatcher_module, only: parse_statement_dispatcher
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        logical :: error_stop_found
        integer :: i
        
        print *, "Testing error stop statement parsing..."
        
        ! Test simple error stop statement directly
        source = "error stop 'fatal error'"
        
        call tokenize_core(source, tokens)
        arena = create_ast_arena()
        root_index = parse_statement_dispatcher(tokens, arena)
        
        if (root_index <= 0) then
            print *, "FAILED: Parse failed"
            all_tests_passed = .false.
            return
        end if
        
        ! Look for error_stop_node in the arena
        error_stop_found = .false.
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                select type (node => arena%entries(i)%node)
                type is (error_stop_node)
                    error_stop_found = .true.
                    print *, "Found error_stop_node with message:", node%error_message
                    if (.not. allocated(node%error_message)) then
                        print *, "FAILED: error_stop_node has no message"
                        all_tests_passed = .false.
                        return
                    end if
                    if (node%error_message /= "'fatal error'") then
                        print *, "FAILED: Expected message 'fatal error', got '", &
                                node%error_message, "'"
                        all_tests_passed = .false.
                        return
                    end if
                    exit
                end select
            end if
        end do
        
        if (error_stop_found) then
            print *, "PASSED: error stop statement test"
        else
            print *, "FAILED: No error_stop_node found in AST"
            print *, "Available node types:"
            do i = 1, arena%size
                if (allocated(arena%entries(i)%node)) then
                    print *, "  - ", arena%entries(i)%node_type
                end if
            end do
            all_tests_passed = .false.
        end if
        
    end subroutine test_error_stop_statement_parsing

    subroutine test_goto_with_label_parsing()
        use lexer_core, only: tokenize_core
        use parser_dispatcher_module, only: parse_statement_dispatcher
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        logical :: goto_found
        integer :: i
        
        print *, "Testing goto with numeric label..."
        
        source = "go to 999"
        
        call tokenize_core(source, tokens)
        arena = create_ast_arena()
        root_index = parse_statement_dispatcher(tokens, arena)
        
        if (root_index <= 0) then
            print *, "FAILED: Parse failed"
            all_tests_passed = .false.
            return
        end if
        
        goto_found = .false.
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                select type (node => arena%entries(i)%node)
                type is (goto_node)
                    goto_found = .true.
                    if (allocated(node%label) .and. node%label == "999") then
                        print *, "PASSED: goto with numeric label test"
                    else
                        print *, "FAILED: Incorrect label in goto_node"
                        all_tests_passed = .false.
                    end if
                    exit
                end select
            end if
        end do
        
        if (.not. goto_found) then
            print *, "FAILED: No goto_node found for numeric label"
            all_tests_passed = .false.
        end if
        
    end subroutine test_goto_with_label_parsing

    subroutine test_error_stop_with_code_parsing()
        use lexer_core, only: tokenize_core
        use parser_dispatcher_module, only: parse_statement_dispatcher
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        logical :: error_stop_found
        integer :: i
        
        print *, "Testing error stop with error code..."
        
        source = "error stop 42"
        
        call tokenize_core(source, tokens)
        arena = create_ast_arena()
        root_index = parse_statement_dispatcher(tokens, arena)
        
        if (root_index <= 0) then
            print *, "FAILED: Parse failed"
            all_tests_passed = .false.
            return
        end if
        
        error_stop_found = .false.
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                select type (node => arena%entries(i)%node)
                type is (error_stop_node)
                    error_stop_found = .true.
                    if (node%error_code_index > 0) then
                        print *, "PASSED: error stop with code test"
                    else
                        print *, "FAILED: error_stop_node has no error code index"
                        all_tests_passed = .false.
                    end if
                    exit
                end select
            end if
        end do
        
        if (.not. error_stop_found) then
            print *, "FAILED: No error_stop_node found for error code"
            all_tests_passed = .false.
        end if
        
    end subroutine test_error_stop_with_code_parsing

end program test_goto_error_stop_nodes