program test_cst_builder
    use cst_builder
    use cst_nodes
    use cst_arena
    use uid_generator
    use lexer_core, only: trivia_token_t, TK_COMMENT, TK_WHITESPACE, TK_NEWLINE
    implicit none

    integer :: total_tests, passed_tests

    total_tests = 0
    passed_tests = 0

    print *, "=== CST Builder Tests ==="
    print *, ""

    ! Initialize UID generator for tests
    call init_uid_generator()

    ! Test 1: Create CST builder with default settings
    call test_start("Create CST builder with default settings")
    block
        type(cst_builder_t) :: builder
        
        builder = create_cst_builder()
        
        if (builder%collect_trivia .and. &
            builder%parallel_mode .and. &
            .not. builder%debug_mode .and. &
            builder%context%initialized) then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: collect_trivia=.true., parallel_mode=.true., debug_mode=.false."
            print *, "  Got: collect_trivia=", builder%collect_trivia, &
                     ", parallel_mode=", builder%parallel_mode, &
                     ", debug_mode=", builder%debug_mode
        end if
    end block

    ! Test 2: Create CST builder with custom settings
    call test_start("Create CST builder with custom settings")
    block
        type(cst_builder_t) :: builder
        
        builder = create_cst_builder(initial_capacity=500, collect_trivia=.false.)
        
        if (.not. builder%collect_trivia .and. &
            builder%parallel_mode .and. &
            builder%context%initialized .and. &
            builder%context%arena%capacity >= 500) then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: collect_trivia=.false., capacity>=500"
            print *, "  Got: collect_trivia=", builder%collect_trivia, &
                     ", capacity=", builder%context%arena%capacity
        end if
    end block

    ! Test 3: Build basic CST node
    call test_start("Build basic CST node")
    block
        type(cst_builder_t) :: builder
        type(builder_result_t) :: result
        type(cst_node_t) :: retrieved_node
        
        builder = create_cst_builder()
        result = build_cst_node(builder, CST_IDENTIFIER, 10, 20, "my_var")
        
        if (result%success .and. result%handle%index > 0) then
            retrieved_node = builder%context%arena%get(result%handle)
            if (retrieved_node%kind == CST_IDENTIFIER .and. &
                retrieved_node%start_pos == 10 .and. &
                retrieved_node%end_pos == 20 .and. &
                retrieved_node%text == "my_var" .and. &
                retrieved_node%uid > 0) then
                call test_pass()
            else
                call test_fail()
                print *, "  Node validation failed"
                print *, "  Expected: kind=", CST_IDENTIFIER, ", start=10, end=20, text='my_var'"
                print *, "  Got: kind=", retrieved_node%kind, ", start=", retrieved_node%start_pos, &
                         ", end=", retrieved_node%end_pos, ", text='", retrieved_node%text, "'"
            end if
        else
            call test_fail()
            print *, "  Build failed or invalid handle"
            print *, "  success=", result%success, ", handle index=", result%handle%index
            if (.not. result%success .and. allocated(result%error_message)) then
                print *, "  Error: ", result%error_message
            end if
        end if
    end block

    ! Test 4: Build CST node with invalid parameters
    call test_start("Build CST node with invalid parameters")
    block
        type(cst_builder_t) :: builder
        type(builder_result_t) :: result
        
        builder = create_cst_builder()
        
        ! Invalid position range (end < start)
        result = build_cst_node(builder, CST_IDENTIFIER, 20, 10)
        
        if (.not. result%success .and. allocated(result%error_message)) then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: build failure with error message"
            print *, "  Got: success=", result%success
        end if
    end block

    ! Test 5: Build CST node with AST link
    call test_start("Build CST node with AST link")
    block
        type(cst_builder_t) :: builder
        type(builder_result_t) :: result
        type(cst_node_t) :: retrieved_node
        
        builder = create_cst_builder()
        result = build_cst_node(builder, CST_FUNCTION, 1, 100, "main", ast_link=42)
        
        if (result%success) then
            retrieved_node = builder%context%arena%get(result%handle)
            if (retrieved_node%ast_link == 42) then
                call test_pass()
            else
                call test_fail()
                print *, "  Expected: ast_link=42, Got:", retrieved_node%ast_link
            end if
        else
            call test_fail()
            print *, "  Build failed"
        end if
    end block

    ! Test 6: Parallel CST construction
    call test_start("Parallel CST construction")
    block
        type(cst_builder_t) :: builder
        type(cst_handle_t) :: cst_handle
        type(cst_node_t) :: node
        
        builder = create_cst_builder()
        
        cst_handle = build_parallel(builder, ast_node_index=123, &
                                   cst_kind=CST_SUBROUTINE, &
                                   start_pos=5, end_pos=50, &
                                   text="test_sub")
        
        if (cst_handle%index > 0) then
            node = builder%context%arena%get(cst_handle)
            if (node%kind == CST_SUBROUTINE .and. &
                node%ast_link == 123 .and. &
                node%text == "test_sub") then
                call test_pass()
            else
                call test_fail()
                print *, "  Node validation failed"
                print *, "  Expected: kind=", CST_SUBROUTINE, ", ast_link=123, text='test_sub'"
                print *, "  Got: kind=", node%kind, ", ast_link=", node%ast_link, &
                         ", text='", node%text, "'"
            end if
        else
            call test_fail()
            print *, "  Invalid handle returned"
        end if
    end block

    ! Test 7: Builder options configuration
    call test_start("Builder options configuration")
    block
        type(cst_builder_t) :: builder
        
        builder = create_cst_builder()
        
        call builder%set_options(collect_trivia=.false., &
                                 parallel_mode=.false., &
                                 debug_mode=.true.)
        
        if (.not. builder%collect_trivia .and. &
            .not. builder%parallel_mode .and. &
            builder%debug_mode) then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: collect_trivia=.false., parallel_mode=.false., debug_mode=.true."
            print *, "  Got: collect_trivia=", builder%collect_trivia, &
                     ", parallel_mode=", builder%parallel_mode, &
                     ", debug_mode=", builder%debug_mode
        end if
    end block

    ! Test 8: Builder root node tracking
    call test_start("Builder root node tracking")
    block
        type(cst_builder_t) :: builder
        type(cst_handle_t) :: first_handle, second_handle, root_handle
        
        builder = create_cst_builder()
        
        first_handle = builder%create_node(CST_PROGRAM, 1, 100, "main_program")
        second_handle = builder%create_node(CST_SUBROUTINE, 20, 50, "sub1")
        
        root_handle = builder%get_root()
        
        ! Root should be the first node created
        if (root_handle%index == first_handle%index .and. &
            root_handle%generation == first_handle%generation) then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected root to be first node"
            print *, "  First: index=", first_handle%index, ", gen=", first_handle%generation
            print *, "  Root: index=", root_handle%index, ", gen=", root_handle%generation
        end if
    end block

    ! Test 9: Builder context parent stack operations
    call test_start("Builder context parent stack operations")
    block
        type(builder_context_t) :: context
        type(cst_handle_t) :: parent1, parent2, current, popped
        
        context = init_builder_context(100)
        
        parent1%index = 10
        parent1%generation = 1
        parent2%index = 20
        parent2%generation = 1
        
        ! Push parents onto stack
        call context%push_parent(parent1)
        call context%push_parent(parent2)
        
        ! Check current parent
        current = context%current_parent()
        if (current%index /= parent2%index) then
            call test_fail()
            print *, "  Expected current parent index=", parent2%index, ", Got:", current%index
            return
        end if
        
        ! Pop parent
        popped = context%pop_parent()
        if (popped%index /= parent2%index) then
            call test_fail()
            print *, "  Expected popped parent index=", parent2%index, ", Got:", popped%index
            return
        end if
        
        ! Check new current parent
        current = context%current_parent()
        if (current%index /= parent1%index) then
            call test_fail()
            print *, "  Expected current parent index=", parent1%index, ", Got:", current%index
            return
        end if
        
        call test_pass()
    end block

    ! Test 10: Builder trivia token handling
    call test_start("Builder trivia token handling")
    block
        type(cst_builder_t) :: builder
        type(cst_handle_t) :: node_handle
        type(trivia_token_t) :: leading_trivia(2), trailing_trivia(1)
        logical :: success
        
        builder = create_cst_builder(collect_trivia=.true.)
        
        ! Create test trivia
        leading_trivia(1)%kind = TK_COMMENT
        leading_trivia(1)%text = "! Leading comment"
        leading_trivia(1)%line = 1
        leading_trivia(1)%column = 1
        
        leading_trivia(2)%kind = TK_WHITESPACE
        leading_trivia(2)%text = "  "
        leading_trivia(2)%line = 2
        leading_trivia(2)%column = 1
        
        trailing_trivia(1)%kind = TK_NEWLINE
        trailing_trivia(1)%text = new_line('A')
        trailing_trivia(1)%line = 2
        trailing_trivia(1)%column = 10
        
        ! Create node
        node_handle = builder%create_node(CST_DECLARATION, 10, 30, "integer :: x")
        
        ! Attach trivia
        success = builder%attach_trivia(node_handle, leading_trivia, trailing_trivia)
        
        if (success .and. node_handle%index > 0) then
            call test_pass()
        else
            call test_fail()
            print *, "  Trivia attachment failed"
            print *, "  success=", success, ", handle=", node_handle%index
        end if
    end block

    ! Test 11: Builder clear functionality
    call test_start("Builder clear functionality")
    block
        type(cst_builder_t) :: builder
        type(cst_handle_t) :: handle_before, handle_after
        
        builder = create_cst_builder()
        
        ! Create a node
        handle_before = builder%create_node(CST_PROGRAM, 1, 50)
        
        ! Clear builder
        call builder%clear()
        
        ! Create another node after clear
        handle_after = builder%create_node(CST_FUNCTION, 1, 30)
        
        ! After clear, arena should be empty and first node should get index 1
        if (handle_before%index > 0 .and. handle_after%index == 1) then
            call test_pass()
        else
            call test_fail()
            print *, "  Clear operation failed"
            print *, "  Before: index=", handle_before%index
            print *, "  After: index=", handle_after%index
        end if
    end block

    ! Test 12: Context pending trivia management
    call test_start("Context pending trivia management")
    block
        type(builder_context_t) :: context
        type(trivia_token_t) :: trivia1, trivia2
        
        context = init_builder_context(100)
        
        ! Create test trivia
        trivia1%kind = TK_COMMENT
        trivia1%text = "! Comment 1"
        
        trivia2%kind = TK_WHITESPACE
        trivia2%text = "    "
        
        ! Add pending trivia
        call context%add_pending_trivia(trivia1, is_leading=.true.)
        call context%add_pending_trivia(trivia2, is_leading=.false.)
        
        if (context%trivia_count == 2 .and. &
            allocated(context%pending_leading) .and. &
            allocated(context%pending_trailing)) then
            call test_pass()
        else
            call test_fail()
            print *, "  Trivia management failed"
            print *, "  Count:", context%trivia_count
            print *, "  Leading allocated:", allocated(context%pending_leading)
            print *, "  Trailing allocated:", allocated(context%pending_trailing)
        end if
    end block

    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", passed_tests, "/", total_tests, " tests"

    if (passed_tests == total_tests) then
        print *, "All CST builder tests passed!"
        stop 0
    else
        print *, "Some CST builder tests failed!"
        stop 1
    end if

contains

    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        total_tests = total_tests + 1
        write(*, '(A,A)', advance='no') "Testing: ", test_name
    end subroutine test_start

    subroutine test_pass()
        print *, " ... PASSED"
        passed_tests = passed_tests + 1
    end subroutine test_pass

    subroutine test_fail()
        print *, " ... FAILED"
    end subroutine test_fail

end program test_cst_builder