program test_frontend_parser_if_statement
    use lexer_core
    use ast_core, only: ast_arena_t, create_ast_arena, if_node
    use ast_factory
    use parser_control_flow_module, only: parse_if
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_dispatcher_module, only: parse_statement_dispatcher
    implicit none

    logical :: all_passed

    all_passed = .true.

    ! Run tests for if statements
    if (.not. test_simple_if_then()) all_passed = .false.
    if (.not. test_if_condition_parsing()) all_passed = .false.
    if (.not. test_full_if_block()) all_passed = .false.

    ! Report results
    if (all_passed) then
        print '(a)', "All if statement parser tests passed"
        stop 0
    else
        print '(a)', "Some if statement parser tests failed"
        stop 1
    end if

contains

    logical function test_simple_if_then()
        ! Test parsing of simple if/then statement
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        integer :: if_index

        test_simple_if_then = .true.

        print '(a)', "Testing simple if/then parsing..."

        ! Create tokens for "if (x > 0) then"
        allocate (tokens(8))
        tokens(1) = token_t(TK_KEYWORD, "if", 1, 1)
        tokens(2) = token_t(TK_OPERATOR, "(", 1, 4)
        tokens(3) = token_t(TK_IDENTIFIER, "x", 1, 5)
        tokens(4) = token_t(TK_OPERATOR, ">", 1, 7)
        tokens(5) = token_t(TK_NUMBER, "0", 1, 9)
        tokens(6) = token_t(TK_OPERATOR, ")", 1, 10)
        tokens(7) = token_t(TK_KEYWORD, "then", 1, 12)
        tokens(8) = token_t(TK_EOF, "", 1, 16)

        ! Create parser and arena
        arena = create_ast_arena()
        parser = create_parser_state(tokens)

        ! Parse if statement
        if_index = parse_if(parser, arena)

        if (if_index > 0) then
            print '(a)', "PASS: If statement parsed successfully"

            ! Check the node type
            if (allocated(arena%entries(if_index)%node_type)) then
                if (arena%entries(if_index)%node_type == "if_statement") then
                    print '(a)', "PASS: Correct node type 'if_statement'"
                else
       print '(a,a)', "FAIL: Wrong node type: ", trim(arena%entries(if_index)%node_type)
                    test_simple_if_then = .false.
                end if
            else
                print '(a)', "FAIL: Node type not allocated"
                test_simple_if_then = .false.
            end if
        else
            print '(a)', "FAIL: If statement not parsed"
            test_simple_if_then = .false.
        end if

    end function test_simple_if_then

    logical function test_if_condition_parsing()
        ! Test that if condition is parsed correctly
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        integer :: if_index

        test_if_condition_parsing = .true.

        print '(a)', "Testing if condition parsing..."

        ! Create tokens for "if (a == b) then"
        allocate (tokens(8))
        tokens(1) = token_t(TK_KEYWORD, "if", 1, 1)
        tokens(2) = token_t(TK_OPERATOR, "(", 1, 4)
        tokens(3) = token_t(TK_IDENTIFIER, "a", 1, 5)
        tokens(4) = token_t(TK_OPERATOR, "==", 1, 7)
        tokens(5) = token_t(TK_IDENTIFIER, "b", 1, 10)
        tokens(6) = token_t(TK_OPERATOR, ")", 1, 12)
        tokens(7) = token_t(TK_KEYWORD, "then", 1, 14)
        tokens(8) = token_t(TK_EOF, "", 1, 18)

        ! Create parser and arena
        arena = create_ast_arena()
        parser = create_parser_state(tokens)

        ! Parse if statement
        if_index = parse_if(parser, arena)

        if (if_index > 0) then
            print '(a)', "PASS: If statement with condition parsed"

            ! The if node should have been created
            if (allocated(arena%entries(if_index)%node)) then
                select type (node => arena%entries(if_index)%node)
                type is (if_node)
                    if (node%condition_index > 0) then
                        print '(a)', "PASS: Condition index is set"
                    else
                        print '(a)', "FAIL: No condition index"
                        test_if_condition_parsing = .false.
                    end if
                class default
                    print '(a)', "FAIL: Not an if_node"
                    test_if_condition_parsing = .false.
                end select
            else
                print '(a)', "FAIL: Node not allocated"
                test_if_condition_parsing = .false.
            end if
        else
            print '(a)', "FAIL: If statement with condition not parsed"
            test_if_condition_parsing = .false.
        end if

    end function test_if_condition_parsing

    logical function test_full_if_block()
        ! Test parsing a complete if block through the dispatcher
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: stmt_index

        test_full_if_block = .true.

        print '(a)', "Testing full if block parsing through dispatcher..."

        ! Create tokens for complete if block
        allocate (tokens(8))
        tokens(1) = token_t(TK_KEYWORD, "if", 1, 1)
        tokens(2) = token_t(TK_OPERATOR, "(", 1, 4)
        tokens(3) = token_t(TK_IDENTIFIER, "x", 1, 5)
        tokens(4) = token_t(TK_OPERATOR, ">", 1, 7)
        tokens(5) = token_t(TK_NUMBER, "0", 1, 9)
        tokens(6) = token_t(TK_OPERATOR, ")", 1, 10)
        tokens(7) = token_t(TK_KEYWORD, "then", 1, 12)
        tokens(8) = token_t(TK_EOF, "", 1, 16)

        ! Create arena
        arena = create_ast_arena()

        ! Parse through dispatcher
        stmt_index = parse_statement_dispatcher(tokens, arena)

        if (stmt_index > 0) then
            print '(a)', "PASS: If block parsed through dispatcher"

            if (allocated(arena%entries(stmt_index)%node_type)) then
                if (arena%entries(stmt_index)%node_type == "if_statement") then
                    print '(a)', "PASS: Dispatcher created if_statement node"
                else
                    print '(a,a)', "FAIL: Dispatcher created wrong node type: ", trim(arena%entries(stmt_index)%node_type)
                    test_full_if_block = .false.
                end if
            else
                print '(a)', "FAIL: Node type not allocated"
                test_full_if_block = .false.
            end if
        else
            print '(a)', "FAIL: If block not parsed through dispatcher"
            test_full_if_block = .false.
        end if

    end function test_full_if_block

end program test_frontend_parser_if_statement
