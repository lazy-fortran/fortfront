program test_intrinsic_parser
    use lexer_core
    use parser_expressions_module
    use ast_core
    implicit none

    type(ast_arena_t) :: arena
    integer :: test_count, passed_count

    test_count = 0
    passed_count = 0

    print *, "Testing intrinsic function parser integration..."

    ! Initialize arena
    arena = create_ast_arena()

    ! Test parsing intrinsic function call
    call test_parse_intrinsic("abs(x)", "abs", "numeric(numeric)", arena, test_count, passed_count)
    call test_parse_intrinsic("sin(theta)", "sin", "real(numeric)", arena, test_count, passed_count)
    call test_parse_intrinsic("sqrt(value)", "sqrt", "real(numeric)", arena, test_count, passed_count)
    call test_parse_intrinsic("len(str)", "len", "integer(character)", arena, test_count, passed_count)
    
    ! Test parsing non-intrinsic function call
    call test_parse_non_intrinsic("my_func(x)", "my_func", arena, test_count, passed_count)
    call test_parse_non_intrinsic("user_sqrt(val)", "user_sqrt", arena, test_count, passed_count)

    ! Summary
    print *, ""
    print *, "Test Results:"
    print *, "  Total tests: ", test_count
    print *, "  Passed:      ", passed_count
    print *, "  Failed:      ", test_count - passed_count
    
    if (passed_count == test_count) then
        print *, "All parser intrinsic tests passed!"
        stop 0
    else
        print *, "Some parser intrinsic tests failed!"
        stop 1
    end if

contains

    subroutine test_parse_intrinsic(source, expected_name, expected_signature, arena, test_count, passed_count)
        character(len=*), intent(in) :: source, expected_name, expected_signature
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(inout) :: test_count, passed_count
        type(token_t), allocatable :: tokens(:)
        integer :: expr_index
        type(call_or_subscript_node) :: call_node

        test_count = test_count + 1

        ! Tokenize the source
        call tokenize_core(source, tokens)
        
        if (size(tokens) < 3) then
            print *, "FAIL: Not enough tokens for '", source, "'"
            return
        end if

        ! Parse the expression
        expr_index = parse_expression(tokens, arena)
        
        if (expr_index <= 0) then
            print *, "FAIL: Failed to parse '", source, "'"
            return
        end if

        ! Get the node and check it
        if (allocated(arena%entries(expr_index)%node)) then
            select type(node => arena%entries(expr_index)%node)
            type is (call_or_subscript_node)
                call_node = node
                if (trim(call_node%name) == trim(expected_name) .and. &
                    call_node%is_intrinsic .and. &
                    allocated(call_node%intrinsic_signature) .and. &
                    trim(call_node%intrinsic_signature) == trim(expected_signature)) then
                    passed_count = passed_count + 1
                    print *, "PASS: Parser intrinsic test for '", trim(source), "'"
                else
                    print *, "FAIL: Parser intrinsic test for '", trim(source), "'"
                    print *, "      Expected name: ", trim(expected_name)
                    print *, "      Got name:      ", trim(call_node%name)
                    print *, "      Expected sig:  ", trim(expected_signature)
                    if (allocated(call_node%intrinsic_signature)) then
                        print *, "      Got sig:       ", trim(call_node%intrinsic_signature)
                    else
                        print *, "      Got sig:       <not allocated>"
                    end if
                    print *, "      Is intrinsic:  ", call_node%is_intrinsic
                end if
            class default
                print *, "FAIL: Wrong node type for '", trim(source), "'"
            end select
        else
            print *, "FAIL: Node not allocated for '", trim(source), "'"
        end if
    end subroutine test_parse_intrinsic

    subroutine test_parse_non_intrinsic(source, expected_name, arena, test_count, passed_count)
        character(len=*), intent(in) :: source, expected_name
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(inout) :: test_count, passed_count
        type(token_t), allocatable :: tokens(:)
        integer :: expr_index
        type(call_or_subscript_node) :: call_node

        test_count = test_count + 1

        ! Tokenize the source
        call tokenize_core(source, tokens)
        
        if (size(tokens) < 3) then
            print *, "FAIL: Not enough tokens for '", source, "'"
            return
        end if

        ! Parse the expression
        expr_index = parse_expression(tokens, arena)
        
        if (expr_index <= 0) then
            print *, "FAIL: Failed to parse '", source, "'"
            return
        end if

        ! Get the node and check it
        if (allocated(arena%entries(expr_index)%node)) then
            select type(node => arena%entries(expr_index)%node)
            type is (call_or_subscript_node)
                call_node = node
                if (trim(call_node%name) == trim(expected_name) .and. &
                    .not. call_node%is_intrinsic) then
                    passed_count = passed_count + 1
                    print *, "PASS: Parser non-intrinsic test for '", trim(source), "'"
                else
                    print *, "FAIL: Parser non-intrinsic test for '", trim(source), "'"
                    print *, "      Expected name: ", trim(expected_name)
                    print *, "      Got name:      ", trim(call_node%name)
                    print *, "      Is intrinsic:  ", call_node%is_intrinsic, " (should be false)"
                end if
            class default
                print *, "FAIL: Wrong node type for '", trim(source), "'"
            end select
        else
            print *, "FAIL: Node not allocated for '", trim(source), "'"
        end if
    end subroutine test_parse_non_intrinsic

end program test_intrinsic_parser