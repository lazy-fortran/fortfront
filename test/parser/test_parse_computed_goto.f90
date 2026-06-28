program test_parse_computed_goto
    use, intrinsic :: iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, tokenize_core
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_control_statements_module, only: parse_goto_statement
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_transfer, only: goto_node
    use ast_nodes_core, only: identifier_node
    implicit none

    character(len=:), allocatable :: source
    type(token_t), allocatable :: tokens(:)
    type(parser_state_t) :: parser
    type(ast_arena_t) :: arena
    integer :: goto_index, selector_index

    print *, '=== Test: parse computed goto preserves selector ==='

    source = 'goto (100, 200, 300), choice'

    call tokenize_core(source, tokens)
    parser = create_parser_state(tokens)
    arena = create_ast_arena(32)

    goto_index = parse_goto_statement(parser, arena)
    if (goto_index <= 0) then
        write (error_unit, '(a)') 'FAIL: parse_goto_statement returned invalid index'
        stop 1
    end if

    if (.not. arena%has_node_at(goto_index)) then
        write (error_unit, '(a)') 'FAIL: arena missing goto node'
        stop 1
    end if

    select type (stmt => arena%entries(goto_index)%node)
        type is (goto_node)
        if (.not. allocated(stmt%label_list)) then
            write (error_unit, '(a)') 'FAIL: computed goto label list missing'
            stop 1
        end if

        if (trim(stmt%label_list) /= '100, 200, 300') then
            write (error_unit, '(a,1x,a)') 'FAIL: unexpected label list:', &
                trim(stmt%label_list)
            stop 1
        end if

        if (stmt%selector_index <= 0) then
            write (error_unit, '(a)') 'FAIL: selector index not recorded'
            stop 1
        end if
        selector_index = stmt%selector_index
    class default
        write (error_unit, '(a)') 'FAIL: node type is not goto_node'
        stop 1
    end select

    if (.not. arena%has_node_at(selector_index)) then
        write (error_unit, '(a)') 'FAIL: selector node missing from arena'
        stop 1
    end if

    select type (selector => arena%entries(selector_index)%node)
        type is (identifier_node)
        if (trim(selector%name) /= 'choice') then
            write (error_unit, '(a,1x,a)') 'FAIL: selector identifier mismatch:', &
                trim(selector%name)
            stop 1
        end if
    class default
        write (error_unit, '(a)') 'FAIL: selector node not identifier'
        stop 1
    end select

    print *, 'PASS: computed goto preserves labels and selector'
end program test_parse_computed_goto
