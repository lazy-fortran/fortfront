program test_statement_computed_goto
    use, intrinsic :: iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, tokenize_core
    use parser_statement_core_module, only: parse_basic_statement_core
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_transfer, only: goto_node
    use ast_nodes_core, only: identifier_node
    implicit none

    character(len=:), allocatable :: source
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer, allocatable :: stmt_indices(:)
    integer :: goto_index, selector_index, i
    logical :: found

    print *, '=== Test: statement parser handles computed goto ==='

    source = 'goto (100, 200, 300), choice'

    call tokenize_core(source, tokens)
    arena = create_ast_arena(16)

    stmt_indices = parse_basic_statement_core(tokens, arena)
    if (.not. allocated(stmt_indices)) then
        write (error_unit, '(a)') 'FAIL: parser returned no statement indices'
        stop 1
    end if

    found = .false.
    goto_index = -1
    do i = 1, size(stmt_indices)
        if (stmt_indices(i) <= 0) cycle
        if (.not. arena%has_node_at(stmt_indices(i))) cycle
        select type (node => arena%entries(stmt_indices(i))%node)
            type is (goto_node)
            goto_index = stmt_indices(i)
            found = .true.
            exit
        class default
        end select
    end do

    if (.not. found) then
        write (error_unit, '(a)') 'FAIL: no goto_node found'
        stop 1
    end if

    select type (stmt => arena%entries(goto_index)%node)
        type is (goto_node)
        if (.not. allocated(stmt%label_list)) then
            write (error_unit, '(a)') 'FAIL: computed goto label list missing'
            stop 1
        end if
        if (trim(stmt%label_list) /= '100, 200, 300') then
            write (error_unit, '(a,1x,a)') 'FAIL: wrong label list:', &
                trim(stmt%label_list)
            stop 1
        end if
        if (stmt%selector_index <= 0) then
            write (error_unit, '(a)') 'FAIL: selector index missing'
            stop 1
        end if
        selector_index = stmt%selector_index
    class default
        write (error_unit, '(a)') 'FAIL: node at goto_index not goto_node'
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

    print *, 'PASS: statement parser preserves computed goto selector'
end program test_statement_computed_goto
