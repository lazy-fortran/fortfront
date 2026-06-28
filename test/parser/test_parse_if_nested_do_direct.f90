program test_parse_if_nested_do_direct
    use, intrinsic :: iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, tokenize_core
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_if_constructs_module, only: parse_if
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_loops, only: do_loop_node
    use ast_nodes_control, only: if_node
    use ast_nodes_core, only: literal_node
    implicit none

    character(len=:), allocatable :: source
    type(token_t), allocatable :: tokens(:)
    type(parser_state_t) :: parser
    type(ast_arena_t) :: arena
    integer :: if_index, i
    logical :: has_do_loop, has_unparsed

    print *, '=== Test: direct parse_if handles nested DO without preregistration ==='

    source = 'if (flag) then' // new_line('a') // &
        '  do i = 1, 3' // new_line('a') // &
        '    print *, i' // new_line('a') // &
        '  end do' // new_line('a') // &
        'end if'

    call tokenize_core(source, tokens)
    parser = create_parser_state(tokens)
    arena = create_ast_arena(64)

    if_index = parse_if(parser, arena)
    if (if_index <= 0) then
        write (error_unit, '(a)') 'ERROR: parse_if returned invalid index'
        stop 1
    end if

    has_do_loop = .false.
    has_unparsed = .false.

    do i = 1, arena%size
        if (.not. arena%has_node_at(i)) cycle
        select type (node => arena%entries(i)%node)
            type is (do_loop_node)
            has_do_loop = .true.
            type is (literal_node)
            if (allocated(node%value)) then
                if (index(node%value, '! Unparsed') > 0) has_unparsed = .true.
            end if
            type is (if_node)
            cycle
        class default
            cycle
        end select
    end do

    if (.not. has_do_loop) then
        write (error_unit, '(a)') 'ERROR: nested DO loop node not created'
        stop 1
    end if

    if (has_unparsed) then
        write (error_unit, '(a)') 'ERROR: unexpected ! Unparsed literal emitted'
        stop 1
    end if

    print *, 'PASS: parse_if registers DO parser lazily'

end program test_parse_if_nested_do_direct
