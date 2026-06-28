program test_parse_if_nested_forall
    use, intrinsic :: iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, tokenize_core
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_if_constructs_module, only: parse_if
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_loops, only: forall_node
    use ast_nodes_control, only: if_node
    use ast_nodes_core, only: literal_node
    implicit none

    character(len=:), allocatable :: source
    type(token_t), allocatable :: tokens(:)
    type(parser_state_t) :: parser
    type(ast_arena_t) :: arena
    integer :: if_index, i
    logical :: has_forall, has_unparsed

    print *, '=== Test: parse_if handles nested FORALL without placeholders ==='

    source = 'if (flag) then' // new_line('a') // &
        '  forall (i = 1:5)' // new_line('a') // &
        '    a(i) = i' // new_line('a') // &
        '  end forall' // new_line('a') // &
        'end if'

    call tokenize_core(source, tokens)
    parser = create_parser_state(tokens)
    arena = create_ast_arena(64)

    if_index = parse_if(parser, arena)
    if (if_index <= 0) then
        write (error_unit, '(a)') 'ERROR: parse_if returned invalid index'
        stop 1
    end if

    has_forall = .false.
    has_unparsed = .false.

    do i = 1, arena%size
        if (.not. arena%has_node_at(i)) cycle
        select type (node => arena%entries(i)%node)
            type is (forall_node)
            has_forall = .true.
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

    if (.not. has_forall) then
        write (error_unit, '(a)') 'ERROR: nested FORALL node not created'
        stop 1
    end if

    if (has_unparsed) then
        write (error_unit, '(a)') 'ERROR: unexpected ! Unparsed literal emitted'
        stop 1
    end if

    print *, 'PASS: parse_if registers FORALL parser and preserves assignments'

end program test_parse_if_nested_forall
