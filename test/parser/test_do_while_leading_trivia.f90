program test_do_while_leading_trivia
    use, intrinsic :: iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, tokenize_core
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_do_constructs_module, only: parse_do_loop
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_loops, only: do_while_node
    use ast_nodes_core, only: assignment_node, literal_node
    implicit none

    character(len=:), allocatable :: source
    type(token_t), allocatable :: tokens(:)
    type(parser_state_t) :: parser
    type(ast_arena_t) :: arena
    integer :: loop_index, i
    logical :: has_assignment, has_unparsed

    print *, '=== Test: DO WHILE skips header trivia before its body ==='

    source = 'do while (flag)' // new_line('a') // &
        '  value = value + 1' // new_line('a') // &
        'end do'

    call tokenize_core(source, tokens)
    parser = create_parser_state(tokens)
    arena = create_ast_arena(32)

    loop_index = parse_do_loop(parser, arena)
    if (loop_index <= 0) then
        write (error_unit, '(a)') 'ERROR: parse_do_loop returned invalid index'
        stop 1
    end if

    if (parser%has_errors()) then
        write (error_unit, '(a)') 'ERROR: DO WHILE body produced parser errors'
        stop 1
    end if

    has_assignment = .false.
    has_unparsed = .false.
    if (.not. arena%has_node_at(loop_index)) then
        write (error_unit, '(a)') 'ERROR: DO WHILE node missing from arena'
        stop 1
    end if

    select type (node => arena%entries(loop_index)%node)
        type is (do_while_node)
        if (.not. allocated(node%body_indices)) then
            write (error_unit, '(a)') 'ERROR: DO WHILE body indices not allocated'
            stop 1
        end if
        do i = 1, size(node%body_indices)
            if (node%body_indices(i) <= 0) cycle
            if (.not. arena%has_node_at(node%body_indices(i))) cycle
            select type (body_node => arena%entries(node%body_indices(i))%node)
                type is (assignment_node)
                has_assignment = .true.
                type is (literal_node)
                if (allocated(body_node%value)) then
                    if (index(body_node%value, '! Unparsed') > 0) then
                        has_unparsed = .true.
                    end if
                end if
            class default
                cycle
            end select
        end do
    class default
        write (error_unit, '(a)') 'ERROR: parsed node is not DO WHILE'
        stop 1
    end select

    if (.not. has_assignment) then
        write (error_unit, '(a)') 'ERROR: DO WHILE body assignment was lost'
        stop 1
    end if
    if (has_unparsed) then
        write (error_unit, '(a)') 'ERROR: DO WHILE body contains ! Unparsed'
        stop 1
    end if

    print *, 'PASS: DO WHILE body starts after header trivia'
end program test_do_while_leading_trivia
