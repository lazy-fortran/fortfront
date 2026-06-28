program test_parse_forall_construct_body
    use, intrinsic :: iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, tokenize_core
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_forall_module, only: parse_forall
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_loops, only: forall_node
    use ast_nodes_core, only: assignment_node, literal_node
    implicit none

    character(len=:), allocatable :: source
    type(token_t), allocatable :: tokens(:)
    type(parser_state_t) :: parser
    type(ast_arena_t) :: arena
    integer :: forall_index, i
    logical :: has_assignment, has_unparsed

    print *, '=== Test: parse_forall constructs body assignments ==='

    source = 'forall (i = 1:5)' // new_line('a') // &
        '  a(i) = i' // new_line('a') // &
        'end forall'

    call tokenize_core(source, tokens)
    parser = create_parser_state(tokens)
    arena = create_ast_arena(32)

    forall_index = parse_forall(parser, arena)
    if (forall_index <= 0) then
        write (error_unit, '(a)') 'ERROR: parse_forall returned invalid index'
        stop 1
    end if

    has_assignment = .false.
    has_unparsed = .false.

    if (.not. arena%has_node_at(forall_index)) then
        write (error_unit, '(a)') 'ERROR: Forall node missing from arena'
        stop 1
    end if

    select type (node => arena%entries(forall_index)%node)
        type is (forall_node)
        if (.not. allocated(node%body_indices)) then
            write (error_unit, '(a)') 'ERROR: Forall body indices not allocated'
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
                    if (index(body_node%value, '! Unparsed') > 0) has_unparsed = .true.
                end if
            class default
                cycle
            end select
        end do
    class default
        write (error_unit, '(a)') 'ERROR: Node returned is not a forall_node'
        stop 1
    end select

    if (.not. has_assignment) then
        write (error_unit, '(a)') 'ERROR: Forall body missing assignment node'
        stop 1
    end if

    if (has_unparsed) then
        write (error_unit, '(a)') 'ERROR: Forall body still contains ! Unparsed placeholder'
        stop 1
    end if

    print *, 'PASS: parse_forall emits assignment body without placeholders'

end program test_parse_forall_construct_body
