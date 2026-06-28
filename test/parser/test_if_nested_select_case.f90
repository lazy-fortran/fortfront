program test_if_nested_select_case
    use, intrinsic :: iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, tokenize_core
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_if_constructs_module, only: parse_if
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_control, only: if_node, select_case_node
    use ast_nodes_core, only: literal_node
    implicit none

    character(len=:), allocatable :: source
    type(token_t), allocatable :: tokens(:)
    type(parser_state_t) :: parser
    type(ast_arena_t) :: arena
    integer :: if_index, i
    logical :: has_select_case, has_unparsed

    print *, '=== Test: parse_if handles nested SELECT CASE blocks ==='

    source = 'if (flag) then' // new_line('a') // &
        '  select case(value)' // new_line('a') // &
        '  case (1)' // new_line('a') // &
        '    print *, 1' // new_line('a') // &
        '  case default' // new_line('a') // &
        '    print *, 2' // new_line('a') // &
        '  end select' // new_line('a') // &
        'end if'

    call tokenize_core(source, tokens)
    parser = create_parser_state(tokens)
    arena = create_ast_arena(128)

    if_index = parse_if(parser, arena)
    if (if_index <= 0) then
        write (error_unit, '(a)') 'ERROR: parse_if returned invalid index'
        stop 1
    end if

    has_select_case = .false.
    has_unparsed = .false.

    do i = 1, arena%size
        if (.not. arena%has_node_at(i)) cycle
        select type (node => arena%entries(i)%node)
            type is (select_case_node)
            has_select_case = .true.
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

    if (.not. has_select_case) then
        write (error_unit, '(a)') 'ERROR: nested SELECT CASE node missing'
        stop 1
    end if

    if (has_unparsed) then
        write (error_unit, '(a)') 'ERROR: unexpected ! Unparsed literal emitted'
        stop 1
    end if

    print *, 'PASS: nested SELECT CASE parsed without placeholders'

end program test_if_nested_select_case
