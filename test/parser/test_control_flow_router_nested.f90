program test_control_flow_router_nested
    use, intrinsic :: iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, tokenize_core
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_execution_statements_module, only: parse_program_statement
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_loops, only: do_loop_node
    use ast_nodes_control, only: if_node, select_case_node
    use ast_nodes_core, only: literal_node, program_node
    implicit none

    character(len=:), allocatable :: source
    type(token_t), allocatable :: tokens(:)
    type(parser_state_t) :: parser
    type(ast_arena_t) :: arena
    integer :: prog_index, i
    logical :: has_do_loop, has_if_block, has_select_case, has_placeholder

    print *, '=== Test: control-flow router handles nested constructs ==='

    source = 'program nested_cf' // new_line('a') // &
        '  do i = 1, 3' // new_line('a') // &
        '    if (flag) then' // new_line('a') // &
        '      select case(value)' // new_line('a') // &
        '      case (1)' // new_line('a') // &
        '        print *, "one"' // new_line('a') // &
        '      case default' // new_line('a') // &
        '        print *, "other"' // new_line('a') // &
        '      end select' // new_line('a') // &
        '    end if' // new_line('a') // &
        '  end do' // new_line('a') // &
        'end program nested_cf'

    call tokenize_core(source, tokens)
    parser = create_parser_state(tokens)
    arena = create_ast_arena(256)

    prog_index = parse_program_statement(parser, arena)
    if (prog_index <= 0) then
        write (error_unit, '(a)') 'ERROR: parse_program_statement returned invalid index'
        stop 1
    end if

    if (parser%has_errors()) then
        write (error_unit, '(a)') 'ERROR: parser reported structured errors'
        stop 1
    end if

    has_do_loop = .false.
    has_if_block = .false.
    has_select_case = .false.
    has_placeholder = .false.

    do i = 1, arena%size
        if (.not. arena%has_node_at(i)) cycle
        select type (node => arena%entries(i)%node)
            type is (program_node)
            cycle
            type is (do_loop_node)
            has_do_loop = .true.
            type is (if_node)
            has_if_block = .true.
            type is (select_case_node)
            has_select_case = .true.
            type is (literal_node)
            if (allocated(node%value)) then
                if (index(node%value, '! Unparsed') > 0) has_placeholder = .true.
            end if
        end select
    end do

    if (.not. has_do_loop) then
        write (error_unit, '(a)') 'ERROR: expected DO loop node not found'
        stop 1
    end if

    if (.not. has_if_block) then
        write (error_unit, '(a)') 'ERROR: expected IF node not found'
        stop 1
    end if

    if (.not. has_select_case) then
        write (error_unit, '(a)') 'ERROR: expected SELECT CASE node not found'
        stop 1
    end if

    if (has_placeholder) then
        write (error_unit, '(a)') 'ERROR: unexpected placeholder literal emitted'
        stop 1
    end if

    print *, 'PASS: nested control flow parsed without placeholders'

end program test_control_flow_router_nested
