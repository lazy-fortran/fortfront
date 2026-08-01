program test_do_while_nested_constructs
    use, intrinsic :: iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, tokenize_core
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_do_constructs_module, only: parse_do_loop
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_control, only: if_node
    use ast_nodes_core, only: assignment_node
    use ast_nodes_loops, only: do_loop_node, do_while_node
    implicit none

    character(len=:), allocatable :: source
    type(token_t), allocatable :: tokens(:)
    type(parser_state_t) :: parser
    type(ast_arena_t) :: arena
    integer :: loop_index, outer_if, inner_if, inner_do, i
    logical :: found_nested_shape

    print *, '=== Test: DO WHILE preserves nested IF and DO bodies ==='

    source = 'do while (n >= 2)' // new_line('a') // &
        '  if (n >= i) then' // new_line('a') // &
        '    if (n >= i) then' // new_line('a') // &
        '      do j = i + 1, n' // new_line('a') // &
        '        i = j + i' // new_line('a') // &
        '      end do' // new_line('a') // &
        '    endif' // new_line('a') // &
        '    n = n - 1' // new_line('a') // &
        '  else' // new_line('a') // &
        '    i = j + i' // new_line('a') // &
        '  endif' // new_line('a') // &
        'end do'

    call tokenize_core(source, tokens)
    parser = create_parser_state(tokens)
    arena = create_ast_arena(128)

    loop_index = parse_do_loop(parser, arena)
    if (loop_index <= 0 .or. parser%has_errors()) then
        write (error_unit, '(a)') 'ERROR: nested DO WHILE did not parse'
        stop 1
    end if

    found_nested_shape = .false.
    outer_if = 0
    select type (loop => arena%entries(loop_index)%node)
        type is (do_while_node)
        if (.not. allocated(loop%body_indices)) then
            write (error_unit, '(a)') 'ERROR: loop body not allocated'
            stop 1
        end if
        if (size(loop%body_indices) /= 1) then
            write (error_unit, '(a,I0)') 'ERROR: loop body count=', &
                size(loop%body_indices)
            stop 1
        end if
        outer_if = loop%body_indices(1)
    class default
        write (error_unit, '(a)') 'ERROR: root node is not DO WHILE'
        stop 1
    end select

    select type (if_stmt => arena%entries(outer_if)%node)
        type is (if_node)
        if (.not. allocated(if_stmt%then_body_indices)) then
            write (error_unit, '(a)') 'ERROR: outer IF then body not allocated'
            stop 1
        end if
        if (.not. allocated(if_stmt%else_body_indices)) then
            write (error_unit, '(a)') 'ERROR: outer IF else body not allocated'
            stop 1
        end if
        if (size(if_stmt%then_body_indices) /= 2) then
            write (error_unit, '(a,I0)') 'ERROR: outer IF then count=', &
                size(if_stmt%then_body_indices)
            stop 1
        end if
        if (size(if_stmt%else_body_indices) /= 1) then
            write (error_unit, '(a,I0)') 'ERROR: outer IF else count=', &
                size(if_stmt%else_body_indices)
            stop 1
        end if
        inner_if = 0
        do i = 1, size(if_stmt%then_body_indices)
            if (.not. arena%has_node_at(if_stmt%then_body_indices(i))) cycle
            select type (body_stmt => &
                    arena%entries(if_stmt%then_body_indices(i))%node)
                type is (if_node)
                inner_if = if_stmt%then_body_indices(i)
                class default
                    cycle
            end select
        end do
        if (inner_if <= 0) then
            write (error_unit, '(a)') 'ERROR: nested IF not found'
            stop 1
        end if
    class default
        write (error_unit, '(a)') 'ERROR: outer node is not IF'
        stop 1
    end select

    select type (if_stmt => arena%entries(inner_if)%node)
        type is (if_node)
        if (.not. allocated(if_stmt%then_body_indices)) then
            write (error_unit, '(a)') 'ERROR: nested IF then body not allocated'
            stop 1
        end if
        if (size(if_stmt%then_body_indices) /= 1) then
            write (error_unit, '(a,I0)') 'ERROR: nested IF then count=', &
                size(if_stmt%then_body_indices)
            stop 1
        end if
        inner_do = if_stmt%then_body_indices(1)
        select type (loop => arena%entries(inner_do)%node)
            type is (do_loop_node)
            if (.not. allocated(loop%body_indices)) then
                write (error_unit, '(a)') 'ERROR: inner DO body not allocated'
                stop 1
            end if
            if (size(loop%body_indices) /= 1) then
                write (error_unit, '(a,I0)') 'ERROR: inner DO body count=', &
                    size(loop%body_indices)
                stop 1
            end if
            select type (body_stmt => arena%entries(loop%body_indices(1))%node)
                type is (assignment_node)
                found_nested_shape = .true.
                class default
                    continue
            end select
            class default
                write (error_unit, '(a)') 'ERROR: nested IF body is not DO'
                continue
        end select
    class default
        write (error_unit, '(a)') 'ERROR: nested node is not IF'
        continue
    end select

    if (.not. found_nested_shape) then
        write (error_unit, '(a)') 'ERROR: nested DO WHILE body shape was flattened'
        stop 1
    end if

    print *, 'PASS: DO WHILE preserves nested IF and DO bodies'
end program test_do_while_nested_constructs
