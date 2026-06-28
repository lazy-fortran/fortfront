program test_component_assignment_in_if_body
    ! Regression test for the if-body parser dropping derived-type
    ! component assignments.  Before this fix, parse_identifier_statement
    ! in parser_statement_core only routed '(', '=', '=>' through the
    ! complex/simple assignment paths; '%' fell off the select case and
    ! left stmt_index == 0, so 'p%x = 7' inside an if body produced no
    ! AST node and the if's then_body_indices was empty.  Consumers
    ! downstream (ffc lowering) then ran the if as a no-op, returning
    ! silently-wrong runtime values.
    use frontend_core, only: lex_source
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_conditional, only: if_node
    use ast_nodes_core, only: assignment_node
    implicit none

    character(:), allocatable :: src
    character(:), allocatable :: error_msg
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: prog_index, then_count, else_count, found_then_assign
    integer :: found_else_assign
    integer :: i

    src = 'program test'//new_line('a')// &
        '  type :: point_t'//new_line('a')// &
        '    integer :: x'//new_line('a')// &
        '    integer :: y'//new_line('a')// &
        '  end type point_t'//new_line('a')// &
        '  type(point_t) :: p'//new_line('a')// &
        '  integer :: flag'//new_line('a')// &
        '  flag = 1'//new_line('a')// &
        '  if (flag == 1) then'//new_line('a')// &
        '    p%x = 7'//new_line('a')// &
        '  else'//new_line('a')// &
        '    p%x = 9'//new_line('a')// &
        '  end if'//new_line('a')// &
        '  stop p%x'//new_line('a')// &
        'end program test'

    arena = create_ast_arena()
    call lex_source(src, tokens, error_msg)
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        print *, 'Lex error: ', trim(error_msg)
        error stop 1
    end if
    call parse_tokens(tokens, arena, prog_index, error_msg)
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        print *, 'Parse error: ', trim(error_msg)
        error stop 1
    end if

    then_count = 0
    else_count = 0
    found_then_assign = 0
    found_else_assign = 0
    do i = 1, arena%size
        if (.not. allocated(arena%entries(i)%node)) cycle
        select type (n => arena%entries(i)%node)
            type is (if_node)
            if (allocated(n%then_body_indices)) then
                then_count = size(n%then_body_indices)
                if (then_count >= 1) found_then_assign = n%then_body_indices(1)
            end if
            if (allocated(n%else_body_indices)) then
                else_count = size(n%else_body_indices)
                if (else_count >= 1) found_else_assign = n%else_body_indices(1)
            end if
        end select
    end do

    if (then_count < 1) then
        print *, 'FAIL: if then_body_indices is empty (parser dropped '// &
            'derived-component assignment)'
        error stop 1
    end if
    if (else_count < 1) then
        print *, 'FAIL: if else_body_indices is empty (parser dropped '// &
            'derived-component assignment)'
        error stop 1
    end if
    select type (n => arena%entries(found_then_assign)%node)
        type is (assignment_node)
        ! expected
    class default
        print *, 'FAIL: then body[1] is not an assignment_node'
        error stop 1
    end select
    select type (n => arena%entries(found_else_assign)%node)
        type is (assignment_node)
        ! expected
    class default
        print *, 'FAIL: else body[1] is not an assignment_node'
        error stop 1
    end select

    print *, 'PASS: derived-component assignments survive into if bodies'
end program test_component_assignment_in_if_body
