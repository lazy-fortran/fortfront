program test_named_do_nested_end
    ! Nested named DO constructs: each END DO name must match the construct it
    ! closes, including DO inside DO WHILE with EXIT/CYCLE and one-line IF.
    use, intrinsic :: iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, tokenize_core
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_do_constructs_module, only: parse_do_loop
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    character(len=:), allocatable :: source
    character(len=1), parameter :: nl = new_line('a')
    logical :: ok

    ok = parses_cleanly( &
        'partition: do while (eta < n)'//nl// &
        '  stepwise: do i = 1, 3'//nl// &
        '    control: do k = 1, 4'//nl// &
        '      if (z(k) < 0.d0) then'//nl// &
        '        if (i == 1) then'//nl// &
        '          return'//nl// &
        '        endif'//nl// &
        '        exit stepwise'//nl// &
        '      endif'//nl// &
        '    end do control !inside?'//nl// &
        '    t = t + 1'//nl// &
        '  end do stepwise'//nl// &
        '  if (t > 1) cycle partition'//nl// &
        '  exit partition'//nl// &
        'end do partition')
    if (.not. ok) stop 1

    ok = parses_cleanly( &
        'outer: do i = 1, 3'//nl// &
        '  inner: do j = 1, 3'//nl// &
        '    if (j == 2) cycle outer'//nl// &
        '  end do inner'//nl// &
        'end do outer')
    if (.not. ok) stop 1
    print *, 'PASS'

contains

    logical function parses_cleanly(src) result(good)
        character(len=*), intent(in) :: src
        type(token_t), allocatable :: tokens(:)
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        integer :: idx

        call tokenize_core(src, tokens)
        parser = create_parser_state(tokens)
        arena = create_ast_arena(256)
        idx = parse_do_loop(parser, arena)
        good = idx > 0 .and. .not. parser%has_errors()
        if (.not. good) write (error_unit, '(a)') 'FAIL: '//src
    end function parses_cleanly

end program test_named_do_nested_end
