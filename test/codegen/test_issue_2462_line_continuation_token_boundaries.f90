program test_issue_2462_line_continuation_token_boundaries
    use, intrinsic :: iso_fortran_env, only: error_unit
    use codegen_basic_utils, only: add_line_continuations
    implicit none

    character(len=:), allocatable :: input_line, output
    integer :: i

    ! Test 1: Long use statement with identifiers at column boundary
    input_line = 'use fortfront, only: tooling_parse_options_t, ' // &
        'tooling_load_ast_from_string, ast_arena_t, token_t, ' // &
        'get_node_type_at, ast_to_json'
    output = add_line_continuations(input_line)

    ! Verify no identifier is broken mid-word
    if (index(output, 'ast_to_jso &') > 0) then
        write (error_unit, '(A)') 'FAIL: identifier ast_to_json broken incorrectly'
        stop 1
    end if

    if (index(output, 'ast_to_json') == 0) then
        write (error_unit, '(A)') 'FAIL: identifier ast_to_json missing from output'
        stop 1
    end if

    ! Test 2: Long string parameter with content at boundary
    input_line = 'character(len=*), parameter :: lazy_code = ' // &
        '''sum_neg = 0'' //new_line(''a'') //''sum_pos = 0'' ' // &
        '//new_line(''a'') //''do i = 1, 6'''
    output = add_line_continuations(input_line)

    ! Verify no identifier inside string is broken
    if (index(output, 'sum_ &') > 0) then
        write (error_unit, '(A)') 'FAIL: identifier sum_pos broken incorrectly'
        stop 1
    end if

    ! Test 3: Verify continuation is added for genuinely long lines
    input_line = repeat('a', 150)
    output = add_line_continuations(input_line)

    if (index(output, ' &') == 0) then
        write (error_unit, '(A)') 'FAIL: expected continuation for very long line'
        stop 1
    end if

    print *, 'PASS: Line continuation respects token boundaries'

end program test_issue_2462_line_continuation_token_boundaries
