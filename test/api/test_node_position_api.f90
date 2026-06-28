program test_node_position_api
    use fortfront, only: tooling_load_ast_from_string, ast_arena_t, &
        tooling_parse_options_t, get_node_line, get_node_column
    implicit none

    logical :: all_passed

    print *, '=== Node Position API Tests (Issue #2598) ==='
    print *

    all_passed = .true.
    if (.not. test_get_node_line_column()) all_passed = .false.
    if (.not. test_invalid_index()) all_passed = .false.
    if (.not. test_type_bound_consistency()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All node position API tests passed!'
        stop 0
    else
        print *, 'Node position API tests failed!'
        stop 1
    end if

contains

    logical function test_get_node_line_column()
        type(ast_arena_t) :: arena
        type(tooling_parse_options_t) :: options
        character(len=:), allocatable :: error_msg
        integer :: root_index, line, col
        character(len=*), parameter :: source = &
            'program test' // new_line('a') // &
            '  x = 5' // new_line('a') // &
            'end program'

        test_get_node_line_column = .true.
        print *, 'Testing get_node_line/get_node_column...'

        options = tooling_parse_options_t()
        options%run_semantics = .false.

        call tooling_load_ast_from_string(source, arena, root_index, error_msg, &
            options)

        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, '  FAIL: parse error ->', trim(error_msg)
            test_get_node_line_column = .false.
            return
        end if

        if (root_index <= 0) then
            print *, '  FAIL: invalid root index'
            test_get_node_line_column = .false.
            return
        end if

        line = get_node_line(arena, root_index)
        col = get_node_column(arena, root_index)

        if (line <= 0) then
            print *, '  FAIL: get_node_line returned', line, '(expected > 0)'
            test_get_node_line_column = .false.
            return
        end if

        if (col <= 0) then
            print *, '  FAIL: get_node_column returned', col, '(expected > 0)'
            test_get_node_line_column = .false.
            return
        end if

        print *, '  PASS: line =', line, ', column =', col
    end function test_get_node_line_column

    logical function test_invalid_index()
        type(ast_arena_t) :: arena
        type(tooling_parse_options_t) :: options
        character(len=:), allocatable :: error_msg
        integer :: root_index, line, col
        character(len=*), parameter :: source = 'program test' // new_line('a') // &
            'end program'

        test_invalid_index = .true.
        print *, 'Testing invalid index handling...'

        options = tooling_parse_options_t()
        options%run_semantics = .false.

        call tooling_load_ast_from_string(source, arena, root_index, error_msg, &
            options)

        line = get_node_line(arena, -1)
        col = get_node_column(arena, 999999)

        if (line /= 0) then
            print *, '  FAIL: get_node_line(-1) returned', line, '(expected 0)'
            test_invalid_index = .false.
            return
        end if

        if (col /= 0) then
            print *, '  FAIL: get_node_column(999999) returned', col, &
                '(expected 0)'
            test_invalid_index = .false.
            return
        end if

        print *, '  PASS: invalid indices return 0'
    end function test_invalid_index

    logical function test_type_bound_consistency()
        type(ast_arena_t) :: arena
        type(tooling_parse_options_t) :: options
        character(len=:), allocatable :: error_msg
        integer :: root_index, line1, line2, col1, col2
        character(len=*), parameter :: source = 'program test' // new_line('a') // &
            'end program'

        test_type_bound_consistency = .true.
        print *, 'Testing consistency with type-bound procedures...'

        options = tooling_parse_options_t()
        options%run_semantics = .false.

        call tooling_load_ast_from_string(source, arena, root_index, error_msg, &
            options)

        line1 = get_node_line(arena, root_index)
        line2 = arena%get_node_line(root_index)
        col1 = get_node_column(arena, root_index)
        col2 = arena%get_node_column(root_index)

        if (line1 /= line2) then
            print *, '  FAIL: standalone vs type-bound line mismatch:', &
                line1, '/=', line2
            test_type_bound_consistency = .false.
            return
        end if

        if (col1 /= col2) then
            print *, '  FAIL: standalone vs type-bound column mismatch:', &
                col1, '/=', col2
            test_type_bound_consistency = .false.
            return
        end if

        print *, '  PASS: standalone and type-bound return same values'
    end function test_type_bound_consistency

end program test_node_position_api
