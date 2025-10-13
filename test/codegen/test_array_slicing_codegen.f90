program test_array_slicing_codegen
    use fortfront, only: emit_fortran, lex_source, parse_tokens, &
                         analyze_semantics, token_t, &
                         ast_arena_t, create_ast_arena
    use ast_nodes_core, only: program_node, assignment_node
    use ast_nodes_misc, only: implicit_statement_node
    use ast_nodes_bounds, only: array_operation_node
    implicit none

    logical :: all_passed
    all_passed = .true.

    if (.not. test_basic_slices()) all_passed = .false.
    if (.not. test_empty_bounds()) all_passed = .false.
    if (.not. test_multidim_slices()) all_passed = .false.

    if (all_passed) then
        print *, 'All array slicing codegen tests passed!'
    else
        print *, 'Some array slicing codegen tests failed!'
    end if

contains

    logical function test_basic_slices()
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        character(len=:), allocatable :: error_msg, code, source
        integer :: root
        integer :: i
        integer :: i
        test_basic_slices = .true.

        source = &
            'program p' // new_line('a') // &
            '  integer :: arr(5)' // new_line('a') // &
            '  arr(2:4) = [10, 20, 30]' // new_line('a') // &
            'end program p'

    call lex_source(source, tokens, error_msg)
    print *, 'DEBUG tokens for multi-dim:'
    do i = 1, size(tokens)
        print *, '  token', i, ':', trim(tokens(i)%text), 'kind', tokens(i)%kind
    end do
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root, error_msg)
        if (root > 0 .and. root <= arena%size) then
            if (allocated(arena%entries(root)%node)) then
                select type (prog_node => arena%entries(root)%node)
                type is (program_node)
                    print *, 'DEBUG program body size:', size(prog_node%body_indices)
                    if (allocated(prog_node%body_indices)) then
                        print *, 'DEBUG body indices:', prog_node%body_indices
                        do i = 1, size(prog_node%body_indices)
                            if (prog_node%body_indices(i) > 0 .and. &
                                prog_node%body_indices(i) <= arena%size) then
                                if (allocated(arena%entries(prog_node%body_indices(i))%node)) then
                                    select type (body_node => arena%entries(prog_node%body_indices(i))%node)
                                    type is (implicit_statement_node)
                                        print *, 'DEBUG body node', i, ': implicit none'
                                    type is (assignment_node)
                                        print *, 'DEBUG body node', i, ': assignment'
                                    type is (array_operation_node)
                                        print *, 'DEBUG body node', i, ': array operation'
                                    class default
                                        print *, 'DEBUG body node', i, ': other'
                                    end select
                                end if
                            end if
                        end do
                    end if
                end select
            end if
        end if
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: parse error: ', trim(error_msg)
                test_basic_slices = .false.
                return
            end if
        end if

    call analyze_semantics(arena, root)
    if (root > 0 .and. root <= arena%size) then
        if (allocated(arena%entries(root)%node)) then
            select type (prog_node => arena%entries(root)%node)
            type is (program_node)
                if (allocated(prog_node%body_indices)) then
                    do i = 1, size(prog_node%body_indices)
                        if (prog_node%body_indices(i) > 0 .and. &
                            prog_node%body_indices(i) <= arena%size) then
                            if (allocated(arena%entries(prog_node%body_indices(i))%node)) then
                                select type (body_node => arena%entries(prog_node%body_indices(i))%node)
                                type is (implicit_statement_node)
                                    print *, 'DEBUG post-sem body node', i, ': implicit none'
                                type is (assignment_node)
                                    print *, 'DEBUG post-sem body node', i, ': assignment'
                                type is (array_operation_node)
                                    print *, 'DEBUG post-sem body node', i, ': array operation'
                                class default
                                    print *, 'DEBUG post-sem body node', i, ': other'
                                end select
                            end if
                        end if
                    end do
                end if
            end select
        end if
    end if
        call emit_fortran(arena, root, code)
        if (.not. allocated(code)) then
            print *, '  FAIL: no generated code'
            test_basic_slices = .false.
            return
        end if

        if (index(code, 'arr(2:4)') == 0) then
            print *, '  FAIL: missing slice arr(2:4) in code'
            test_basic_slices = .false.
            return
        end if

        ! Array constructor can be [] or (/ /) depending on configuration
        if (index(code, '[10, 20, 30]') == 0 .and. index(code, '(/10, 20, 30/)') == 0) then
            print *, '  FAIL: missing array constructor for RHS'
            test_basic_slices = .false.
            return
        end if
    end function test_basic_slices

    logical function test_empty_bounds()
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        character(len=:), allocatable :: error_msg, code, source
        integer :: root
        test_empty_bounds = .true.

        source = &
            'program p' // new_line('a') // &
            '  integer :: arr(5)' // new_line('a') // &
            '  arr(:3) = arr(:3)' // new_line('a') // &
            '  arr(2:) = arr(2:)' // new_line('a') // &
            '  arr(:)  = arr(:)' // new_line('a') // &
            'end program p'

        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: parse error: ', trim(error_msg)
                test_empty_bounds = .false.
                return
            end if
        end if

        call analyze_semantics(arena, root)
        call emit_fortran(arena, root, code)
        if (.not. allocated(code)) then
            print *, '  FAIL: no generated code'
            test_empty_bounds = .false.
            return
        end if

        if (index(code, 'arr(:3)') == 0) then
            print *, '  FAIL: missing arr(:3)'
            test_empty_bounds = .false.
            return
        end if
        if (index(code, 'arr(2:)') == 0) then
            print *, '  FAIL: missing arr(2:)'
            test_empty_bounds = .false.
            return
        end if
        ! Expect a full-range slice for 1-D array
        if (index(code, 'arr(:)') == 0) then
            print *, '  FAIL: missing arr(:)'
            test_empty_bounds = .false.
            return
        end if
    end function test_empty_bounds

    logical function test_multidim_slices()
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        character(len=:), allocatable :: error_msg, code, source
        integer :: root
        test_multidim_slices = .true.

    source = &
        'program p' // new_line('a') // &
        '  integer :: b(4,4)' // new_line('a') // &
        '  b(1:2, :3) = 0' // new_line('a') // &
        'end program p'

    call lex_source(source, tokens, error_msg)
    do i = 1, size(tokens)
        print *, 'DEBUG multi tokens', i, ':', trim(tokens(i)%text), 'kind', tokens(i)%kind
    end do
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: parse error: ', trim(error_msg)
                test_multidim_slices = .false.
                return
            end if
        end if

        call analyze_semantics(arena, root)
        call emit_fortran(arena, root, code)
        if (.not. allocated(code)) then
            print *, '  FAIL: no generated code'
            test_multidim_slices = .false.
            return
        end if
        print *, 'DEBUG slice code:', trim(code)

        if (index(code, 'b(1:2, :3)') == 0 .and. index(code, 'b(1:2,:3)') == 0) then
            print *, '  FAIL: missing multidimensional slice b(1:2, :3)'
            test_multidim_slices = .false.
            return
        end if
    end function test_multidim_slices

end program test_array_slicing_codegen
