program test_array_slicing_codegen
    use fortfront, only: emit_fortran, lex_source, parse_tokens, &
                        analyze_semantics, token_t, &
                        ast_arena_t, create_ast_arena
    implicit none

    logical :: all_passed
    all_passed = .true.

    if (.not. test_basic_slices())      all_passed = .false.
    if (.not. test_empty_bounds())       all_passed = .false.
    if (.not. test_multidim_slices())    all_passed = .false.

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
        test_basic_slices = .true.

        source = &
            'program p' // new_line('a') // &
            '  integer :: arr(5)' // new_line('a') // &
            '  arr(2:4) = [10, 20, 30]' // new_line('a') // &
            'end program p'

        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root, error_msg)
        if (error_msg /= '') then
            print *, '  FAIL: parse error: ', trim(error_msg)
            test_basic_slices = .false.
            return
        end if

        call analyze_semantics(arena, root)
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
            '  arr(:)  = arr(:)'  // new_line('a') // &
            'end program p'

        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root, error_msg)
        if (error_msg /= '') then
            print *, '  FAIL: parse error: ', trim(error_msg)
            test_empty_bounds = .false.
            return
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
        ! Accept either 'arr(:)' or 'arr(:,:)' variations depending on internal transforms
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
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root, error_msg)
        if (error_msg /= '') then
            print *, '  FAIL: parse error: ', trim(error_msg)
            test_multidim_slices = .false.
            return
        end if

        call analyze_semantics(arena, root)
        call emit_fortran(arena, root, code)
        if (.not. allocated(code)) then
            print *, '  FAIL: no generated code'
            test_multidim_slices = .false.
            return
        end if

        if (index(code, 'b(1:2, :3)') == 0 .and. index(code, 'b(1:2,:3)') == 0) then
            print *, '  FAIL: missing multidimensional slice b(1:2, :3)'
            test_multidim_slices = .false.
            return
        end if
    end function test_multidim_slices

end program test_array_slicing_codegen
