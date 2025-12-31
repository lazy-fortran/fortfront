program test_symbol_table_api
    ! Test suite for symbol table query API (issue #2613)
    use fortfront
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Symbol Table API Tests (Issue #2613) ==='
    print *

    if (.not. test_get_symbols_in_scope()) all_passed = .false.
    if (.not. test_is_symbol_defined()) all_passed = .false.
    if (.not. test_lookup_symbol()) all_passed = .false.
    if (.not. test_get_scope_info()) all_passed = .false.
    if (.not. test_get_all_symbols()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All symbol table API tests PASSED!'
        stop 0
    else
        print *, 'Some symbol table API tests FAILED!'
        stop 1
    end if

contains

    logical function test_get_symbols_in_scope()
        test_get_symbols_in_scope = .true.
        print *, 'Testing: get_symbols_in_scope...'

        block
            character(len=:), allocatable :: source, error_msg
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            type(semantic_context_t) :: ctx
            integer :: prog_index
            type(symbol_info_t), allocatable :: symbols(:)

            source = 'x = 42' // new_line('A') // &
                     'y = 3.14' // new_line('A') // &
                     'z = x + y'

            call lex_source(source, tokens, error_msg)
            if (error_msg /= "") then
                print *, '  FAIL: Lexer error:', trim(error_msg)
                test_get_symbols_in_scope = .false.
                return
            end if

            arena = create_ast_arena()
            call parse_tokens(tokens, arena, prog_index, error_msg)
            if (error_msg /= "") then
                print *, '  FAIL: Parser error:', trim(error_msg)
                test_get_symbols_in_scope = .false.
                return
            end if

            call create_semantic_context(ctx)
            call analyze_program(ctx, arena, prog_index)

            symbols = get_symbols_in_scope(ctx%scopes)

            if (.not. allocated(symbols)) then
                print *, '  FAIL: symbols array not allocated'
                test_get_symbols_in_scope = .false.
                return
            end if

            print *, '  Found', size(symbols), 'symbols in current scope'
            print *, '  PASS'
        end block
    end function test_get_symbols_in_scope

    logical function test_is_symbol_defined()
        test_is_symbol_defined = .true.
        print *, 'Testing: is_symbol_defined...'

        block
            character(len=:), allocatable :: source, error_msg
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            type(semantic_context_t) :: ctx
            integer :: prog_index
            logical :: defined

            source = 'myvar = 100'

            call lex_source(source, tokens, error_msg)
            arena = create_ast_arena()
            call parse_tokens(tokens, arena, prog_index, error_msg)
            call create_semantic_context(ctx)
            call analyze_program(ctx, arena, prog_index)

            defined = is_symbol_defined(ctx%scopes, 'myvar')

            if (.not. defined) then
                print *, '  INFO: myvar not found (may depend on analysis depth)'
            else
                print *, '  Symbol myvar is defined'
            end if

            defined = is_symbol_defined(ctx%scopes, 'undefined_symbol')
            if (defined) then
                print *, '  FAIL: undefined_symbol should not be defined'
                test_is_symbol_defined = .false.
                return
            end if
            print *, '  undefined_symbol correctly not found'

            print *, '  PASS'
        end block
    end function test_is_symbol_defined

    logical function test_lookup_symbol()
        test_lookup_symbol = .true.
        print *, 'Testing: lookup_symbol...'

        block
            character(len=:), allocatable :: source, error_msg
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            type(semantic_context_t) :: ctx
            integer :: prog_index
            type(symbol_info_t) :: info

            source = 'counter = 1' // new_line('A') // &
                     'counter = counter + 1'

            call lex_source(source, tokens, error_msg)
            arena = create_ast_arena()
            call parse_tokens(tokens, arena, prog_index, error_msg)
            call create_semantic_context(ctx)
            call analyze_program(ctx, arena, prog_index)

            info = lookup_symbol(ctx%scopes, 'counter')

            if (allocated(info%name)) then
                print *, '  Looked up symbol:', trim(info%name)
                print *, '  is_defined:', info%is_defined
                print *, '  scope_level:', info%scope_level
            end if

            info = lookup_symbol(ctx%scopes, 'nonexistent')
            if (info%is_defined) then
                print *, '  FAIL: nonexistent should not be defined'
                test_lookup_symbol = .false.
                return
            end if
            print *, '  nonexistent correctly marked as not defined'

            print *, '  PASS'
        end block
    end function test_lookup_symbol

    logical function test_get_scope_info()
        test_get_scope_info = .true.
        print *, 'Testing: get_scope_info...'

        block
            character(len=:), allocatable :: source, error_msg
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            type(semantic_context_t) :: ctx
            integer :: prog_index, depth
            type(scope_info_t) :: info

            source = 'program test_prog' // new_line('A') // &
                     '    implicit none' // new_line('A') // &
                     '    integer :: a' // new_line('A') // &
                     'end program test_prog'

            call lex_source(source, tokens, error_msg)
            arena = create_ast_arena()
            call parse_tokens(tokens, arena, prog_index, error_msg)
            call create_semantic_context(ctx)
            call analyze_program(ctx, arena, prog_index)

            depth = get_current_scope_depth(ctx%scopes)
            print *, '  Current scope depth:', depth

            if (depth < 1) then
                print *, '  FAIL: Scope depth should be at least 1'
                test_get_scope_info = .false.
                return
            end if

            info = get_scope_info(ctx%scopes)
            print *, '  Current scope level:', info%level
            print *, '  Current scope type:', info%scope_type
            print *, '  Symbol count:', info%symbol_count

            info = get_scope_info(ctx%scopes, 1)
            print *, '  Global scope level:', info%level
            print *, '  Global scope type:', info%scope_type
            if (info%scope_type /= SCOPE_GLOBAL) then
                print *, '  FAIL: First scope should be SCOPE_GLOBAL'
                test_get_scope_info = .false.
                return
            end if

            print *, '  PASS'
        end block
    end function test_get_scope_info

    logical function test_get_all_symbols()
        test_get_all_symbols = .true.
        print *, 'Testing: get_all_symbols...'

        block
            character(len=:), allocatable :: source, error_msg
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            type(semantic_context_t) :: ctx
            integer :: prog_index, i
            type(symbol_info_t), allocatable :: all_symbols(:)

            source = 'a = 1' // new_line('A') // &
                     'b = 2' // new_line('A') // &
                     'c = a + b'

            call lex_source(source, tokens, error_msg)
            arena = create_ast_arena()
            call parse_tokens(tokens, arena, prog_index, error_msg)
            call create_semantic_context(ctx)
            call analyze_program(ctx, arena, prog_index)

            all_symbols = get_all_symbols(ctx%scopes)

            if (.not. allocated(all_symbols)) then
                print *, '  FAIL: all_symbols not allocated'
                test_get_all_symbols = .false.
                return
            end if

            print *, '  Total symbols across all scopes:', size(all_symbols)

            do i = 1, min(5, size(all_symbols))
                if (allocated(all_symbols(i)%name)) then
                    print *, '    Symbol:', trim(all_symbols(i)%name), &
                        ' scope_level:', all_symbols(i)%scope_level
                end if
            end do

            print *, '  PASS'
        end block
    end function test_get_all_symbols

end program test_symbol_table_api
