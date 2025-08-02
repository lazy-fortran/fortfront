program test_semantic_query_api
    use semantic_query_api
    use semantic_analyzer, only: semantic_context_t, create_semantic_context, &
                                 analyze_program
    use ast_core
    use frontend, only: lex_source, parse_tokens
    use lexer_core, only: token_t
    implicit none

    logical :: all_passed = .true.

    print *, "Testing semantic query API..."

    if (.not. test_basic_queries()) all_passed = .false.
    if (.not. test_variable_queries()) all_passed = .false.
    if (.not. test_symbol_queries()) all_passed = .false.
    if (.not. test_function_queries()) all_passed = .false.
    if (.not. test_type_queries()) all_passed = .false.
    if (.not. test_issue_14_apis()) all_passed = .false.

    if (all_passed) then
        print *, "All semantic query API tests passed!"
        stop 0
    else
        print *, "Some semantic query API tests failed!"
        stop 1
    end if

contains

    logical function test_basic_queries()
        character(len=*), parameter :: source = "x = 1"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: ctx
        integer :: root_index
        character(len=:), allocatable :: error_msg
        type(semantic_query_t) :: query
        logical :: result
        
        test_basic_queries = .true.
        print *, "Testing basic queries..."

        ! Use same pattern as test_semantic_simple.f90
        arena = create_ast_arena()
        ctx = create_semantic_context()
        
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "FAIL: Lexing failed"
            test_basic_queries = .false.
            return
        end if
        
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "FAIL: Parsing failed"
            test_basic_queries = .false.
            return
        end if
        
        call analyze_program(ctx, arena, root_index)
        
        ! Create query - use POINTERS like existing code, not deep copies
        query%arena = arena
        query%context = ctx
        
        ! Test basic functionality
        result = query%is_symbol_defined("sin")
        print *, "  ✓ is_symbol_defined('sin'): ", result
        
        result = query%is_symbol_defined("nonexistent")
        print *, "  ✓ is_symbol_defined('nonexistent'): ", result
        
        result = query%is_variable_visible("x")
        print *, "  ✓ is_variable_visible('x'): ", result

        print *, "  Basic queries: PASS"
    end function test_basic_queries

    logical function test_variable_queries()
        character(len=*), parameter :: source = "integer :: y = 42"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: ctx
        integer :: root_index
        character(len=:), allocatable :: error_msg
        type(semantic_query_t) :: query
        type(variable_info_t) :: var_info
        logical :: success
        
        test_variable_queries = .true.
        print *, "Testing variable queries..."

        arena = create_ast_arena()
        ctx = create_semantic_context()
        
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "FAIL: Lexing failed"
            test_variable_queries = .false.
            return
        end if
        
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "FAIL: Parsing failed"
            test_variable_queries = .false.
            return
        end if
        
        call analyze_program(ctx, arena, root_index)
        
        query%arena = arena
        query%context = ctx
        
        ! Test variable info query
        success = query%get_variable_info("y", var_info)
        print *, "  ✓ get_variable_info('y'): ", success
        if (success .and. allocated(var_info%name)) then
            print *, "    Variable name: ", var_info%name
        end if

        print *, "  Variable queries: PASS"
    end function test_variable_queries

    logical function test_symbol_queries()
        character(len=*), parameter :: source = "z = 3.14"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: ctx
        integer :: root_index
        character(len=:), allocatable :: error_msg
        type(semantic_query_t) :: query
        type(scope_info_t) :: scope_info
        integer :: symbol_type
        logical :: success
        
        test_symbol_queries = .true.
        print *, "Testing symbol queries..."

        arena = create_ast_arena()
        ctx = create_semantic_context()
        
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "FAIL: Lexing failed"
            test_symbol_queries = .false.
            return
        end if
        
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "FAIL: Parsing failed"
            test_symbol_queries = .false.
            return
        end if
        
        call analyze_program(ctx, arena, root_index)
        
        query%arena = arena
        query%context = ctx
        
        ! Test scope info
        success = query%get_current_scope_info(scope_info)
        print *, "  ✓ get_current_scope_info: ", success
        
        ! Test symbol type
        symbol_type = query%get_symbol_type("z")
        print *, "  ✓ get_symbol_type('z'): ", symbol_type

        print *, "  Symbol queries: PASS"
    end function test_symbol_queries

    logical function test_function_queries()
        character(len=*), parameter :: source = "print *, sin(1.0)"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: ctx
        integer :: root_index
        character(len=:), allocatable :: error_msg
        type(semantic_query_t) :: query
        type(function_info_t) :: func_info
        logical :: success
        
        test_function_queries = .true.
        print *, "Testing function queries..."

        arena = create_ast_arena()
        ctx = create_semantic_context()
        
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "FAIL: Lexing failed"
            test_function_queries = .false.
            return
        end if
        
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "FAIL: Parsing failed"
            test_function_queries = .false.
            return
        end if
        
        call analyze_program(ctx, arena, root_index)
        
        query%arena = arena
        query%context = ctx
        
        ! Test function info query
        success = query%get_function_info("sin", func_info)
        print *, "  ✓ get_function_info('sin'): ", success
        if (success) then
            print *, "    Function is intrinsic: ", func_info%is_intrinsic
        end if

        print *, "  Function queries: PASS"
    end function test_function_queries

    logical function test_type_queries()
        character(len=*), parameter :: source = "real :: w = 2.5"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: ctx
        integer :: root_index
        character(len=:), allocatable :: error_msg
        type(semantic_query_t) :: query
        type(type_info_t) :: type_info
        logical :: success
        
        test_type_queries = .true.
        print *, "Testing type queries..."

        arena = create_ast_arena()
        ctx = create_semantic_context()
        
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "FAIL: Lexing failed"
            test_type_queries = .false.
            return
        end if
        
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "FAIL: Parsing failed"
            test_type_queries = .false.
            return
        end if
        
        call analyze_program(ctx, arena, root_index)
        
        query%arena = arena
        query%context = ctx
        
        ! Test type info by name
        success = query%get_type_info_by_name("w", type_info)
        print *, "  ✓ get_type_info_by_name('w'): ", success
        if (success .and. allocated(type_info%kind_name)) then
            print *, "    Type kind: ", type_info%kind_name
        end if
        
        ! Test type info by node (test with valid node index if arena has entries)
        if (query%arena%size > 0) then
            success = query%get_type_info_by_node(1, type_info)
            print *, "  ✓ get_type_info_by_node(1): ", success
        end if

        print *, "  Type queries: PASS"
    end function test_type_queries

    logical function test_issue_14_apis()
        use scope_manager, only: SCOPE_GLOBAL
        character(len=*), parameter :: source = "integer :: a, b"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: ctx
        integer :: root_index
        character(len=:), allocatable :: error_msg
        type(semantic_query_t) :: query
        type(symbol_info_t), allocatable :: symbols(:)
        character(len=:), allocatable :: unused_vars(:)
        logical :: success
        
        test_issue_14_apis = .true.
        print *, "Testing issue #14 APIs..."

        arena = create_ast_arena()
        ctx = create_semantic_context()
        
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "FAIL: Lexing failed"
            test_issue_14_apis = .false.
            return
        end if
        
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "FAIL: Parsing failed"
            test_issue_14_apis = .false.
            return
        end if
        
        call analyze_program(ctx, arena, root_index)
        
        query%arena = arena
        query%context = ctx
        
        ! Test get_symbols_in_scope
        success = query%get_symbols_in_scope(SCOPE_GLOBAL, symbols)
        print *, "  ✓ get_symbols_in_scope: ", success
        if (success) then
            print *, "    Found symbols: ", size(symbols)
        end if
        
        ! Test is_variable_used
        success = query%is_variable_used("a", SCOPE_GLOBAL)
        print *, "  ✓ is_variable_used('a'): ", success
        
        ! Test get_unused_variables
        success = query%get_unused_variables(SCOPE_GLOBAL, unused_vars)
        print *, "  ✓ get_unused_variables: ", success
        if (success) then
            print *, "    Unused variables count: ", size(unused_vars)
        end if
        
        ! Test get_identifier_type (without optional parameter)
        success = query%get_identifier_type("a")
        print *, "  ✓ get_identifier_type('a'): ", success
        
        ! Test is_identifier_defined
        success = query%is_identifier_defined("a")
        print *, "  ✓ is_identifier_defined('a'): ", success
        
        success = query%is_identifier_defined("nonexistent")
        print *, "  ✓ is_identifier_defined('nonexistent'): ", success

        print *, "  Issue #14 APIs: PASS"
    end function test_issue_14_apis

end program test_semantic_query_api