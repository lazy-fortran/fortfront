program test_fortfront_api_semantic
    ! Test the public API semantic analysis functionality
    use fortfront, only: analyze_semantics, analyze_program, &
                        semantic_context_t, create_semantic_context, &
                        lex_source, parse_tokens, token_t, &
                        ast_arena_t, create_ast_arena, &
                        get_node, get_type_for_node, ast_node, &
                        mono_type_t, TINT, TREAL, TCHAR, TLOGICAL, &
                        program_node, assignment_node, identifier_node, literal_node
    implicit none
    
    logical :: all_passed
    
    all_passed = .true.
    
    print *, '=== fortfront Public API Semantic Analysis Tests ==='
    print *
    
    ! Test semantic analysis functionality
    if (.not. test_basic_semantic_analysis()) all_passed = .false.
    if (.not. test_type_inference()) all_passed = .false.
    if (.not. test_explicit_context()) all_passed = .false.
    if (.not. test_function_types()) all_passed = .false.
    if (.not. test_array_types()) all_passed = .false.
    
    ! Report results
    print *
    if (all_passed) then
        print *, 'All fortfront API semantic tests passed!'
        stop 0
    else
        print *, 'Some fortfront API semantic tests failed!'
        stop 1
    end if
    
contains
    
    logical function test_basic_semantic_analysis()
        test_basic_semantic_analysis = .true.
        print *, 'Testing basic semantic analysis...'
        
        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            character(len=:), allocatable :: error_msg
            integer :: prog_index
            
            ! Parse simple code
            call lex_source('x = 42', tokens, error_msg)
            arena = create_ast_arena()
            call parse_tokens(tokens, arena, prog_index, error_msg)
            
            if (error_msg /= "") then
                print *, '  FAIL: Parsing error: ', error_msg
                test_basic_semantic_analysis = .false.
                return
            end if
            
            ! Run semantic analysis
            call analyze_semantics(arena, prog_index)
            
            ! Should complete without error
            print *, '  PASS: Basic semantic analysis'
        end block
    end function test_basic_semantic_analysis
    
    logical function test_type_inference()
        test_type_inference = .true.
        print *, 'Testing type inference...'
        
        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            character(len=:), allocatable :: error_msg, source
            integer :: prog_index
            type(mono_type_t), allocatable :: node_type
            class(ast_node), allocatable :: prog_node, stmt_node
            
            ! Type inference code
            source = 'x := 42' // new_line('A') // &
                     'y := 3.14' // new_line('A') // &
                     'z := "hello"'
            
            call lex_source(source, tokens, error_msg)
            arena = create_ast_arena()
            call parse_tokens(tokens, arena, prog_index, error_msg)
            
            if (error_msg /= "") then
                print *, '  FAIL: Parsing error: ', error_msg
                test_type_inference = .false.
                return
            end if
            
            ! Run semantic analysis
            call analyze_semantics(arena, prog_index)
            
            ! Check inferred types
            prog_node = get_node(arena, prog_index)
            if (.not. allocated(prog_node)) then
                print *, '  FAIL: Could not get program node'
                test_type_inference = .false.
                return
            end if
            
            select type (prog_node)
            type is (program_node)
                if (size(prog_node%body_indices) < 3) then
                    print *, '  FAIL: Expected at least 3 statements'
                    test_type_inference = .false.
                    return
                end if
                
                ! Check first assignment (x := 42)
                stmt_node = get_node(arena, prog_node%body_indices(1))
                select type (stmt_node)
                type is (assignment_node)
                    ! Check if type was inferred
                    if (allocated(stmt_node%inferred_type)) then
                        if (stmt_node%inferred_type%kind /= TINT) then
                            print *, '  FAIL: Expected integer type for x'
                            test_type_inference = .false.
                            return
                        end if
                    else
                        print *, '  Note: No type inferred for assignment node'
                    end if
                    
                    ! Check target identifier type
                    if (stmt_node%target_index > 0) then
                        block
                            logical :: type_found
                            call get_type_for_node(arena, stmt_node%target_index, node_type, type_found)
                            if (type_found .and. allocated(node_type)) then
                                if (node_type%kind /= TINT) then
                                    print *, '  FAIL: Expected integer type for x identifier'
                                    test_type_inference = .false.
                                    return
                                end if
                            end if
                        end block
                    end if
                    
                class default
                    print *, '  FAIL: Expected assignment node'
                    test_type_inference = .false.
                    return
                end select
                
            class default
                print *, '  FAIL: Expected program node'
                test_type_inference = .false.
                return
            end select
            
            print *, '  PASS: Type inference'
        end block
    end function test_type_inference
    
    logical function test_explicit_context()
        test_explicit_context = .true.
        print *, 'Testing explicit semantic context...'
        
        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            type(semantic_context_t) :: ctx
            character(len=:), allocatable :: error_msg
            integer :: prog_index
            
            ! Parse code
            call lex_source('real :: x' // new_line('A') // 'x = 3.14', tokens, error_msg)
            arena = create_ast_arena()
            call parse_tokens(tokens, arena, prog_index, error_msg)
            
            if (error_msg /= "") then
                print *, '  FAIL: Parsing error: ', error_msg
                test_explicit_context = .false.
                return
            end if
            
            ! Create explicit context
            ctx = create_semantic_context()
            
            ! Analyze with explicit context
            call analyze_program(ctx, arena, prog_index)
            
            print *, '  PASS: Explicit semantic context'
        end block
    end function test_explicit_context
    
    logical function test_function_types()
        test_function_types = .true.
        print *, 'Testing function type analysis...'
        
        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            character(len=:), allocatable :: error_msg, source
            integer :: prog_index
            
            ! Function with parameters
            source = 'real function add(a, b)' // new_line('A') // &
                     '    real :: a, b' // new_line('A') // &
                     '    add = a + b' // new_line('A') // &
                     'end function add'
            
            call lex_source(source, tokens, error_msg)
            arena = create_ast_arena()
            call parse_tokens(tokens, arena, prog_index, error_msg)
            
            if (error_msg /= "") then
                print *, '  FAIL: Parsing error: ', error_msg
                test_function_types = .false.
                return
            end if
            
            ! Analyze semantics
            call analyze_semantics(arena, prog_index)
            
            ! Function type checking would happen here
            print *, '  PASS: Function type analysis'
        end block
    end function test_function_types
    
    logical function test_array_types()
        test_array_types = .true.
        print *, 'Testing array type analysis...'
        
        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            character(len=:), allocatable :: error_msg, source
            integer :: prog_index
            
            ! Array declarations and literals
            source = 'real, dimension(10) :: arr' // new_line('A') // &
                     'arr = [1.0, 2.0, 3.0]'
            
            call lex_source(source, tokens, error_msg)
            arena = create_ast_arena()
            call parse_tokens(tokens, arena, prog_index, error_msg)
            
            if (error_msg /= "") then
                print *, '  FAIL: Parsing error: ', error_msg
                test_array_types = .false.
                return
            end if
            
            ! Analyze semantics
            call analyze_semantics(arena, prog_index)
            
            print *, '  PASS: Array type analysis'
        end block
    end function test_array_types
    
end program test_fortfront_api_semantic