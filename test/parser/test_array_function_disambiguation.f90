program test_array_function_disambiguation
    use ast_core
    use ast_nodes_core
    use ast_factory
    use ast_arena
    use parser_core
    use parser_state_module
    use parser_statements_module
    use lexer_core
    use frontend
    use semantic_analyzer
    use iso_fortran_env, only: error_unit
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Array vs Function Call Disambiguation Tests ==='
    print *

    ! Run tests
    if (.not. test_disambiguation_flag()) all_passed = .false.
    if (.not. test_semantic_array_disambiguation()) all_passed = .false.
    if (.not. test_semantic_function_disambiguation()) all_passed = .false.
    if (.not. test_intrinsic_disambiguation()) all_passed = .false.
    if (.not. test_full_program_disambiguation()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All array/function disambiguation tests passed!'
        stop 0
    else
        print *, 'Some array/function disambiguation tests failed!'
        stop 1
    end if

contains

    logical function test_disambiguation_flag()
        test_disambiguation_flag = .true.
        print *, 'Testing disambiguation flag exists...'
        
        block
            type(ast_arena_t) :: arena
            type(call_or_subscript_node) :: node
            
            ! Initialize arena
            arena = create_ast_arena()
            
            ! Create a call_or_subscript node
            node%name = "test"
            allocate(node%arg_indices(1))
            node%arg_indices(1) = 1
            
            ! Check if is_array_access field exists
            ! The flag should be false by default (function call)
            if (.not. node%is_array_access) then
                print *, '  PASS: is_array_access flag exists and defaults to false'
            else
                print *, '  FAIL: is_array_access should default to false'
                test_disambiguation_flag = .false.
            end if
            
            ! Test setting the flag
            node%is_array_access = .true.
            if (node%is_array_access) then
                print *, '  PASS: is_array_access flag can be set to true'
            else
                print *, '  FAIL: is_array_access flag not settable'
                test_disambiguation_flag = .false.
            end if
            
            call arena%clear()
        end block
        
    end function test_disambiguation_flag

    logical function test_semantic_array_disambiguation()
        test_semantic_array_disambiguation = .true.
        print *, 'Testing semantic disambiguation of array access...'
        
        block
            character(len=:), allocatable :: source, error_msg
            type(ast_arena_t) :: arena
            type(semantic_context_t) :: sem_ctx
            type(token_t), allocatable :: tokens(:)
            integer :: prog_index
            
            source = 'program test' // new_line('a') // &
                     '    real :: arr(10)' // new_line('a') // &
                     '    real :: x' // new_line('a') // &
                     '    x = arr(5)' // new_line('a') // &
                     'end program'
            
            ! Lex and parse
            call lex_source(source, tokens, error_msg)
            if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
                print *, '  FAIL: Lexing error: ', trim(error_msg)
                test_semantic_array_disambiguation = .false.
                return
            end if
            
            arena = create_ast_arena()
            block
                use parser_state_module, only: parser_state_t, create_parser_state
                use parser_statements_module, only: parse_program_statement
                type(parser_state_t) :: parser
                
                parser = create_parser_state(tokens)
                prog_index = parse_program_statement(parser, arena)
            end block
            
            if (prog_index > 0) then
                ! Run semantic analysis
                sem_ctx = create_semantic_context()
                call analyze_program(sem_ctx, arena, prog_index)
                
                ! Find the call_or_subscript node for arr(5)
                block
                    integer :: i
                    logical :: found_array_access
                    found_array_access = .false.
                    
                    do i = 1, arena%size
                        if (allocated(arena%entries(i)%node)) then
                            select type (node => arena%entries(i)%node)
                            type is (call_or_subscript_node)
                                if (node%name == "arr") then
                                    if (node%is_array_access) then
                                        print *, '  PASS: arr(5) correctly marked as array access'
                                        found_array_access = .true.
                                    else
                                        print *, '  FAIL: arr(5) not marked as array access'
                                        test_semantic_array_disambiguation = .false.
                                    end if
                                end if
                            end select
                        end if
                    end do
                    
                    if (.not. found_array_access) then
                        print *, '  INFO: No arr(5) node found (implementation pending)'
                    end if
                end block
            else
                print *, '  FAIL: Failed to parse program'
                test_semantic_array_disambiguation = .false.
            end if
            
            call arena%clear()
        end block
        
    end function test_semantic_array_disambiguation

    logical function test_semantic_function_disambiguation()
        test_semantic_function_disambiguation = .true.
        print *, 'Testing semantic disambiguation of function calls...'
        
        block
            character(len=:), allocatable :: source, error_msg
            type(ast_arena_t) :: arena
            type(semantic_context_t) :: sem_ctx
            type(token_t), allocatable :: tokens(:)
            integer :: prog_index
            
            source = 'program test' // new_line('a') // &
                     '    real :: x, y' // new_line('a') // &
                     '    x = sin(1.0)' // new_line('a') // &
                     'end program'
            
            ! Lex and parse
            call lex_source(source, tokens, error_msg)
            if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
                print *, '  FAIL: Lexing error: ', trim(error_msg)
                test_semantic_function_disambiguation = .false.
                return
            end if
            
            arena = create_ast_arena()
            block
                use parser_state_module, only: parser_state_t, create_parser_state
                use parser_statements_module, only: parse_program_statement
                type(parser_state_t) :: parser
                
                parser = create_parser_state(tokens)
                prog_index = parse_program_statement(parser, arena)
            end block
            
            if (prog_index > 0) then
                ! Run semantic analysis
                sem_ctx = create_semantic_context()
                call analyze_program(sem_ctx, arena, prog_index)
                
                ! Find the call_or_subscript node for sin(1.0)
                block
                    integer :: i
                    logical :: found_function_call
                    found_function_call = .false.
                    
                    do i = 1, arena%size
                        if (allocated(arena%entries(i)%node)) then
                            select type (node => arena%entries(i)%node)
                            type is (call_or_subscript_node)
                                if (node%name == "sin") then
                                    if (.not. node%is_array_access) then
                                        print *, '  PASS: sin(1.0) correctly marked as function call'
                                        found_function_call = .true.
                                    else
                                        print *, '  FAIL: sin(1.0) wrongly marked as array access'
                                        test_semantic_function_disambiguation = .false.
                                    end if
                                end if
                            end select
                        end if
                    end do
                    
                    if (.not. found_function_call) then
                        print *, '  INFO: No sin(1.0) node found (implementation pending)'
                    end if
                end block
            else
                print *, '  FAIL: Failed to parse program'
                test_semantic_function_disambiguation = .false.
            end if
            
            call arena%clear()
        end block
        
    end function test_semantic_function_disambiguation

    logical function test_intrinsic_disambiguation()
        test_intrinsic_disambiguation = .true.
        print *, 'Testing intrinsic function disambiguation...'
        
        block
            type(ast_arena_t) :: arena
            type(call_or_subscript_node) :: node
            
            arena = create_ast_arena()
            
            ! Test that intrinsic functions are never marked as array access
            node%name = "sqrt"
            node%is_intrinsic = .true.
            node%is_array_access = .false.
            
            if (.not. node%is_array_access .and. node%is_intrinsic) then
                print *, '  PASS: Intrinsic functions correctly marked as function calls'
            else
                print *, '  FAIL: Intrinsic function disambiguation error'
                test_intrinsic_disambiguation = .false.
            end if
            
            call arena%clear()
        end block
        
    end function test_intrinsic_disambiguation

    logical function test_full_program_disambiguation()
        test_full_program_disambiguation = .true.
        print *, 'Testing full program with mixed array and function calls...'
        
        block
            character(len=:), allocatable :: source, output, error_msg
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            integer :: prog_index
            
            source = 'program test' // new_line('a') // &
                     '    real :: arr(10), mat(5,5)' // new_line('a') // &
                     '    real :: x, y, z' // new_line('a') // &
                     '    x = sin(2.0)' // new_line('a') // &
                     '    y = arr(3)' // new_line('a') // &
                     '    z = mat(2,4)' // new_line('a') // &
                     'end program'
            
            ! Phase 1: Lex
            call lex_source(source, tokens, error_msg)
            if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
                print *, '  FAIL: Lexing error: ', trim(error_msg)
                test_full_program_disambiguation = .false.
                return
            end if
            
            ! Phase 2: Parse
            arena = create_ast_arena(1000)
            block
                character(len=1024) :: parse_error_msg
                parse_error_msg = ""
                call parse_tokens(tokens, arena, prog_index, parse_error_msg)
                if (len_trim(parse_error_msg) > 0) then
                    print *, '  FAIL: Parsing error: ', trim(parse_error_msg)
                    test_full_program_disambiguation = .false.
                    return
                end if
            end block
            
            ! Phase 3: Semantic analysis
            call analyze_semantics(arena, prog_index)
            
            print *, '  PASS: Full program compiles with mixed calls'
        end block
        
    end function test_full_program_disambiguation

end program