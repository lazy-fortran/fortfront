program test_allocatable_propagation
    use frontend
    use ast_core
    use ast_nodes_data, only: declaration_node
    use ast_arena
    use lexer_core, only: token_t
    use semantic_analyzer, only: semantic_context_t, create_semantic_context, analyze_program
    use type_system_hm, only: allocation_info_t
    implicit none

    logical :: all_passed
    all_passed = .true.

    print *, "Running allocatable attribute propagation tests..."
    
    if (.not. test_allocatable_variable_declaration()) all_passed = .false.
    if (.not. test_allocatable_binary_operation()) all_passed = .false.
    if (.not. test_allocatable_assignment()) all_passed = .false.
    if (.not. test_mixed_allocatable_static()) all_passed = .false.
    if (.not. test_allocatable_function_result()) all_passed = .false.
    if (.not. test_allocatable_array_slice()) all_passed = .false.

    if (all_passed) then
        print *, "All allocatable propagation tests passed"
        stop 0
    else
        print *, "Some allocatable propagation tests failed"
        stop 1
    end if

contains

    logical function test_allocatable_variable_declaration()
        character(len=*), parameter :: source = "real, allocatable :: a(:)"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index, i
        type(semantic_context_t) :: sem_ctx
        character(len=:), allocatable :: error_msg
        logical :: found_allocatable
        
        test_allocatable_variable_declaration = .true.
        print *, "  Testing allocatable variable declaration..."
        
        ! Tokenize
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "    FAIL: Tokenization failed: ", error_msg
                test_allocatable_variable_declaration = .false.
                return
            end if
        end if
        
        ! Parse
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "    FAIL: Parsing failed: ", error_msg
                test_allocatable_variable_declaration = .false.
                return
            end if
        end if
        
        ! Check for allocatable attribute
        found_allocatable = .false.
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                select type(node => arena%entries(i)%node)
                type is (declaration_node)
                    if (node%var_name == "a" .and. node%is_allocatable) then
                        found_allocatable = .true.
                        print *, "    PASS: Declaration has allocatable attribute"
                    end if
                end select
            end if
        end do
        
        if (.not. found_allocatable) then
            print *, "    FAIL: Allocatable attribute not found in declaration"
            test_allocatable_variable_declaration = .false.
        end if
        
    end function test_allocatable_variable_declaration

    logical function test_allocatable_binary_operation()
        character(len=*), parameter :: source = &
            "program test" // new_line('a') // &
            "real, allocatable :: a(:), b(:), c(:)" // new_line('a') // &
            "c = a + b" // new_line('a') // &
            "end program test"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        type(semantic_context_t) :: sem_ctx
        character(len=:), allocatable :: error_msg
        
        test_allocatable_binary_operation = .true.
        print *, "  Testing allocatable binary operation..."
        
        ! Tokenize
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "    FAIL: Tokenization failed: ", error_msg
                test_allocatable_binary_operation = .false.
                return
            end if
        end if
        
        ! Parse
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "    FAIL: Parsing failed: ", error_msg
                test_allocatable_binary_operation = .false.
                return
            end if
        end if
        
        ! Analyze semantics
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, prog_index)
        
        ! TODO: Check that binary operation tracks allocatable operands
        print *, "    TODO: Binary operation allocatable tracking not yet implemented"
        
    end function test_allocatable_binary_operation

    logical function test_allocatable_assignment()
        character(len=*), parameter :: source = &
            "program test" // new_line('a') // &
            "real, allocatable :: a(:), b(:)" // new_line('a') // &
            "a = b" // new_line('a') // &
            "end program test"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        type(semantic_context_t) :: sem_ctx
        character(len=:), allocatable :: error_msg
        
        test_allocatable_assignment = .true.
        print *, "  Testing allocatable assignment..."
        
        ! Tokenize
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "    FAIL: Tokenization failed: ", error_msg
                test_allocatable_assignment = .false.
                return
            end if
        end if
        
        ! Parse
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "    FAIL: Parsing failed: ", error_msg
                test_allocatable_assignment = .false.
                return
            end if
        end if
        
        ! Analyze semantics
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, prog_index)
        
        ! TODO: Check that assignment tracks allocatable attributes
        print *, "    TODO: Assignment allocatable tracking not yet implemented"
        
    end function test_allocatable_assignment

    logical function test_mixed_allocatable_static()
        character(len=*), parameter :: source = &
            "program test" // new_line('a') // &
            "real, allocatable :: a(:)" // new_line('a') // &
            "real :: b(100), c(:)" // new_line('a') // &
            "allocatable :: c" // new_line('a') // &
            "c = a + b" // new_line('a') // &
            "end program test"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        type(semantic_context_t) :: sem_ctx
        character(len=:), allocatable :: error_msg
        
        test_mixed_allocatable_static = .true.
        print *, "  Testing mixed allocatable/static operations..."
        
        ! Tokenize
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "    FAIL: Tokenization failed: ", error_msg
                test_mixed_allocatable_static = .false.
                return
            end if
        end if
        
        ! Parse
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "    FAIL: Parsing failed: ", error_msg
                test_mixed_allocatable_static = .false.
                return
            end if
        end if
        
        ! Analyze semantics
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, prog_index)
        
        ! TODO: Check mixed operations
        print *, "    TODO: Mixed allocatable/static tracking not yet implemented"
        
    end function test_mixed_allocatable_static

    logical function test_allocatable_function_result()
        character(len=*), parameter :: source = &
            "program test" // new_line('a') // &
            "contains" // new_line('a') // &
            "function get_array() result(arr)" // new_line('a') // &
            "real, allocatable :: arr(:)" // new_line('a') // &
            "allocate(arr(10))" // new_line('a') // &
            "end function get_array" // new_line('a') // &
            "end program test"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        type(semantic_context_t) :: sem_ctx
        character(len=:), allocatable :: error_msg
        
        test_allocatable_function_result = .true.
        print *, "  Testing allocatable function result..."
        
        ! Tokenize
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "    FAIL: Tokenization failed: ", error_msg
                test_allocatable_function_result = .false.
                return
            end if
        end if
        
        ! Parse
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "    FAIL: Parsing failed: ", error_msg
                test_allocatable_function_result = .false.
                return
            end if
        end if
        
        ! Analyze semantics
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, prog_index)
        
        ! TODO: Check function result allocatable attribute
        print *, "    TODO: Function result allocatable tracking not yet implemented"
        
    end function test_allocatable_function_result

    logical function test_allocatable_array_slice()
        character(len=*), parameter :: source = &
            "program test" // new_line('a') // &
            "real, allocatable :: a(:), b(:)" // new_line('a') // &
            "b = a(1:5)" // new_line('a') // &
            "end program test"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        type(semantic_context_t) :: sem_ctx
        character(len=:), allocatable :: error_msg
        
        test_allocatable_array_slice = .true.
        print *, "  Testing allocatable array slice..."
        
        ! Tokenize
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "    FAIL: Tokenization failed: ", error_msg
                test_allocatable_array_slice = .false.
                return
            end if
        end if
        
        ! Parse
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "    FAIL: Parsing failed: ", error_msg
                test_allocatable_array_slice = .false.
                return
            end if
        end if
        
        ! Analyze semantics
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, prog_index)
        
        ! TODO: Check array slice allocatable propagation
        print *, "    TODO: Array slice allocatable tracking not yet implemented"
        
    end function test_allocatable_array_slice

end program test_allocatable_propagation