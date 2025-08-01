program test_pointer_propagation
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

    print *, "Running pointer attribute propagation tests..."
    
    if (.not. test_pointer_variable_declaration()) all_passed = .false.
    if (.not. test_pointer_assignment_vs_association()) all_passed = .false.
    if (.not. test_pointer_binary_operation()) all_passed = .false.
    if (.not. test_mixed_pointer_allocatable()) all_passed = .false.
    if (.not. test_pointer_function_result()) all_passed = .false.
    if (.not. test_target_attribute()) all_passed = .false.

    if (all_passed) then
        print *, "All pointer propagation tests passed"
        stop 0
    else
        print *, "Some pointer propagation tests failed"
        stop 1
    end if

contains

    logical function test_pointer_variable_declaration()
        character(len=*), parameter :: source = "real, pointer :: p(:)"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index, i
        type(semantic_context_t) :: sem_ctx
        character(len=:), allocatable :: error_msg
        logical :: found_pointer
        
        test_pointer_variable_declaration = .true.
        print *, "  Testing pointer variable declaration..."
        
        ! Tokenize
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "    FAIL: Tokenization failed: ", error_msg
                test_pointer_variable_declaration = .false.
                return
            end if
        end if
        
        ! Parse
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "    FAIL: Parsing failed: ", error_msg
                test_pointer_variable_declaration = .false.
                return
            end if
        end if
        
        ! Check for pointer attribute
        found_pointer = .false.
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                select type(node => arena%entries(i)%node)
                type is (declaration_node)
                    if (node%var_name == "p" .and. node%is_pointer) then
                        found_pointer = .true.
                        print *, "    PASS: Declaration has pointer attribute"
                    end if
                end select
            end if
        end do
        
        if (.not. found_pointer) then
            print *, "    FAIL: Pointer attribute not found in declaration"
            test_pointer_variable_declaration = .false.
        end if
        
    end function test_pointer_variable_declaration

    logical function test_pointer_assignment_vs_association()
        character(len=*), parameter :: source = &
            "program test" // new_line('a') // &
            "real, pointer :: p(:)" // new_line('a') // &
            "real, target :: t(10)" // new_line('a') // &
            "real :: values(10)" // new_line('a') // &
            "p => t" // new_line('a') // &  ! Pointer association
            "p = values" // new_line('a') // &  ! Value assignment
            "end program test"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        type(semantic_context_t) :: sem_ctx
        character(len=:), allocatable :: error_msg
        
        test_pointer_assignment_vs_association = .true.
        print *, "  Testing pointer assignment vs association..."
        
        ! Tokenize
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "    FAIL: Tokenization failed: ", error_msg
                test_pointer_assignment_vs_association = .false.
                return
            end if
        end if
        
        ! Parse
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "    FAIL: Parsing failed: ", error_msg
                test_pointer_assignment_vs_association = .false.
                return
            end if
        end if
        
        ! Analyze semantics
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, prog_index)
        
        ! TODO: Check distinction between pointer association and value assignment
        print *, "    TODO: Pointer assignment vs association not yet implemented"
        
    end function test_pointer_assignment_vs_association

    logical function test_pointer_binary_operation()
        character(len=*), parameter :: source = &
            "program test" // new_line('a') // &
            "real, pointer :: p(:), q(:)" // new_line('a') // &
            "real :: r(10)" // new_line('a') // &
            "r = p + q" // new_line('a') // &
            "end program test"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        type(semantic_context_t) :: sem_ctx
        character(len=:), allocatable :: error_msg
        
        test_pointer_binary_operation = .true.
        print *, "  Testing pointer binary operation..."
        
        ! Tokenize
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "    FAIL: Tokenization failed: ", error_msg
                test_pointer_binary_operation = .false.
                return
            end if
        end if
        
        ! Parse
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "    FAIL: Parsing failed: ", error_msg
                test_pointer_binary_operation = .false.
                return
            end if
        end if
        
        ! Analyze semantics
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, prog_index)
        
        ! TODO: Check pointer tracking in expressions
        print *, "    TODO: Pointer tracking in expressions not yet implemented"
        
    end function test_pointer_binary_operation

    logical function test_mixed_pointer_allocatable()
        character(len=*), parameter :: source = &
            "program test" // new_line('a') // &
            "real, pointer :: p(:)" // new_line('a') // &
            "real, allocatable :: a(:)" // new_line('a') // &
            "real :: c(10)" // new_line('a') // &
            "c = p + a" // new_line('a') // &
            "end program test"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        type(semantic_context_t) :: sem_ctx
        character(len=:), allocatable :: error_msg
        
        test_mixed_pointer_allocatable = .true.
        print *, "  Testing mixed pointer/allocatable operations..."
        
        ! Tokenize
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "    FAIL: Tokenization failed: ", error_msg
                test_mixed_pointer_allocatable = .false.
                return
            end if
        end if
        
        ! Parse
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "    FAIL: Parsing failed: ", error_msg
                test_mixed_pointer_allocatable = .false.
                return
            end if
        end if
        
        ! Analyze semantics
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, prog_index)
        
        ! TODO: Check mixed pointer/allocatable operations
        print *, "    TODO: Mixed pointer/allocatable tracking not yet implemented"
        
    end function test_mixed_pointer_allocatable

    logical function test_pointer_function_result()
        character(len=*), parameter :: source = &
            "program test" // new_line('a') // &
            "contains" // new_line('a') // &
            "function get_ptr() result(ptr)" // new_line('a') // &
            "real, pointer :: ptr(:)" // new_line('a') // &
            "allocate(ptr(10))" // new_line('a') // &
            "end function get_ptr" // new_line('a') // &
            "end program test"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        type(semantic_context_t) :: sem_ctx
        character(len=:), allocatable :: error_msg
        
        test_pointer_function_result = .true.
        print *, "  Testing pointer function result..."
        
        ! Tokenize
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "    FAIL: Tokenization failed: ", error_msg
                test_pointer_function_result = .false.
                return
            end if
        end if
        
        ! Parse
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "    FAIL: Parsing failed: ", error_msg
                test_pointer_function_result = .false.
                return
            end if
        end if
        
        ! Analyze semantics
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, prog_index)
        
        ! TODO: Check function result pointer attribute
        print *, "    TODO: Function result pointer tracking not yet implemented"
        
    end function test_pointer_function_result

    logical function test_target_attribute()
        character(len=*), parameter :: source = &
            "program test" // new_line('a') // &
            "real, target :: t(10)" // new_line('a') // &
            "real, pointer :: p(:)" // new_line('a') // &
            "p => t" // new_line('a') // &
            "end program test"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index, i
        type(semantic_context_t) :: sem_ctx
        character(len=:), allocatable :: error_msg
        logical :: found_target
        
        test_target_attribute = .true.
        print *, "  Testing target attribute..."
        
        ! Tokenize
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "    FAIL: Tokenization failed: ", error_msg
                test_target_attribute = .false.
                return
            end if
        end if
        
        ! Parse
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "    FAIL: Parsing failed: ", error_msg
                test_target_attribute = .false.
                return
            end if
        end if
        
        ! Check for target attribute
        found_target = .false.
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                select type(node => arena%entries(i)%node)
                type is (declaration_node)
                    if (node%var_name == "t" .and. node%is_target) then
                        found_target = .true.
                        print *, "    PASS: Declaration has target attribute"
                    end if
                end select
            end if
        end do
        
        if (.not. found_target) then
            print *, "    FAIL: Target attribute not found in declaration"
            test_target_attribute = .false.
        end if
        
    end function test_target_attribute

end program test_pointer_propagation