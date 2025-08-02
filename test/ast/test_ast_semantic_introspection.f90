program test_ast_semantic_introspection
    use fortfront
    use semantic_analyzer, only: semantic_context_t, create_semantic_context
    use type_system_hm, only: TINT, TREAL, TCHAR, TLOGICAL
    implicit none

    logical :: all_passed = .true.

    print *, "Testing AST Semantic Introspection Integration..."

    if (.not. test_semantic_analysis_integration()) all_passed = .false.

    if (all_passed) then
        print *, "All AST semantic introspection tests passed!"
        stop 0
    else
        print *, "Some AST semantic introspection tests failed!"
        stop 1
    end if

contains

    ! Helper function to find assignment node and get type details
    subroutine find_assignment_and_get_type_info(arena, type_kind, type_size, &
                                                is_allocatable, is_pointer, found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(out) :: type_kind, type_size
        logical, intent(out) :: is_allocatable, is_pointer, found
        integer :: i, assignment_index
        
        found = .false.
        assignment_index = 0
        
        ! Find assignment node
        do i = 1, arena%size
            if (get_node_type_at(arena, i) == "assignment") then
                assignment_index = i
                exit
            end if
        end do
        
        if (assignment_index > 0) then
            print *, "    SUCCESS: Found assignment node at index: ", assignment_index
            call get_node_type_details(arena, assignment_index, type_kind, type_size, &
                                     is_allocatable, is_pointer, found)
        else
            print *, "    FAIL: No assignment node found"
        end if
    end subroutine find_assignment_and_get_type_info

    ! Test semantic analysis integration with introspection APIs
    logical function test_semantic_analysis_integration()
        test_semantic_analysis_integration = .true.
        print *, "Testing semantic analysis integration..."

        ! Test multiple different type assignments to verify type system works
        if (.not. test_integer_type_inference()) then
            test_semantic_analysis_integration = .false.
        end if
        
        if (.not. test_real_type_inference()) then
            test_semantic_analysis_integration = .false.
        end if
        
        if (.not. test_character_type_inference()) then
            test_semantic_analysis_integration = .false.
        end if
        
        if (.not. test_logical_type_inference()) then
            test_semantic_analysis_integration = .false.
        end if

        print *, "  Semantic analysis integration: ", &
                 merge("PASS", "FAIL", test_semantic_analysis_integration)
    end function test_semantic_analysis_integration

    ! Test integer type inference with introspection APIs
    logical function test_integer_type_inference()
        character(len=*), parameter :: source = "x = 42"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: sem_ctx
        integer :: root_index
        character(len=:), allocatable :: error_msg
        integer :: type_kind, type_size
        logical :: is_allocatable, is_pointer, found
        
        test_integer_type_inference = .true.
        print *, "  Testing integer type inference..."

        ! Create AST
        arena = create_ast_arena()
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "    FAIL: Lexing failed: ", error_msg
            test_integer_type_inference = .false.
            return
        end if
        
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "    FAIL: Parsing failed: ", error_msg
            test_integer_type_inference = .false.
            return
        end if

        ! Run semantic analysis
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, root_index)

        
        ! Find assignment node and get type info
        call find_assignment_and_get_type_info(arena, type_kind, type_size, &
                                              is_allocatable, is_pointer, found)
        
        if (.not. found) then
            test_integer_type_inference = .false.
            return
        end if
        
        print *, "    SUCCESS: Type details found"
        print *, "      kind=", type_kind, " size=", type_size
        print *, "      allocatable=", is_allocatable, " pointer=", is_pointer
        
        ! Verify this is an integer type
        if (type_kind == TINT) then
            print *, "    PASS: Correctly identified as integer type"
        else
            print *, "    FAIL: Expected TINT (", TINT, "), got ", type_kind
            test_integer_type_inference = .false.
        end if
    end function test_integer_type_inference

    ! Test real type inference with introspection APIs
    logical function test_real_type_inference()
        character(len=*), parameter :: source = "pi = 3.14"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: sem_ctx
        integer :: root_index
        character(len=:), allocatable :: error_msg
        integer :: type_kind, type_size
        logical :: is_allocatable, is_pointer, found
        
        test_real_type_inference = .true.
        print *, "  Testing real type inference..."

        ! Create AST
        arena = create_ast_arena()
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "    FAIL: Lexing failed: ", error_msg
            test_real_type_inference = .false.
            return
        end if
        
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "    FAIL: Parsing failed: ", error_msg
            test_real_type_inference = .false.
            return
        end if

        ! Run semantic analysis
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, root_index)

        ! Find assignment node and get type info
        call find_assignment_and_get_type_info(arena, type_kind, type_size, &
                                              is_allocatable, is_pointer, found)
        
        if (.not. found) then
            test_real_type_inference = .false.
            return
        end if
        
        print *, "    SUCCESS: Type details found"
        print *, "      kind=", type_kind, " size=", type_size
        
        ! Verify this is a real type
        if (type_kind == TREAL) then
            print *, "    PASS: Correctly identified as real type"
        else
            print *, "    FAIL: Expected TREAL (", TREAL, "), got ", type_kind
            test_real_type_inference = .false.
        end if
    end function test_real_type_inference

    ! Test character type inference with introspection APIs
    logical function test_character_type_inference()
        character(len=*), parameter :: source = 'name = "hello"'
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: sem_ctx
        integer :: root_index
        character(len=:), allocatable :: error_msg
        integer :: type_kind, type_size
        logical :: is_allocatable, is_pointer, found
        
        test_character_type_inference = .true.
        print *, "  Testing character type inference..."

        ! Create AST
        arena = create_ast_arena()
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "    FAIL: Lexing failed: ", error_msg
            test_character_type_inference = .false.
            return
        end if
        
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "    FAIL: Parsing failed: ", error_msg
            test_character_type_inference = .false.
            return
        end if

        ! Run semantic analysis
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, root_index)

        ! Find assignment node and get type info
        call find_assignment_and_get_type_info(arena, type_kind, type_size, &
                                              is_allocatable, is_pointer, found)
        
        if (.not. found) then
            test_character_type_inference = .false.
            return
        end if
        
        print *, "    SUCCESS: Type details found"
        print *, "      kind=", type_kind, " size=", type_size
        
        ! Verify this is a character type
        if (type_kind == TCHAR) then
            print *, "    PASS: Correctly identified as character type"
            if (type_size == 5) then  ! "hello" is 5 characters
                print *, "    PASS: Correctly identified string length: ", type_size
            else
                print *, "    INFO: String length: ", type_size, " (expected 5)"
            end if
        else
            print *, "    FAIL: Expected TCHAR (", TCHAR, "), got ", type_kind
            test_character_type_inference = .false.
        end if
    end function test_character_type_inference

    ! Test logical type inference with introspection APIs
    logical function test_logical_type_inference()
        character(len=*), parameter :: source = "flag = .true."
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: sem_ctx
        integer :: root_index
        character(len=:), allocatable :: error_msg
        integer :: type_kind, type_size
        logical :: is_allocatable, is_pointer, found
        
        test_logical_type_inference = .true.
        print *, "  Testing logical type inference..."

        ! Create AST
        arena = create_ast_arena()
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "    FAIL: Lexing failed: ", error_msg
            test_logical_type_inference = .false.
            return
        end if
        
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "    FAIL: Parsing failed: ", error_msg
            test_logical_type_inference = .false.
            return
        end if

        ! Run semantic analysis
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, root_index)

        ! Find assignment node and get type info
        call find_assignment_and_get_type_info(arena, type_kind, type_size, &
                                              is_allocatable, is_pointer, found)
        
        if (.not. found) then
            test_logical_type_inference = .false.
            return
        end if
        
        print *, "    SUCCESS: Type details found"
        print *, "      kind=", type_kind, " size=", type_size
        
        ! Verify this is a logical type
        if (type_kind == TLOGICAL) then
            print *, "    PASS: Correctly identified as logical type"
        else
            print *, "    FAIL: Expected TLOGICAL (", TLOGICAL, "), got ", type_kind
            test_logical_type_inference = .false.
        end if
    end function test_logical_type_inference

end program test_ast_semantic_introspection