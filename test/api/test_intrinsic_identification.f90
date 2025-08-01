program test_intrinsic_identification
    use fortfront
    use ast_core
    use intrinsic_registry, only: registry_is_intrinsic => is_intrinsic_function, &
                                  registry_get_signature => get_intrinsic_signature
    implicit none
    
    logical :: all_tests_passed
    
    print *, "=== Intrinsic Function Identification Tests ==="
    all_tests_passed = .true.
    
    ! Test 1: Registry initialization and lookup
    if (.not. test_registry_functions()) all_tests_passed = .false.
    
    ! Test 2: Intrinsic functions in AST
    if (.not. test_ast_intrinsic_identification()) all_tests_passed = .false.
    
    ! Test 3: Non-intrinsic functions in AST  
    if (.not. test_ast_user_function()) all_tests_passed = .false.
    
    ! Test 4: Multiple intrinsic functions
    if (.not. test_multiple_intrinsics()) all_tests_passed = .false.
    
    if (all_tests_passed) then
        print *, "All intrinsic identification tests passed!"
    else
        print *, "Some intrinsic identification tests failed!"
        stop 1
    end if
    
contains
    
    logical function test_registry_functions()
        logical :: result
        character(len=:), allocatable :: signature
        
        test_registry_functions = .true.
        print *, "Testing intrinsic registry functions..."
        
        ! Test known intrinsic functions
        result = registry_is_intrinsic("sin")
        if (.not. result) then
            print *, "  FAIL: sin not recognized as intrinsic"
            test_registry_functions = .false.
        else
            print *, "  PASS: sin recognized as intrinsic"
        end if
        
        result = registry_is_intrinsic("cos")
        if (.not. result) then
            print *, "  FAIL: cos not recognized as intrinsic"
            test_registry_functions = .false.
        else
            print *, "  PASS: cos recognized as intrinsic"
        end if
        
        result = registry_is_intrinsic("sqrt")
        if (.not. result) then
            print *, "  FAIL: sqrt not recognized as intrinsic"
            test_registry_functions = .false.
        else
            print *, "  PASS: sqrt recognized as intrinsic"
        end if
        
        ! Test user-defined function (should not be intrinsic)
        result = registry_is_intrinsic("my_function")
        if (result) then
            print *, "  FAIL: my_function incorrectly recognized as intrinsic"
            test_registry_functions = .false.
        else
            print *, "  PASS: my_function correctly not recognized as intrinsic"
        end if
        
        ! Test signature retrieval
        signature = registry_get_signature("sin")
        if (len_trim(signature) == 0) then
            print *, "  FAIL: No signature returned for sin"
            test_registry_functions = .false.
        else
            print *, "  PASS: sin signature: ", signature
        end if
        
    end function test_registry_functions
    
    logical function test_ast_intrinsic_identification()
        character(len=*), parameter :: source = &
            "program test" // new_line('A') // &
            "    real :: x, y" // new_line('A') // &
            "    x = 3.14" // new_line('A') // &
            "    y = sin(x)" // new_line('A') // &
            "end program test"
        
        type(token_t), allocatable :: tokens(:)  
        type(ast_arena_t) :: arena
        integer :: prog_index
        character(len=:), allocatable :: error_msg
        integer :: i
        logical :: found_sin_call
        
        test_ast_intrinsic_identification = .true.
        found_sin_call = .false.
        print *, "Testing AST intrinsic identification..."
        
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  FAIL: Lex error: ", error_msg
            test_ast_intrinsic_identification = .false.
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  FAIL: Parse error: ", error_msg  
            test_ast_intrinsic_identification = .false.
            return
        end if
        
        ! Search through arena for call_or_subscript_node with sin
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                select type (node => arena%entries(i)%node)
                type is (call_or_subscript_node)
                    if (allocated(node%name) .and. node%name == "sin") then
                        found_sin_call = .true.
                        if (.not. node%is_intrinsic) then
                            print *, "  FAIL: sin call not marked as intrinsic"
                            test_ast_intrinsic_identification = .false.
                        else
                            print *, "  PASS: sin call correctly marked as intrinsic"
                        end if
                        
                        if (.not. allocated(node%intrinsic_signature)) then
                            print *, "  FAIL: sin call missing intrinsic signature"
                            test_ast_intrinsic_identification = .false.
                        else
                            print *, "  PASS: sin signature: ", node%intrinsic_signature
                        end if
                        exit
                    end if
                end select
            end if
        end do
        
        if (.not. found_sin_call) then
            print *, "  FAIL: sin call not found in AST"
            test_ast_intrinsic_identification = .false.
        end if
        
    end function test_ast_intrinsic_identification
    
    logical function test_ast_user_function()
        character(len=*), parameter :: source = &
            "program test" // new_line('A') // &
            "    real :: x, y" // new_line('A') // &
            "    x = 3.14" // new_line('A') // &
            "    y = my_func(x)" // new_line('A') // &
            "end program test"
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        character(len=:), allocatable :: error_msg
        integer :: i
        logical :: found_user_call
        
        test_ast_user_function = .true.
        found_user_call = .false.
        print *, "Testing user function identification..."
        
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  FAIL: Lex error"
            test_ast_user_function = .false.
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  FAIL: Parse error"
            test_ast_user_function = .false.
            return
        end if
        
        ! Search through arena for call_or_subscript_node with my_func
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                select type (node => arena%entries(i)%node)
                type is (call_or_subscript_node)
                    if (allocated(node%name) .and. node%name == "my_func") then
                        found_user_call = .true.
                        if (node%is_intrinsic) then
                            print *, "  FAIL: my_func incorrectly marked as intrinsic"
                            test_ast_user_function = .false.
                        else
                            print *, "  PASS: my_func correctly not marked as intrinsic"
                        end if
                        
                        if (allocated(node%intrinsic_signature)) then
                            print *, "  FAIL: my_func has intrinsic signature"
                            test_ast_user_function = .false.
                        else
                            print *, "  PASS: my_func has no intrinsic signature"
                        end if
                        exit
                    end if
                end select
            end if
        end do
        
        if (.not. found_user_call) then
            print *, "  FAIL: my_func call not found in AST"
            test_ast_user_function = .false.
        end if
        
    end function test_ast_user_function
    
    logical function test_multiple_intrinsics()
        character(len=*), parameter :: source = &
            "program test" // new_line('A') // &
            "    real :: x, y, z" // new_line('A') // &
            "    x = 3.14" // new_line('A') // &
            "    y = sin(x)" // new_line('A') // &
            "    z = cos(x) + sqrt(y)" // new_line('A') // &
            "end program test"
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        character(len=:), allocatable :: error_msg
        integer :: i, intrinsic_count
        
        test_multiple_intrinsics = .true.
        intrinsic_count = 0
        print *, "Testing multiple intrinsic functions..."
        
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  FAIL: Lex error"
            test_multiple_intrinsics = .false.
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  FAIL: Parse error"
            test_multiple_intrinsics = .false.
            return
        end if
        
        ! Count intrinsic function calls
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                select type (node => arena%entries(i)%node)
                type is (call_or_subscript_node)
                    if (node%is_intrinsic) then
                        intrinsic_count = intrinsic_count + 1
                        print *, "  Found intrinsic: ", node%name, &
                                " with signature: ", node%intrinsic_signature
                    end if
                end select
            end if
        end do
        
        if (intrinsic_count == 3) then
            print *, "  PASS: Found 3 intrinsic functions (sin, cos, sqrt)"
        else
            print *, "  FAIL: Expected 3 intrinsic functions, found ", intrinsic_count
            test_multiple_intrinsics = .false.
        end if
        
    end function test_multiple_intrinsics
    
end program test_intrinsic_identification