program test_json_reader_direct
    use json_reader
    use json_module
    use lexer_core
    use ast_core
    use semantic_analyzer
    implicit none
    
    integer :: test_count, pass_count
    
    test_count = 0
    pass_count = 0
    
    print *, "=== JSON Reader Direct Tests ==="
    
    ! Test JSON to tokens conversion
    call test_json_to_tokens()
    
    ! Test JSON to AST conversion
    call test_json_to_ast()
    
    ! Test error handling
    call test_json_error_handling()
    
    ! Test edge cases
    call test_json_edge_cases()
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "All JSON reader tests passed!"
        stop 0
    else
        print *, "Some JSON reader tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_json_to_tokens()
        type(json_file) :: json
        type(token_t), allocatable :: tokens(:)
        type(json_core) :: core
        type(json_value), pointer :: root, tokens_array, token_obj
        integer :: i
        
        call test_start("JSON to tokens conversion")
        
        ! Create JSON structure for tokens
        call json%initialize()
        call core%create_object(root, '')
        call core%create_array(tokens_array, 'tokens')
        
        ! Add a few test tokens
        do i = 1, 3
            call core%create_object(token_obj, '')
            
            select case (i)
            case (1)
                call core%add(token_obj, 'type', 'identifier')
                call core%add(token_obj, 'text', 'x')
                call core%add(token_obj, 'line', 1)
                call core%add(token_obj, 'column', 1)
            case (2)
                call core%add(token_obj, 'type', 'operator')
                call core%add(token_obj, 'text', '=')
                call core%add(token_obj, 'line', 1)
                call core%add(token_obj, 'column', 3)
            case (3)
                call core%add(token_obj, 'type', 'number')
                call core%add(token_obj, 'text', '42')
                call core%add(token_obj, 'line', 1)
                call core%add(token_obj, 'column', 5)
            end select
            
            call core%add(tokens_array, token_obj)
        end do
        
        call core%add(root, tokens_array)
        call json%add(root)
        
        ! Convert to tokens
        tokens = json_to_tokens(json)
        
        ! Verify conversion
        if (allocated(tokens)) then
            if (size(tokens) == 3) then
                if (tokens(1)%kind == TK_IDENTIFIER .and. &
                    tokens(2)%kind == TK_OPERATOR .and. &
                    tokens(3)%kind == TK_NUMBER) then
                    call test_pass()
                else
                    call test_fail("Token types incorrect")
                end if
            else
                call test_fail("Wrong number of tokens")
            end if
        else
            call test_fail("Tokens not allocated")
        end if
        
        call json%destroy()
    end subroutine test_json_to_tokens
    
    subroutine test_json_to_ast()
        type(json_file) :: json
        type(ast_arena_t) :: arena
        type(json_core) :: core
        type(json_value), pointer :: root, assign_node
        integer :: root_index
        
        call test_start("JSON to AST conversion")
        
        ! Initialize arena
        arena = create_ast_stack()
        
        ! Create JSON structure for simple assignment
        call json%initialize()
        call core%create_object(root, '')
        call core%add(root, 'type', 'assignment')
        call core%add(root, 'variable', 'x')
        call core%add(root, 'value_type', 'number')
        call core%add(root, 'value', '42')
        call json%add(root)
        
        ! Convert to AST
        root_index = json_to_ast(json, arena)
        
        ! Verify conversion
        if (root_index > 0 .and. root_index <= arena%size) then
            ! Can only verify that conversion succeeded
            call test_pass()
        else
            call test_fail("Invalid root index")
        end if
        
        call json%destroy()
    end subroutine test_json_to_ast
    
    subroutine test_json_error_handling()
        type(json_file) :: json
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        
        call test_start("JSON error handling - missing tokens")
        
        ! Test with JSON missing tokens array
        call json%initialize()
        tokens = json_to_tokens(json)
        
        if (.not. allocated(tokens) .or. size(tokens) == 0) then
            call test_pass()
        else
            call test_fail("Should handle missing tokens")
        end if
        
        call json%destroy()
        
        call test_start("JSON error handling - invalid token type")
        
        ! Test with invalid token type
        block
            type(json_core) :: core
            type(json_value), pointer :: root, tokens_array, token_obj
            
            call json%initialize()
            call core%create_object(root, '')
            call core%create_array(tokens_array, 'tokens')
            call core%create_object(token_obj, '')
            call core%add(token_obj, 'type', 'invalid_type')
            call core%add(token_obj, 'text', 'test')
            call core%add(token_obj, 'line', 1)
            call core%add(token_obj, 'column', 1)
            call core%add(tokens_array, token_obj)
            call core%add(root, tokens_array)
            call json%add(root)
            
            tokens = json_to_tokens(json)
            
            if (allocated(tokens) .and. size(tokens) == 1) then
                if (tokens(1)%kind == TK_EOF) then  ! Default for unknown type
                    call test_pass()
                else
                    call test_fail("Should default to TK_EOF")
                end if
            else
                call test_fail("Token conversion failed")
            end if
        end block
        
        call json%destroy()
    end subroutine test_json_error_handling
    
    subroutine test_json_edge_cases()
        type(json_file) :: json
        type(ast_arena_t) :: arena
        type(json_core) :: core
        type(json_value), pointer :: root
        integer :: root_index
        
        call test_start("JSON edge case - nested AST node")
        
        ! Initialize arena
        arena = create_ast_stack()
        
        ! Create nested structure
        call json%initialize()
        call core%create_object(root, '')
        call core%add(root, 'type', 'identifier')
        call core%add(root, 'name', 'test_var')
        call json%add(root)
        
        ! Convert to AST - should handle nested case
        root_index = json_to_ast(json, arena)
        
        if (root_index > 0) then
            call test_pass()
        else
            call test_fail("Failed to handle nested node")
        end if
        
        call json%destroy()
        
        call test_start("JSON edge case - all token types")
        
        ! Test all token type conversions
        block
            type(json_value), pointer :: tokens_array, token_obj
            type(token_t), allocatable :: tokens(:)
            character(len=20), dimension(7) :: token_types
            integer :: i
            
            token_types = ['identifier', 'keyword   ', 'operator  ', &
                          'number    ', 'string    ', 'newline   ', 'eof       ']
            
            call json%initialize()
            call core%create_object(root, '')
            call core%create_array(tokens_array, 'tokens')
            
            do i = 1, 7
                call core%create_object(token_obj, '')
                call core%add(token_obj, 'type', trim(token_types(i)))
                call core%add(token_obj, 'text', 'test')
                call core%add(token_obj, 'line', i)
                call core%add(token_obj, 'column', 1)
                call core%add(tokens_array, token_obj)
            end do
            
            call core%add(root, tokens_array)
            call json%add(root)
            
            tokens = json_to_tokens(json)
            
            if (allocated(tokens) .and. size(tokens) == 7) then
                if (tokens(1)%kind == TK_IDENTIFIER .and. &
                    tokens(2)%kind == TK_KEYWORD .and. &
                    tokens(3)%kind == TK_OPERATOR .and. &
                    tokens(4)%kind == TK_NUMBER .and. &
                    tokens(5)%kind == TK_STRING .and. &
                    tokens(6)%kind == TK_NEWLINE .and. &
                    tokens(7)%kind == TK_EOF) then
                    call test_pass()
                else
                    call test_fail("Token type conversion error")
                end if
            else
                call test_fail("Wrong number of tokens")
            end if
        end block
        
        call json%destroy()
    end subroutine test_json_edge_cases
    
    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A,A)', advance='no') "Testing: ", test_name
    end subroutine test_start
    
    subroutine test_pass()
        print *, " ... PASSED"
        pass_count = pass_count + 1
    end subroutine test_pass
    
    subroutine test_fail(reason)
        character(len=*), intent(in) :: reason
        print *, " ... FAILED"
        print *, "  Reason: ", reason
    end subroutine test_fail
    
end program test_json_reader_direct