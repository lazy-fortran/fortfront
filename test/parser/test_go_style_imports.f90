program test_go_style_imports
    use parser_statements_module
    use parser_state_module
    use lexer_core
    use ast_core
    use ast_factory
    use ast_nodes_misc, only: use_statement_node
    use url_utilities, only: extract_module_from_url
    implicit none
    
    integer :: test_count, pass_count
    
    test_count = 0
    pass_count = 0
    
    print *, "=== Go-Style Module Import Tests ==="
    
    ! Test URL parsing functionality
    call test_url_parsing()
    
    ! Test use statement with string token parsing
    call test_use_statement_string_parsing()
    
    ! Test AST node url_spec field
    call test_use_statement_url_spec()
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "All Go-style import tests passed!"
        stop 0
    else
        print *, "Some Go-style import tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_url_parsing()
        character(:), allocatable :: module_name
        logical :: is_valid
        
        call test_start("URL parsing - simple HTTPS URL")
        
        ! Test: use "https://github.com/user/repo/module.f90"
        call extract_module_from_url( &
            "https://github.com/user/repo/module.f90", &
            module_name, is_valid)
        
        if (is_valid .and. module_name == "module") then
            call test_pass()
        else
            call test_fail("Failed to extract module name from URL")
        end if
        
        call test_start("URL parsing - with version specifier")
        
        ! Test: use "https://github.com/user/repo/math.f90@v1.2.3"
        call extract_module_from_url( &
            "https://github.com/user/repo/math.f90@v1.2.3", &
            module_name, is_valid)
        
        if (is_valid .and. module_name == "math") then
            call test_pass()
        else
            call test_fail("Failed to extract module name from versioned URL")
        end if
        
        call test_start("URL parsing - invalid URL")
        
        ! Test: invalid URL should fail gracefully
        call extract_module_from_url("not-a-url", module_name, is_valid)
        
        if (.not. is_valid) then
            call test_pass()
        else
            call test_fail("Should reject invalid URL")
        end if
    end subroutine test_url_parsing
    
    subroutine test_use_statement_string_parsing()
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: stmt_idx
        
        call test_start("Use statement - quoted module name")
        
        ! Create tokens: use "https://example.com/module.f90"
        allocate(tokens(2))
        tokens(1) = token_t(kind=TK_KEYWORD, text="use", line=1, column=1)
        tokens(2) = token_t(kind=TK_STRING, &
            text='"https://example.com/module.f90"', line=1, column=5)
        
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        
        stmt_idx = parse_use_statement(parser, arena)
        
        if (stmt_idx > 0) then
            call test_pass()
        else
            call test_fail("Failed to parse use statement with string")
        end if
        
        deallocate(tokens)
    end subroutine test_use_statement_string_parsing
    
    subroutine test_use_statement_url_spec()
        type(use_statement_node) :: use_node
        
        call test_start("Use statement node - url_spec field")
        
        ! Create a use statement node with URL specification
        use_node%module_name = "module"
        use_node%url_spec = "https://example.com/module.f90"
        use_node%has_only = .false.
        
        if (allocated(use_node%url_spec) .and. &
            use_node%url_spec == "https://example.com/module.f90") then
            call test_pass()
        else
            call test_fail("url_spec field not working correctly")
        end if
    end subroutine test_use_statement_url_spec
    
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
    
    
end program test_go_style_imports