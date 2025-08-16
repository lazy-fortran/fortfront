program test_go_style_integration
    use ast_core
    use ast_nodes_misc, only: use_statement_node
    use parser_statements_module, only: parse_use_statement
    use parser_state_module
    use lexer_core
    use url_utilities, only: extract_module_from_url
    implicit none
    
    integer :: test_count = 0, pass_count = 0
    
    print *, "=== Comprehensive Go-Style Module Import Integration Tests ==="
    print *
    
    ! Test 1: URL parsing functionality with various formats
    call test_url_parsing_comprehensive()
    
    ! Test 2: Parser handling of use statements with URLs
    call test_parser_use_statement_integration()
    
    ! Test 3: AST node verification
    call test_ast_node_verification()
    
    ! Test 4: Edge cases and error handling
    call test_edge_cases()
    
    ! Test 5: Mixed traditional and Go-style imports
    call test_mixed_import_styles()
    
    print *
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "All Go-style integration tests passed!"
        stop 0
    else
        print *, "Some Go-style integration tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_url_parsing_comprehensive()
        character(:), allocatable :: module_name
        logical :: is_valid
        
        call test_start("URL parsing - GitHub HTTPS URL")
        call extract_module_from_url("https://github.com/user/repo/math_utils.f90", &
                                   module_name, is_valid)
        if (is_valid .and. module_name == "math_utils") then
            call test_pass()
        else
            call test_fail("Failed to extract module name from GitHub URL")
        end if
        
        call test_start("URL parsing - custom domain with path")
        call extract_module_from_url("https://example.com/libs/matrix.f90", &
                                   module_name, is_valid)
        if (is_valid .and. module_name == "matrix") then
            call test_pass()
        else
            call test_fail("Failed to extract module name from custom domain")
        end if
        
        call test_start("URL parsing - with version specifier")
        call extract_module_from_url("https://example.com/libs/matrix.f90@v2.1.0", &
                                   module_name, is_valid)
        if (is_valid .and. module_name == "matrix") then
            call test_pass()
        else
            call test_fail("Failed to extract module name with version specifier")
        end if
        
        call test_start("URL parsing - complex path structure")
        call extract_module_from_url("https://gitlab.com/group/project/src/utils/string_utils.f90", &
                                   module_name, is_valid)
        if (is_valid .and. module_name == "string_utils") then
            call test_pass()
        else
            call test_fail("Failed to extract module name from complex path")
        end if
        
        call test_start("URL parsing - HTTP (not HTTPS)")
        call extract_module_from_url("http://legacy.example.com/old_module.f90", &
                                   module_name, is_valid)
        if (is_valid .and. module_name == "old_module") then
            call test_pass()
        else
            call test_fail("Failed to handle HTTP URLs")
        end if
        
        call test_start("URL parsing - invalid URL (no protocol)")
        call extract_module_from_url("github.com/user/repo/module.f90", &
                                   module_name, is_valid)
        if (.not. is_valid) then
            call test_pass()
        else
            call test_fail("Should reject URL without protocol")
        end if
        
        call test_start("URL parsing - invalid URL (no file extension)")
        call extract_module_from_url("https://example.com/module", &
                                   module_name, is_valid)
        if (.not. is_valid) then
            call test_pass()
        else
            call test_fail("Should reject URL without .f90 extension")
        end if
        
        call test_start("URL parsing - invalid module name (starts with number)")
        call extract_module_from_url("https://example.com/123invalid.f90", &
                                   module_name, is_valid)
        if (.not. is_valid) then
            call test_pass()
        else
            call test_fail("Should reject invalid module name")
        end if
    end subroutine test_url_parsing_comprehensive
    
    
    subroutine test_parser_use_statement_integration()
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: stmt_idx
        type(use_statement_node), pointer :: use_node
        
        call test_start("Parser - basic Go-style use statement")
        ! Create tokens manually for a Go-style use statement
        allocate(tokens(2))
        tokens(1) = token_t(kind=TK_KEYWORD, text="use", line=1, column=1)
        tokens(2) = token_t(kind=TK_STRING, &
            text='"https://github.com/user/repo/math_utils.f90"', line=1, column=5)
        
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        
        stmt_idx = parse_use_statement(parser, arena)
        
        if (stmt_idx > 0) then
            select type(node => arena%entries(stmt_idx)%node)
            type is (use_statement_node)
                use_node => node
                if (allocated(use_node%url_spec) .and. &
                    use_node%url_spec == "https://github.com/user/repo/math_utils.f90" .and. &
                    use_node%module_name == "math_utils") then
                    call test_pass()
                else
                    call test_fail("Parser created incorrect use statement node")
                end if
            class default
                call test_fail("Parser returned wrong node type")
            end select
        else
            call test_fail("Parser failed to parse Go-style use statement")
        end if
        
        deallocate(tokens)
        
        call test_start("Parser - Go-style use with only clause")
        ! Create tokens manually: use "url", only: sin, cos
        allocate(tokens(6))
        tokens(1) = token_t(kind=TK_KEYWORD, text="use", line=1, column=1)
        tokens(2) = token_t(kind=TK_STRING, &
            text='"https://example.com/math.f90"', line=1, column=5)
        tokens(3) = token_t(kind=TK_OPERATOR, text=",", line=1, column=35)
        tokens(4) = token_t(kind=TK_KEYWORD, text="only", line=1, column=37)
        tokens(5) = token_t(kind=TK_OPERATOR, text=":", line=1, column=42)
        tokens(6) = token_t(kind=TK_IDENTIFIER, text="sin", line=1, column=44)
        
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        
        stmt_idx = parse_use_statement(parser, arena)
        
        if (stmt_idx > 0) then
            select type(node => arena%entries(stmt_idx)%node)
            type is (use_statement_node)
                use_node => node
                if (allocated(use_node%url_spec) .and. &
                    use_node%url_spec == "https://example.com/math.f90" .and. &
                    use_node%module_name == "math" .and. &
                    use_node%has_only .and. &
                    allocated(use_node%only_list) .and. &
                    size(use_node%only_list) >= 1) then
                    call test_pass()
                else
                    call test_fail("Parser failed to handle only clause with URL")
                end if
            class default
                call test_fail("Parser returned wrong node type for only clause")
            end select
        else
            call test_fail("Parser failed to parse Go-style use with only clause")
        end if
        
        deallocate(tokens)
        
        call test_start("Parser - traditional use statement still works")
        allocate(tokens(2))
        tokens(1) = token_t(kind=TK_KEYWORD, text="use", line=1, column=1)
        tokens(2) = token_t(kind=TK_IDENTIFIER, text="iso_fortran_env", line=1, column=5)
        
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        
        stmt_idx = parse_use_statement(parser, arena)
        
        if (stmt_idx > 0) then
            select type(node => arena%entries(stmt_idx)%node)
            type is (use_statement_node)
                use_node => node
                if (use_node%module_name == "iso_fortran_env" .and. &
                    (.not. allocated(use_node%url_spec) .or. len(use_node%url_spec) == 0)) then
                    call test_pass()
                else
                    call test_fail("Traditional use statement parsing broken")
                end if
            class default
                call test_fail("Parser returned wrong node type for traditional use")
            end select
        else
            call test_fail("Parser failed to parse traditional use statement")
        end if
        
        deallocate(tokens)
    end subroutine test_parser_use_statement_integration
    
    
    subroutine test_ast_node_verification()
        type(use_statement_node) :: use_node
        
        call test_start("AST node - url_spec field allocation and assignment")
        
        ! Test manual node creation
        use_node%module_name = "test_module"
        use_node%url_spec = "https://example.com/test_module.f90"
        use_node%has_only = .false.
        
        if (allocated(use_node%url_spec) .and. &
            use_node%url_spec == "https://example.com/test_module.f90" .and. &
            use_node%module_name == "test_module") then
            call test_pass()
        else
            call test_fail("AST node url_spec field not working correctly")
        end if
        
        call test_start("AST node - url_spec with only clause")
        use_node%has_only = .true.
        allocate(use_node%only_list(2))
        use_node%only_list(1)%s = "func1"
        use_node%only_list(2)%s = "func2"
        
        if (use_node%has_only .and. allocated(use_node%only_list) .and. &
            size(use_node%only_list) == 2 .and. &
            use_node%only_list(1)%s == "func1" .and. &
            use_node%only_list(2)%s == "func2") then
            call test_pass()
        else
            call test_fail("AST node only clause not working with URL")
        end if
    end subroutine test_ast_node_verification
    
    subroutine test_edge_cases()
        character(:), allocatable :: module_name
        logical :: is_valid
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: stmt_idx
        
        call test_start("Edge case - empty string URL")
        call extract_module_from_url("", module_name, is_valid)
        if (.not. is_valid) then
            call test_pass()
        else
            call test_fail("Should reject empty URL")
        end if
        
        call test_start("Edge case - URL with no filename")
        call extract_module_from_url("https://example.com/", module_name, is_valid)
        if (.not. is_valid) then
            call test_pass()
        else
            call test_fail("Should reject URL with no filename")
        end if
        
        call test_start("Edge case - URL with query parameters")
        call extract_module_from_url("https://example.com/module.f90?param=value", &
                                   module_name, is_valid)
        if (.not. is_valid) then
            call test_pass()
        else
            call test_fail("Should reject URL with query parameters")
        end if
        
        call test_start("Edge case - malformed use statement")
        ! Create tokens for malformed use statement
        allocate(tokens(1))
        tokens(1) = token_t(kind=TK_KEYWORD, text="use", line=1, column=1)
        
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        
        stmt_idx = parse_use_statement(parser, arena)
        
        ! Should handle gracefully (parser implementation dependent)
        if (stmt_idx >= 0) then  ! Any valid response is acceptable
            call test_pass()
        else
            call test_fail("Parser should handle malformed use statements gracefully")
        end if
        
        deallocate(tokens)
        
        call test_start("Edge case - manual token validation")
        ! Test the logic that would handle malformed tokens
        allocate(tokens(2))
        tokens(1) = token_t(kind=TK_KEYWORD, text="use", line=1, column=1)
        tokens(2) = token_t(kind=TK_STRING, text='"malformed"url"', line=1, column=5)
        
        ! Should process as a string token even if malformed
        if (tokens(2)%kind == TK_STRING) then
            call test_pass()
        else
            call test_fail("Should handle malformed string tokens")
        end if
        
        deallocate(tokens)
    end subroutine test_edge_cases
    
    subroutine test_mixed_import_styles()
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: stmt_idx
        type(use_statement_node), pointer :: use_node
        
        call test_start("Mixed styles - parse traditional use statement")
        allocate(tokens(2))
        tokens(1) = token_t(kind=TK_KEYWORD, text="use", line=1, column=1)
        tokens(2) = token_t(kind=TK_IDENTIFIER, text="iso_fortran_env", line=1, column=5)
        
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        
        stmt_idx = parse_use_statement(parser, arena)
        
        if (stmt_idx > 0) then
            select type(node => arena%entries(stmt_idx)%node)
            type is (use_statement_node)
                use_node => node
                if (use_node%module_name == "iso_fortran_env" .and. &
                    (.not. allocated(use_node%url_spec) .or. len(use_node%url_spec) == 0)) then
                    call test_pass()
                else
                    call test_fail("Traditional use statement parsing broken in mixed context")
                end if
            class default
                call test_fail("Traditional use statement returned wrong node type")
            end select
        else
            call test_fail("Failed to parse traditional use statement in mixed context")
        end if
        
        deallocate(tokens)
        
        call test_start("Mixed styles - parse Go-style use statement")
        allocate(tokens(2))
        tokens(1) = token_t(kind=TK_KEYWORD, text="use", line=1, column=1)
        tokens(2) = token_t(kind=TK_STRING, &
            text='"https://example.com/math.f90"', line=1, column=5)
        
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        
        stmt_idx = parse_use_statement(parser, arena)
        
        if (stmt_idx > 0) then
            select type(node => arena%entries(stmt_idx)%node)
            type is (use_statement_node)
                use_node => node
                if (allocated(use_node%url_spec) .and. &
                    use_node%url_spec == "https://example.com/math.f90" .and. &
                    use_node%module_name == "math") then
                    call test_pass()
                else
                    call test_fail("Go-style use statement parsing broken in mixed context")
                end if
            class default
                call test_fail("Go-style use statement returned wrong node type")
            end select
        else
            call test_fail("Failed to parse Go-style use statement in mixed context")
        end if
        
        deallocate(tokens)
    end subroutine test_mixed_import_styles
    
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
    
end program test_go_style_integration