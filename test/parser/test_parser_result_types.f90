program test_parser_result_types
    use parser_result_types
    use error_handling, only: ERROR_PARSER
    implicit none

    print *, "=== Parser Result Types Test ==="

    ! Test parse_result_t
    call test_parse_result_success()
    call test_parse_result_error()
    call test_parse_result_methods()

    ! Test compile_result_t
    call test_compile_result_success()
    call test_compile_result_error()
    call test_compile_result_warnings()

    print *, ""
    print *, "=== ALL TESTS PASSED ==="
    print *, "Parser result types working correctly!"

contains

    subroutine test_parse_result_success()
        type(parse_result_t) :: result
        
        ! Test success factory
        result = success_parse_result(42)
        
        if (.not. result%is_success()) then
            print *, "FAIL: Success result should be successful"
            stop 1
        end if
        
        if (result%is_failure()) then
            print *, "FAIL: Success result should not be failure"
            stop 1
        end if
        
        if (result%get_node() /= 42) then
            print *, "FAIL: Success result should return correct node index"
            stop 1
        end if
        
        print *, "PASS: Parse result success creation"
    end subroutine test_parse_result_success

    subroutine test_parse_result_error()
        type(parse_result_t) :: result
        
        ! Test error factory
        result = error_parse_result( &
            "Expected variable name after type", &
            ERROR_PARSER, &
            "parser_declarations", &
            "parse_declaration", &
            "Add variable name: 'integer :: var'" &
        )
        
        if (result%is_success()) then
            print *, "FAIL: Error result should not be successful"
            stop 1
        end if
        
        if (.not. result%is_failure()) then
            print *, "FAIL: Error result should be failure"
            stop 1
        end if
        
        if (result%get_node() /= 0) then
            print *, "FAIL: Error result should return 0 node index"
            stop 1
        end if
        
        print *, "PASS: Parse result error creation"
    end subroutine test_parse_result_error

    subroutine test_parse_result_methods()
        type(parse_result_t) :: result
        
        ! Test set_success method
        call result%set_success(123)
        if (.not. result%is_success() .or. result%get_node() /= 123) then
            print *, "FAIL: set_success method not working"
            stop 1
        end if
        
        ! Test set_error method
        call result%set_error("Test error", ERROR_PARSER, "test_component")
        if (result%is_success() .or. result%get_node() /= 0) then
            print *, "FAIL: set_error method not working"
            stop 1
        end if
        
        print *, "PASS: Parse result methods working"
    end subroutine test_parse_result_methods

    subroutine test_compile_result_success()
        type(compile_result_t) :: result
        
        ! Test success factory
        result = success_compile_result(99)
        
        if (.not. result%is_success()) then
            print *, "FAIL: Compile success result should be successful"
            stop 1
        end if
        
        if (result%get_program() /= 99) then
            print *, "FAIL: Compile success result should return correct program index"
            stop 1
        end if
        
        if (result%has_warnings()) then
            print *, "FAIL: New compile result should have no warnings"
            stop 1
        end if
        
        print *, "PASS: Compile result success creation"
    end subroutine test_compile_result_success

    subroutine test_compile_result_error()
        type(compile_result_t) :: result
        
        ! Test error factory
        result = error_compile_result( &
            "Compilation failed due to syntax errors", &
            ERROR_PARSER, &
            "parser_core", &
            "compile_source", &
            "Fix syntax errors and retry" &
        )
        
        if (result%is_success()) then
            print *, "FAIL: Compile error result should not be successful"
            stop 1
        end if
        
        if (result%get_program() /= 0) then
            print *, "FAIL: Compile error result should return 0 program index"
            stop 1
        end if
        
        print *, "PASS: Compile result error creation"
    end subroutine test_compile_result_error

    subroutine test_compile_result_warnings()
        type(compile_result_t) :: result
        
        ! Start with success result
        result = success_compile_result(55)
        
        ! Add warnings
        call result%add_warning( &
            "Unused variable 'temp'", &
            ERROR_PARSER, &
            "semantic_analyzer", &
            "variable_usage_check", &
            "Remove unused variable or add '!UNUSED' comment" &
        )
        
        call result%add_warning( &
            "Implicit interface for function 'legacy_func'", &
            ERROR_PARSER, &
            "semantic_analyzer", &
            "interface_check", &
            "Add explicit interface or import from module" &
        )
        
        if (.not. result%has_warnings()) then
            print *, "FAIL: Compile result should have warnings"
            stop 1
        end if
        
        if (result%get_warning_count() /= 2) then
            print *, "FAIL: Compile result should have exactly 2 warnings"
            stop 1
        end if
        
        ! Should still be successful despite warnings
        if (.not. result%is_success()) then
            print *, "FAIL: Compile result with warnings should still be successful"
            stop 1
        end if
        
        print *, "PASS: Compile result warnings working"
    end subroutine test_compile_result_warnings

end program test_parser_result_types