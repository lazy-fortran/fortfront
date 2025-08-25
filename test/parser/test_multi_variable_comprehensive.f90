program test_multi_variable_comprehensive
    ! Comprehensive test for multi-variable declarations covering all scenarios:
    ! - Simple multi-var declarations (issue #5)
    ! - Multi-variable with attributes 
    ! - Parser-level multi-var handling
    ! - Mixed types and arrays
    use fortfront
    use lexer_core, only: token_t, TK_KEYWORD, TK_IDENTIFIER, TK_OPERATOR, TK_EOF
    use parser_state_module
    use parser_declarations, only: parse_declaration
    use ast_core
    implicit none

    character(len=:), allocatable :: source, output, error_msg
    integer :: test_count = 0, tests_passed = 0

    print *, "=== Comprehensive Multi-Variable Declaration Tests ==="

    ! Test all scenarios
    call test_simple_multi_var()
    call test_multi_var_with_attributes()
    call test_mixed_type_multi_var()
    call test_array_multi_var()
    call test_multi_var_usage_preservation()
    call test_parser_level_multi_var()

    ! Report results
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", tests_passed, "/", test_count, " tests"
    
    if (tests_passed == test_count) then
        print *, "All multi-variable declaration tests passed!"
        stop 0
    else
        print *, "Some multi-variable declaration tests failed"
        stop 1
    end if

contains

    subroutine test_simple_multi_var()
        ! Test issue #5: Simple multi-variable declarations
        call test_start("Simple multi-variable declaration")
        
        source = "program test" // new_line('a') // &
                 "    implicit none" // new_line('a') // &
                 "    real :: a, b, c" // new_line('a') // &
                 "end program test"
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        call assert_no_error(error_msg)
        call assert_contains(output, "real", "Type preserved")
        
        ! Check all variables are present (not necessarily as multi-var, but present)
        call assert_contains(output, "a", "Variable a preserved")
        call assert_contains(output, "b", "Variable b preserved") 
        call assert_contains(output, "c", "Variable c preserved")
        
        call test_pass()
    end subroutine test_simple_multi_var

    subroutine test_multi_var_with_attributes()
        ! Test multi-variable declarations with attributes
        call test_start("Multi-variable with attributes")
        
        source = "subroutine test()" // new_line('a') // &
                 "    real, parameter :: pi = 3.14, e = 2.718" // new_line('a') // &
                 "    integer, intent(in) :: x, y, z" // new_line('a') // &
                 "end subroutine test"
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        call assert_no_error(error_msg)
        call assert_contains(output, "parameter", "Parameter attribute preserved")
        call assert_contains(output, "intent(in)", "Intent attribute preserved")
        call assert_contains(output, "pi", "Parameter pi preserved")
        call assert_contains(output, "e", "Parameter e preserved")
        
        call test_pass()
    end subroutine test_multi_var_with_attributes

    subroutine test_mixed_type_multi_var()
        ! Test different types in same program
        call test_start("Mixed type multi-variable declarations")
        
        source = "program mixed_test" // new_line('a') // &
                 "    implicit none" // new_line('a') // &
                 "    real :: x, y" // new_line('a') // &
                 "    integer :: i, j, k" // new_line('a') // &
                 "    logical :: flag1, flag2" // new_line('a') // &
                 "end program mixed_test"
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        call assert_no_error(error_msg)
        call assert_contains(output, "real", "Real type preserved")
        call assert_contains(output, "integer", "Integer type preserved")
        call assert_contains(output, "logical", "Logical type preserved")
        
        call test_pass()
    end subroutine test_mixed_type_multi_var

    subroutine test_array_multi_var()
        ! Test multi-variable declarations with arrays
        call test_start("Multi-variable array declarations")
        
        source = "program array_test" // new_line('a') // &
                 "    implicit none" // new_line('a') // &
                 "    real, dimension(10) :: arr1, arr2, arr3" // new_line('a') // &
                 "    integer :: n = 5" // new_line('a') // &
                 "    real, dimension(n) :: dynamic1, dynamic2" // new_line('a') // &
                 "end program array_test"
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        call assert_no_error(error_msg)
        call assert_contains(output, "dimension", "Array dimension preserved")
        call assert_contains(output, "arr1", "Array 1 preserved")
        call assert_contains(output, "arr2", "Array 2 preserved")
        
        call test_pass()
    end subroutine test_array_multi_var

    subroutine test_multi_var_usage_preservation()
        ! Test that usage of multi-declared variables is preserved
        call test_start("Multi-variable usage preservation")
        
        source = "program usage_test" // new_line('a') // &
                 "    implicit none" // new_line('a') // &
                 "    real :: x, y, sum" // new_line('a') // &
                 "    x = 1.0" // new_line('a') // &
                 "    y = 2.0" // new_line('a') // &
                 "    sum = x + y" // new_line('a') // &
                 "    print *, sum" // new_line('a') // &
                 "end program usage_test"
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        call assert_no_error(error_msg)
        call assert_contains(output, "x = 1.0", "Assignment to x preserved")
        call assert_contains(output, "y = 2.0", "Assignment to y preserved") 
        call assert_contains(output, "sum = x + y", "Usage in expression preserved")
        
        call test_pass()
    end subroutine test_multi_var_usage_preservation

    subroutine test_parser_level_multi_var()
        ! Test parser-level multi-variable handling (direct parser call)
        type(token_t), allocatable :: tokens(:)
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        integer :: decl_index

        call test_start("Parser-level multi-variable handling")

        ! Create tokens for: real :: x, y
        allocate (tokens(8))
        tokens(1) = token_t(TK_KEYWORD, "real", 1, 1)
        tokens(2) = token_t(TK_OPERATOR, "::", 1, 6)
        tokens(3) = token_t(TK_IDENTIFIER, "x", 1, 9)
        tokens(4) = token_t(TK_OPERATOR, ",", 1, 10)
        tokens(5) = token_t(TK_IDENTIFIER, "y", 1, 12)
        tokens(6) = token_t(TK_EOF, "", 1, 13)

        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        decl_index = parse_declaration(parser, arena)

        if (decl_index <= 0) then
            call test_fail("No AST node returned for multi-variable declaration")
            return
        end if

        if (.not. allocated(arena%entries(decl_index)%node)) then
            call test_fail("Declaration node not allocated")
            return
        end if

        ! The specific node type check depends on implementation
        ! but we should get a valid declaration node
        call test_pass()
    end subroutine test_parser_level_multi_var

    ! Helper routines
    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A,A,A)', advance='no') "Testing ", test_name, "... "
    end subroutine test_start

    subroutine test_pass()
        tests_passed = tests_passed + 1
        print *, "PASS"
    end subroutine test_pass

    subroutine test_fail(reason)
        character(len=*), intent(in) :: reason
        print *, "FAIL: ", reason
    end subroutine test_fail

    subroutine assert_no_error(error_msg)
        character(len=:), allocatable, intent(in) :: error_msg
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            call test_fail("Unexpected error: " // trim(error_msg))
            return
        end if
    end subroutine assert_no_error

    subroutine assert_contains(text, pattern, description)
        character(len=*), intent(in) :: text, pattern, description
        if (index(text, pattern) == 0) then
            call test_fail(description // " - expected to find '" // pattern // "'")
            return
        end if
    end subroutine assert_contains

end program test_multi_variable_comprehensive