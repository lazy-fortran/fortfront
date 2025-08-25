program test_mixed_constructs_comprehensive
    use frontend, only: transform_lazy_fortran_string
    implicit none
    
    logical :: all_tests_pass
    
    all_tests_pass = .true.
    
    print *, "=== Comprehensive Mixed Constructs Tests (Issue #488) ==="
    
    ! Test the exact CLI reproducer
    if (.not. test_cli_reproducer()) all_tests_pass = .false.
    
    ! Test with use statement
    if (.not. test_with_use_statement()) all_tests_pass = .false.
    
    ! Test with multiple assignments
    if (.not. test_multiple_assignments()) all_tests_pass = .false.
    
    ! Test with function call
    if (.not. test_with_function_call()) all_tests_pass = .false.
    
    print *
    if (all_tests_pass) then
        print *, "SUCCESS: All mixed constructs tests passed"
        stop 0
    else
        print *, "FAIL: Some mixed constructs tests failed"
        stop 1
    end if
    
contains

    logical function test_cli_reproducer()
        character(:), allocatable :: input, output, error_msg
        
        print *, "Testing CLI reproducer case..."
        
        input = "module test_mod" // new_line('a') // &
                "  x = 10" // new_line('a') // &
                "end module" // new_line('a') // &
                "y = x + 5"
        
        call transform_lazy_fortran_string(input, output, error_msg)
        
        if (error_msg /= "") then
            print *, "  FAIL: Error:", trim(error_msg)
            test_cli_reproducer = .false.
            return
        end if
        
        ! Check that output contains both module and implicit main
        if (index(output, "module test_mod") > 0 .and. &
            index(output, "end module test_mod") > 0 .and. &
            index(output, "program main") > 0 .and. &
            index(output, "y = x + 5") > 0) then
            print *, "  SUCCESS: Both module and implicit main generated"
            test_cli_reproducer = .true.
        else
            print *, "  FAIL: Missing expected content in output"
            print *, "  Output:"
            print *, trim(output)
            print *, "  Expected: module test_mod, end module test_mod, program main, y = x + 5"
            test_cli_reproducer = .false.
        end if
    end function test_cli_reproducer
    
    logical function test_with_use_statement()
        character(:), allocatable :: input, output, error_msg
        
        print *, "Testing mixed constructs with use statement..."
        
        input = "module utils" // new_line('a') // &
                "  integer :: x = 42" // new_line('a') // &
                "end module utils" // new_line('a') // &
                "use utils" // new_line('a') // &
                "print *, x"
        
        call transform_lazy_fortran_string(input, output, error_msg)
        
        if (error_msg /= "") then
            print *, "  FAIL: Error:", trim(error_msg)
            test_with_use_statement = .false.
            return
        end if
        
        if (index(output, "module utils") > 0 .and. &
            index(output, "use utils") > 0 .and. &
            index(output, "print") > 0) then
            print *, "  SUCCESS: Module with use statement works"
            test_with_use_statement = .true.
        else
            print *, "  FAIL: Missing expected content"
            test_with_use_statement = .false.
        end if
    end function test_with_use_statement
    
    logical function test_multiple_assignments()
        character(:), allocatable :: input, output, error_msg
        
        print *, "Testing multiple implicit main assignments..."
        
        input = "module math" // new_line('a') // &
                "end module math" // new_line('a') // &
                "x = 1" // new_line('a') // &
                "y = 2" // new_line('a') // &
                "z = x + y"
        
        call transform_lazy_fortran_string(input, output, error_msg)
        
        if (error_msg /= "") then
            print *, "  FAIL: Error:", trim(error_msg)
            test_multiple_assignments = .false.
            return
        end if
        
        if (index(output, "module math") > 0 .and. &
            index(output, "x = 1") > 0 .and. &
            index(output, "y = 2") > 0 .and. &
            index(output, "z = x + y") > 0) then
            print *, "  SUCCESS: Multiple assignments work"
            test_multiple_assignments = .true.
        else
            print *, "  FAIL: Missing expected assignments"
            test_multiple_assignments = .false.
        end if
    end function test_multiple_assignments
    
    logical function test_with_function_call()
        character(:), allocatable :: input, output, error_msg
        
        print *, "Testing mixed constructs with function call..."
        
        input = "module funcs" // new_line('a') // &
                "  contains" // new_line('a') // &
                "  integer function add(a, b)" // new_line('a') // &
                "    integer :: a, b" // new_line('a') // &
                "    add = a + b" // new_line('a') // &
                "  end function add" // new_line('a') // &
                "end module funcs" // new_line('a') // &
                "use funcs" // new_line('a') // &
                "result = add(5, 3)"
        
        call transform_lazy_fortran_string(input, output, error_msg)
        
        if (error_msg /= "") then
            print *, "  FAIL: Error:", trim(error_msg)
            test_with_function_call = .false.
            return
        end if
        
        if (index(output, "module funcs") > 0 .and. &
            index(output, "function add") > 0 .and. &
            index(output, "result = add(5, 3)") > 0) then
            print *, "  SUCCESS: Function call works"
            test_with_function_call = .true.
        else
            print *, "  FAIL: Missing expected function content"
            test_with_function_call = .false.
        end if
    end function test_with_function_call
    
end program test_mixed_constructs_comprehensive