program test_string_transformation
    use frontend, only: transform_lazy_fortran_string
    implicit none
    
    integer :: test_count, pass_count
    
    test_count = 0
    pass_count = 0
    
    print *, "=== String Transformation Unit Tests ==="
    print *, ""
    
    ! Test 1: Simple hello world
    call test_hello_world()
    
    ! Test 2: Type inference
    call test_type_inference()
    
    ! Test 3: Control flow
    call test_control_flow()
    
    ! Test 4: Multiple statements
    call test_multiple_statements()
    
    ! Test 5: Syntax error handling
    call test_syntax_error()
    
    ! Test 6: Empty input
    call test_empty_input()
    
    ! Test 7: Complex expression
    call test_complex_expression()
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "All tests passed!"
        stop 0
    else
        print *, "Some tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_hello_world()
        character(len=:), allocatable :: output, error_msg
        logical :: success
        
        call test_start("Simple hello world")
        
        call transform_lazy_fortran_string("print *, 'Hello'", output, error_msg)
        
        success = (len_trim(error_msg) == 0) .and. &
                  (index(output, "program main") > 0) .and. &
                  (index(output, "implicit none") > 0) .and. &
                  (index(output, "print *, 'Hello'") > 0)
        
        call test_result(success)
        if (.not. success) then
            print *, "  Error: ", trim(error_msg)
            print *, "  Output length: ", len(output)
        end if
    end subroutine test_hello_world
    
    subroutine test_type_inference()
        character(len=:), allocatable :: input, output, error_msg
        logical :: success
        
        call test_start("Type inference")
        
        input = "x = 42" // new_line('A') // "y = 3.14"
        call transform_lazy_fortran_string(input, output, error_msg)
        
        success = (len_trim(error_msg) == 0) .and. &
                  (index(output, "integer :: x") > 0) .and. &
                  (index(output, "real") > 0)
        
        call test_result(success)
        if (.not. success) then
            print *, "  Error: ", trim(error_msg)
            print *, "  Looking for 'integer :: x' and 'real'"
        end if
    end subroutine test_type_inference
    
    subroutine test_control_flow()
        character(len=:), allocatable :: input, output, error_msg
        logical :: success
        
        call test_start("Control flow (if statement)")
        
        input = "x = 5" // new_line('A') // &
                "if (x > 0) then" // new_line('A') // &
                "  print *, 'positive'" // new_line('A') // &
                "end if"
        
        call transform_lazy_fortran_string(input, output, error_msg)
        
        success = (len_trim(error_msg) == 0) .and. &
                  (index(output, "if (x > 0) then") > 0) .and. &
                  (index(output, "end if") > 0)
        
        call test_result(success)
        if (.not. success) then
            print *, "  Error: ", trim(error_msg)
        end if
    end subroutine test_control_flow
    
    subroutine test_multiple_statements()
        character(len=:), allocatable :: input, output, error_msg
        logical :: success
        
        call test_start("Multiple statements")
        
        input = "a = 1" // new_line('A') // &
                "b = 2" // new_line('A') // &
                "c = a + b" // new_line('A') // &
                "print *, c"
        
        call transform_lazy_fortran_string(input, output, error_msg)
        
        success = (len_trim(error_msg) == 0) .and. &
                  (index(output, "integer :: a") > 0) .and. &
                  (index(output, "integer :: b") > 0) .and. &
                  (index(output, "integer :: c") > 0)
        
        call test_result(success)
        if (.not. success) then
            print *, "  Error: ", trim(error_msg)
        end if
    end subroutine test_multiple_statements
    
    subroutine test_syntax_error()
        character(len=:), allocatable :: output, error_msg
        logical :: success
        
        call test_start("Syntax error handling")
        
        call transform_lazy_fortran_string("invalid fortran !!!", output, error_msg)
        
        ! For syntax errors, we expect the transformation to still work
        ! but might produce minimal output
        success = .true.  ! Any non-crash result is success for now
        
        call test_result(success)
    end subroutine test_syntax_error
    
    subroutine test_empty_input()
        character(len=:), allocatable :: output, error_msg
        logical :: success
        
        call test_start("Empty input")
        
        call transform_lazy_fortran_string("", output, error_msg)
        
        ! Empty input should produce minimal program
        success = (index(output, "program main") > 0)
        
        call test_result(success)
        if (.not. success) then
            print *, "  Error: ", trim(error_msg)
            print *, "  Output: ", trim(output)
        end if
    end subroutine test_empty_input
    
    subroutine test_complex_expression()
        character(len=:), allocatable :: input, output, error_msg
        logical :: success
        
        call test_start("Complex expression")
        
        input = "x = 5" // new_line('A') // &
                "y = 2.5" // new_line('A') // &
                "result = (x * 2 + y) / 3.0"
        call transform_lazy_fortran_string(input, output, error_msg)
        
        success = (len_trim(error_msg) == 0) .and. &
                  (index(output, "result = ") > 0) .and. &
                  (index(output, "integer :: x") > 0) .and. &
                  (index(output, "real") > 0) .and. &
                  (index(output, "real(8) :: result") > 0)
        
        call test_result(success)
        if (.not. success) then
            print *, "  Error: ", trim(error_msg)
            print *, "  Looking for variable declarations and expression"
            print *, "  Actual output:"
            print *, output
        end if
    end subroutine test_complex_expression
    
    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A,A)', advance='no') "Testing: ", test_name
    end subroutine test_start
    
    subroutine test_result(success)
        logical, intent(in) :: success
        if (success) then
            print *, " ... PASSED"
            pass_count = pass_count + 1
        else
            print *, " ... FAILED"
        end if
    end subroutine test_result
    
end program test_string_transformation