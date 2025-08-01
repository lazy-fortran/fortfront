program test_intrinsic_parser
    use frontend
    use lexer_core
    use ast_core
    implicit none
    
    call test_intrinsic_function_in_pipeline()
    
    print *, "All intrinsic parser integration tests passed!"
    
contains

    subroutine test_intrinsic_function_in_pipeline()
        character(len=*), parameter :: source_code = &
            "program test" // new_line('a') // &
            "  real :: x, y" // new_line('a') // &
            "  x = sin(y)" // new_line('a') // &
            "  y = sqrt(x)" // new_line('a') // &
            "end program"
        
        character(len=:), allocatable :: output, error_msg
        
        print *, "Testing intrinsic function processing in pipeline..."
        
        ! Test that the pipeline can process intrinsic functions without errors
        call transform_lazy_fortran_string(source_code, output, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, "Error: ", error_msg
            error stop "Pipeline failed to process intrinsic functions"
        end if
        
        if (.not. allocated(output) .or. len_trim(output) == 0) &
            error stop "Pipeline should produce output"
        
        ! Check that the output contains the intrinsic function calls
        if (index(output, "sin(") == 0) &
            error stop "Output should contain sin function call"
        
        if (index(output, "sqrt(") == 0) &
            error stop "Output should contain sqrt function call"
        
        print *, "  ✓ Intrinsic function pipeline processing tests passed"
    end subroutine test_intrinsic_function_in_pipeline

end program test_intrinsic_parser