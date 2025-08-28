program test_parameter_attributes_subroutine
    use frontend
    implicit none
    
    logical :: all_passed = .true.
    
    print *, '=== Testing Parameter Attributes in Subroutine ==='
    print *
    
    if (.not. test_subroutine_params()) all_passed = .false.
    
    if (all_passed) then
        print *, 'PASS: Subroutine parameter tests passed'
        stop 0
    else
        print *, 'FAIL: Subroutine parameter tests failed'
        stop 1
    end if
    
contains
    
    function test_subroutine_params() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, generated_code, error_msg
        
        passed = .true.
        
        ! Create a simple subroutine to test parameter preservation
        source = &
            "subroutine test(required, opt, output)" // new_line('a') // &
            "    integer, intent(in) :: required" // new_line('a') // &
            "    integer, intent(in), optional :: opt" // new_line('a') // &
            "    integer, intent(out) :: output" // new_line('a') // &
            "    output = required * 2" // new_line('a') // &
            "end subroutine test"
        
        call transform_lazy_fortran_string(source, generated_code, error_msg)
        
        if (error_msg /= "") then
            print *, "ERROR: Failed to parse: ", trim(error_msg)
            passed = .false.
        else if (.not. allocated(generated_code)) then
            print *, "ERROR: No output generated"
            passed = .false.
        else if (len(generated_code) == 0) then
            print *, "ERROR: Empty output generated"
            passed = .false.
        else
            print *, "Generated code:"
            print *, "================="
            print *, trim(generated_code)
            print *, "================="
            print *, "Length:", len(generated_code)
            
            ! Check attributes are preserved
            if (index(generated_code, "intent(in)") == 0) then
                print *, "FAIL: intent(in) not preserved"
                passed = .false.
            else
                print *, "PASS: intent(in) preserved"
            end if
            
            if (index(generated_code, "intent(out)") == 0) then
                print *, "FAIL: intent(out) not preserved"
                passed = .false.
            else
                print *, "PASS: intent(out) preserved"
            end if
            
            if (index(generated_code, "optional") == 0) then
                print *, "FAIL: optional not preserved"
                passed = .false.
            else
                print *, "PASS: optional preserved"
            end if
        end if
        
    end function test_subroutine_params
    
end program test_parameter_attributes_subroutine