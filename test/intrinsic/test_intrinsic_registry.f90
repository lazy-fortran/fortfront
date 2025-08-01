program test_intrinsic_registry
    use intrinsic_registry
    implicit none
    
    call test_intrinsic_function_identification()
    call test_intrinsic_signature_retrieval()
    call test_case_insensitive_lookup()
    call test_non_intrinsic_functions()
    call test_registry_initialization()
    
    print *, "All intrinsic registry tests passed!"
    
contains

    subroutine test_intrinsic_function_identification()
        logical :: result
        
        print *, "Testing intrinsic function identification..."
        
        ! Test mathematical functions
        if (.not. is_intrinsic_function("sin")) error stop "sin should be intrinsic"
        if (.not. is_intrinsic_function("cos")) error stop "cos should be intrinsic"
        if (.not. is_intrinsic_function("sqrt")) error stop "sqrt should be intrinsic"
        if (.not. is_intrinsic_function("abs")) error stop "abs should be intrinsic"
        
        ! Test type conversion functions
        if (.not. is_intrinsic_function("int")) error stop "int should be intrinsic"
        if (.not. is_intrinsic_function("real")) error stop "real should be intrinsic"
        if (.not. is_intrinsic_function("nint")) error stop "nint should be intrinsic"
        
        ! Test array functions
        if (.not. is_intrinsic_function("size")) error stop "size should be intrinsic"
        if (.not. is_intrinsic_function("sum")) error stop "sum should be intrinsic"
        if (.not. is_intrinsic_function("shape")) error stop "shape should be intrinsic"
        
        ! Test string functions
        if (.not. is_intrinsic_function("len")) error stop "len should be intrinsic"
        if (.not. is_intrinsic_function("trim")) error stop "trim should be intrinsic"
        if (.not. is_intrinsic_function("adjustl")) error stop "adjustl should be intrinsic"
        
        ! Test inquiry functions
        if (.not. is_intrinsic_function("present")) error stop "present should be intrinsic"
        if (.not. is_intrinsic_function("allocated")) error stop "allocated should be intrinsic"
        
        print *, "  ✓ Intrinsic function identification tests passed"
    end subroutine test_intrinsic_function_identification

    subroutine test_intrinsic_signature_retrieval()
        character(len=:), allocatable :: signature
        
        print *, "Testing intrinsic signature retrieval..."
        
        ! Test mathematical function signatures
        signature = get_intrinsic_signature("sin")
        if (signature /= "real(real)") error stop "sin signature incorrect"
        
        signature = get_intrinsic_signature("sqrt")
        if (signature /= "real(real)") error stop "sqrt signature incorrect"
        
        signature = get_intrinsic_signature("abs")
        if (signature /= "real(real)") error stop "abs signature incorrect"
        
        ! Test type conversion signatures
        signature = get_intrinsic_signature("int")
        if (signature /= "integer(real)") error stop "int signature incorrect"
        
        signature = get_intrinsic_signature("real")
        if (signature /= "real(integer)") error stop "real signature incorrect"
        
        ! Test array function signatures
        signature = get_intrinsic_signature("size")
        if (signature /= "integer(array)") error stop "size signature incorrect"
        
        signature = get_intrinsic_signature("sum")
        if (signature /= "numeric(array)") error stop "sum signature incorrect"
        
        ! Test string function signatures
        signature = get_intrinsic_signature("len")
        if (signature /= "integer(character)") error stop "len signature incorrect"
        
        signature = get_intrinsic_signature("trim")
        if (signature /= "character(character)") error stop "trim signature incorrect"
        
        print *, "  ✓ Intrinsic signature retrieval tests passed"
    end subroutine test_intrinsic_signature_retrieval

    subroutine test_case_insensitive_lookup()
        logical :: result
        character(len=:), allocatable :: signature
        
        print *, "Testing case insensitive lookup..."
        
        ! Test uppercase
        if (.not. is_intrinsic_function("SIN")) error stop "SIN should be intrinsic"
        if (.not. is_intrinsic_function("COS")) error stop "COS should be intrinsic"
        if (.not. is_intrinsic_function("SQRT")) error stop "SQRT should be intrinsic"
        
        ! Test mixed case
        if (.not. is_intrinsic_function("Sin")) error stop "Sin should be intrinsic"
        if (.not. is_intrinsic_function("CoS")) error stop "CoS should be intrinsic"
        if (.not. is_intrinsic_function("SqRt")) error stop "SqRt should be intrinsic"
        
        ! Test signature retrieval with different cases
        signature = get_intrinsic_signature("SIN")
        if (signature /= "real(real)") error stop "SIN signature incorrect"
        
        signature = get_intrinsic_signature("Sin")
        if (signature /= "real(real)") error stop "Sin signature incorrect"
        
        print *, "  ✓ Case insensitive lookup tests passed"
    end subroutine test_case_insensitive_lookup

    subroutine test_non_intrinsic_functions()
        logical :: result
        character(len=:), allocatable :: signature
        
        print *, "Testing non-intrinsic functions..."
        
        ! Test user-defined function names
        if (is_intrinsic_function("my_function")) &
            error stop "my_function should not be intrinsic"
        if (is_intrinsic_function("user_routine")) &
            error stop "user_routine should not be intrinsic"
        if (is_intrinsic_function("calculate")) &
            error stop "calculate should not be intrinsic"
        if (is_intrinsic_function("nonexistent")) &
            error stop "nonexistent should not be intrinsic"
        
        ! Test empty signature for non-intrinsic functions
        signature = get_intrinsic_signature("my_function")
        if (allocated(signature) .and. len_trim(signature) /= 0) &
            error stop "Non-intrinsic function should have empty signature"
        
        signature = get_intrinsic_signature("unknown")
        if (allocated(signature) .and. len_trim(signature) /= 0) &
            error stop "Unknown function should have empty signature"
        
        print *, "  ✓ Non-intrinsic function tests passed"
    end subroutine test_non_intrinsic_functions

    subroutine test_registry_initialization()
        logical :: is_intrinsic
        character(len=:), allocatable :: signature
        
        print *, "Testing registry initialization..."
        
        ! Force registry initialization through get_intrinsic_info
        call get_intrinsic_info("sin", is_intrinsic, signature)
        
        if (.not. is_intrinsic) error stop "sin should be intrinsic after initialization"
        if (signature /= "real(real)") error stop "sin signature incorrect after initialization"
        
        ! Test that multiple calls work correctly
        call get_intrinsic_info("cos", is_intrinsic, signature)
        if (.not. is_intrinsic) error stop "cos should be intrinsic"
        if (signature /= "real(real)") error stop "cos signature incorrect"
        
        ! Test with non-intrinsic
        call get_intrinsic_info("my_func", is_intrinsic, signature)
        if (is_intrinsic) error stop "my_func should not be intrinsic"
        if (allocated(signature)) then
            if (len_trim(signature) > 0) &
                error stop "Non-intrinsic should not have non-empty signature"
        end if
        
        print *, "  ✓ Registry initialization tests passed"
    end subroutine test_registry_initialization

end program test_intrinsic_registry