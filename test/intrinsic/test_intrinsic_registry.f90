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
        if (.not. is_intrinsic_function("sin")) then
            print *, "sin should be intrinsic"
            stop 1
        end if
        if (.not. is_intrinsic_function("cos")) then
            print *, "cos should be intrinsic"
            stop 1
        end if
        if (.not. is_intrinsic_function("sqrt")) then
            print *, "sqrt should be intrinsic"
            stop 1
        end if
        if (.not. is_intrinsic_function("abs")) then
            print *, "abs should be intrinsic"
            stop 1
        end if
        
        ! Test type conversion functions
        if (.not. is_intrinsic_function("int")) then
            print *, "int should be intrinsic"
            stop 1
        end if
        if (.not. is_intrinsic_function("real")) then
            print *, "real should be intrinsic"
            stop 1
        end if
        if (.not. is_intrinsic_function("nint")) then
            print *, "nint should be intrinsic"
            stop 1
        end if
        
        ! Test array functions
        if (.not. is_intrinsic_function("size")) then
            print *, "size should be intrinsic"
            stop 1
        end if
        if (.not. is_intrinsic_function("sum")) then
            print *, "sum should be intrinsic"
            stop 1
        end if
        if (.not. is_intrinsic_function("shape")) then
            print *, "shape should be intrinsic"
            stop 1
        end if
        
        ! Test string functions
        if (.not. is_intrinsic_function("len")) then
            print *, "len should be intrinsic"
            stop 1
        end if
        if (.not. is_intrinsic_function("trim")) then
            print *, "trim should be intrinsic"
            stop 1
        end if
        if (.not. is_intrinsic_function("adjustl")) then
            print *, "adjustl should be intrinsic"
            stop 1
        end if
        
        ! Test inquiry functions
        if (.not. is_intrinsic_function("present")) then
            print *, "present should be intrinsic"
            stop 1
        end if
        if (.not. is_intrinsic_function("allocated")) then
            print *, "allocated should be intrinsic"
            stop 1
        end if
        
        print *, "  ✓ Intrinsic function identification tests passed"
    end subroutine test_intrinsic_function_identification

    subroutine test_intrinsic_signature_retrieval()
        character(len=:), allocatable :: signature
        
        print *, "Testing intrinsic signature retrieval..."
        
        ! Test mathematical function signatures
        signature = get_intrinsic_signature("sin")
        if (signature /= "real(real)") then
            print *, "sin signature incorrect"
            stop 1
        end if
        
        signature = get_intrinsic_signature("sqrt")
        if (signature /= "real(real)") then
            print *, "sqrt signature incorrect"
            stop 1
        end if
        
        signature = get_intrinsic_signature("abs")
        if (signature /= "real(real)") then
            print *, "abs signature incorrect"
            stop 1
        end if
        
        ! Test type conversion signatures
        signature = get_intrinsic_signature("int")
        if (signature /= "integer(real)") then
            print *, "int signature incorrect"
            stop 1
        end if
        
        signature = get_intrinsic_signature("real")
        if (signature /= "real(integer)") then
            print *, "real signature incorrect"
            stop 1
        end if
        
        ! Test array function signatures
        signature = get_intrinsic_signature("size")
        if (signature /= "integer(array)") then
            print *, "size signature incorrect"
            stop 1
        end if
        
        signature = get_intrinsic_signature("sum")
        if (signature /= "numeric(array)") then
            print *, "sum signature incorrect"
            stop 1
        end if
        
        ! Test string function signatures
        signature = get_intrinsic_signature("len")
        if (signature /= "integer(character)") then
            print *, "len signature incorrect"
            stop 1
        end if
        
        signature = get_intrinsic_signature("trim")
        if (signature /= "character(character)") then
            print *, "trim signature incorrect"
            stop 1
        end if
        
        print *, "  ✓ Intrinsic signature retrieval tests passed"
    end subroutine test_intrinsic_signature_retrieval

    subroutine test_case_insensitive_lookup()
        logical :: result
        character(len=:), allocatable :: signature
        
        print *, "Testing case insensitive lookup..."
        
        ! Test uppercase
        if (.not. is_intrinsic_function("SIN")) then
            print *, "SIN should be intrinsic"
            stop 1
        end if
        if (.not. is_intrinsic_function("COS")) then
            print *, "COS should be intrinsic"
            stop 1
        end if
        if (.not. is_intrinsic_function("SQRT")) then
            print *, "SQRT should be intrinsic"
            stop 1
        end if
        
        ! Test mixed case
        if (.not. is_intrinsic_function("Sin")) then
            print *, "Sin should be intrinsic"
            stop 1
        end if
        if (.not. is_intrinsic_function("CoS")) then
            print *, "CoS should be intrinsic"
            stop 1
        end if
        if (.not. is_intrinsic_function("SqRt")) then
            print *, "SqRt should be intrinsic"
            stop 1
        end if
        
        ! Test signature retrieval with different cases
        signature = get_intrinsic_signature("SIN")
        if (signature /= "real(real)") then
            print *, "SIN signature incorrect"
            stop 1
        end if
        
        signature = get_intrinsic_signature("Sin")
        if (signature /= "real(real)") then
            print *, "Sin signature incorrect"
            stop 1
        end if
        
        print *, "  ✓ Case insensitive lookup tests passed"
    end subroutine test_case_insensitive_lookup

    subroutine test_non_intrinsic_functions()
        logical :: result
        character(len=:), allocatable :: signature
        
        print *, "Testing non-intrinsic functions..."
        
        ! Test user-defined function names
        if (is_intrinsic_function("my_function")) then
            print *, "my_function should not be intrinsic"
            stop 1
        end if
        if (is_intrinsic_function("user_routine")) then
            print *, "user_routine should not be intrinsic"
            stop 1
        end if
        if (is_intrinsic_function("calculate")) then
            print *, "calculate should not be intrinsic"
            stop 1
        end if
        if (is_intrinsic_function("nonexistent")) then
            print *, "nonexistent should not be intrinsic"
            stop 1
        end if
        
        ! Test empty signature for non-intrinsic functions
        signature = get_intrinsic_signature("my_function")
        if (allocated(signature) .and. len_trim(signature) /= 0) then
            print *, "Non-intrinsic function should have empty signature"
            stop 1
        end if
        
        signature = get_intrinsic_signature("unknown")
        if (allocated(signature) .and. len_trim(signature) /= 0) then
            print *, "Unknown function should have empty signature"
            stop 1
        end if
        
        print *, "  ✓ Non-intrinsic function tests passed"
    end subroutine test_non_intrinsic_functions

    subroutine test_registry_initialization()
        logical :: is_intrinsic
        character(len=:), allocatable :: signature
        
        print *, "Testing registry initialization..."
        
        ! Force registry initialization through get_intrinsic_info
        call get_intrinsic_info("sin", is_intrinsic, signature)
        
        if (.not. is_intrinsic) then
            print *, "sin should be intrinsic after initialization"
            stop 1
        end if
        if (signature /= "real(real)") then
            print *, "sin signature incorrect after initialization"
            stop 1
        end if
        
        ! Test that multiple calls work correctly
        call get_intrinsic_info("cos", is_intrinsic, signature)
        if (.not. is_intrinsic) then
            print *, "cos should be intrinsic"
            stop 1
        end if
        if (signature /= "real(real)") then
            print *, "cos signature incorrect"
            stop 1
        end if
        
        ! Test with non-intrinsic
        call get_intrinsic_info("my_func", is_intrinsic, signature)
        if (is_intrinsic) then
            print *, "my_func should not be intrinsic"
            stop 1
        end if
        if (allocated(signature)) then
            if (len_trim(signature) > 0) then
                print *, "Non-intrinsic should not have non-empty signature"
                stop 1
            end if
        end if
        
        print *, "  ✓ Registry initialization tests passed"
    end subroutine test_registry_initialization

end program test_intrinsic_registry