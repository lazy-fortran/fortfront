program test_issue_320_function_variable_declarations
    ! Comprehensive BDD tests for Issue #320: "Variables not declared in function"
    ! Tests function and subroutine variable declaration generation in standardizer
    
    use frontend, only: transform_lazy_fortran_string
    implicit none
    
    logical :: all_tests_passed
    
    print *, "=== Issue #320 Function Variable Declaration Tests ==="
    
    all_tests_passed = .true.
    
    ! Test 1: Function result variable declaration (exact Issue #320 example)
    if (.not. test_function_result_variable_declaration()) all_tests_passed = .false.
    
    ! Test 2: Local variables in function bodies 
    if (.not. test_function_local_variable_declarations()) all_tests_passed = .false.
    
    ! Test 3: Subroutine variable declarations
    if (.not. test_subroutine_variable_declarations()) all_tests_passed = .false.
    
    ! Test 4: Parameter type preservation
    if (.not. test_parameter_type_preservation()) all_tests_passed = .false.
    
    ! Test 5: Mixed variable types in functions
    if (.not. test_mixed_variable_types()) all_tests_passed = .false.
    
    ! Test 6: Functions with no variables (no spurious declarations)
    if (.not. test_function_with_no_variables()) all_tests_passed = .false.
    
    ! Test 7: Functions with existing declarations (no duplicates)
    if (.not. test_function_with_existing_declarations()) all_tests_passed = .false.
    
    ! Test 8: Complex function with multiple variable types
    if (.not. test_complex_function_declarations()) all_tests_passed = .false.
    
    ! Test 9: Function with array variables
    if (.not. test_function_with_array_variables()) all_tests_passed = .false.
    
    ! Test 10: Function with character variables and length inference
    if (.not. test_function_with_character_variables()) all_tests_passed = .false.
    
    if (all_tests_passed) then
        print *, "All Issue #320 function variable declaration tests passed!"
        stop 0
    else
        print *, "Some Issue #320 function variable declaration tests failed!"
        stop 1
    end if

contains

    function test_function_result_variable_declaration() result(passed)
        ! GIVEN a function with undeclared result variable
        ! WHEN standardization is applied  
        ! THEN the result variable gets proper declaration
        
        logical :: passed
        character(len=*), parameter :: source = &
            "function twice(x) result(y)" // char(10) // &
            "y = 2*x" // char(10) // &
            "end function"
        character(len=:), allocatable :: result_code, error_msg
        
        passed = .true.
        print *, "Given: function with undeclared result variable"
        
        print *, "When: standardization is applied"
        call transform_lazy_fortran_string(source, result_code, error_msg)
        if (len_trim(error_msg) > 0) then
            passed = .false.
            print *, "FAIL: Compilation error: ", trim(error_msg)
            return
        end if
        
        print *, "Then: result variable gets proper declaration"
        
        ! Check that result variable y is declared
        if (index(result_code, "real(8) :: y") == 0) then
            passed = .false.
            print *, "FAIL: Result variable 'y' not declared as real(8)"
        end if
        
        ! Check that parameter x is declared with intent(in)
        if (index(result_code, "real(8), intent(in) :: x") == 0) then
            passed = .false.
            print *, "FAIL: Parameter 'x' not declared with intent(in)"
        end if
        
        ! Check that implicit none is present
        if (index(result_code, "implicit none") == 0) then
            passed = .false.
            print *, "FAIL: 'implicit none' not added"
        end if
        
        if (passed) then
            print *, "PASS: test_function_result_variable_declaration"
        else
            print *, "FAIL: test_function_result_variable_declaration"
            print *, "Generated code:"
            print *, trim(result_code)
        end if
    end function test_function_result_variable_declaration
    
    function test_function_local_variable_declarations() result(passed)
        ! GIVEN a function with undeclared local variables
        ! WHEN standardization is applied
        ! THEN local variables get proper declarations
        
        logical :: passed
        character(len=*), parameter :: source = &
            "function calculate(a, b)" // char(10) // &
            "temp = a + b" // char(10) // &
            "calculate = temp * 2" // char(10) // &
            "end function"
        character(len=:), allocatable :: result_code, error_msg
        
        passed = .true.
        print *, "Given: function with undeclared local variables"
        
        call transform_lazy_fortran_string(source, result_code, error_msg)
        if (len_trim(error_msg) > 0) then
            passed = .false.
            print *, "FAIL: Compilation error: ", trim(error_msg)
            return
        end if
        
        print *, "When: standardization is applied"
        print *, "Then: local variables get proper declarations"
        
        ! Check that local variable temp is declared
        if (index(result_code, "real(8) :: temp") == 0) then
            passed = .false.
            print *, "FAIL: Local variable 'temp' not declared"
        end if
        
        ! Check that function result variable is declared
        if (index(result_code, "real(8) :: calculate") == 0) then
            passed = .false.
            print *, "FAIL: Function result variable 'calculate' not declared"
        end if
        
        if (passed) then
            print *, "PASS: test_function_local_variable_declarations"
        else
            print *, "FAIL: test_function_local_variable_declarations"
            print *, "Generated code:"
            print *, trim(result_code)
        end if
    end function test_function_local_variable_declarations
    
    function test_subroutine_variable_declarations() result(passed)
        ! GIVEN a subroutine with undeclared local variables
        ! WHEN standardization is applied
        ! THEN local variables get proper declarations
        
        logical :: passed
        character(len=*), parameter :: source = &
            "subroutine process(input, output)" // char(10) // &
            "temp = input * 2" // char(10) // &
            "output = temp + 1" // char(10) // &
            "end subroutine"
        character(len=:), allocatable :: result_code, error_msg
        
        passed = .true.
        print *, "Given: subroutine with undeclared local variables"
        
        call transform_lazy_fortran_string(source, result_code, error_msg)
        if (len_trim(error_msg) > 0) then
            passed = .false.
            print *, "FAIL: Compilation error: ", trim(error_msg)
            return
        end if
        
        print *, "When: standardization is applied"
        print *, "Then: local variables get proper declarations"
        
        ! Check that local variable temp is declared
        if (index(result_code, "real(8) :: temp") == 0) then
            passed = .false.
            print *, "FAIL: Local variable 'temp' not declared in subroutine"
        end if
        
        ! Check that parameters have intent declarations
        if (index(result_code, "intent(in)") == 0 .or. index(result_code, "intent(out)") == 0) then
            passed = .false.
            print *, "FAIL: Parameters missing proper intent declarations"
        end if
        
        if (passed) then
            print *, "PASS: test_subroutine_variable_declarations"
        else
            print *, "FAIL: test_subroutine_variable_declarations"
            print *, "Generated code:"
            print *, trim(result_code)
        end if
    end function test_subroutine_variable_declarations
    
    function test_parameter_type_preservation() result(passed)
        ! GIVEN a function with typed parameters
        ! WHEN standardization is applied
        ! THEN parameter types are preserved and not overwritten
        
        logical :: passed
        character(len=*), parameter :: source = &
            "function add_int(i, j)" // char(10) // &
            "integer :: i, j" // char(10) // &
            "add_int = i + j" // char(10) // &
            "end function"
        character(len=:), allocatable :: result_code, error_msg
        
        passed = .true.
        print *, "Given: function with typed parameters"
        
        call transform_lazy_fortran_string(source, result_code, error_msg)
        if (len_trim(error_msg) > 0) then
            passed = .false.
            print *, "FAIL: Compilation error: ", trim(error_msg)
            return
        end if
        
        print *, "When: standardization is applied"
        print *, "Then: parameter types are preserved"
        
        ! Check that integer parameters are preserved
        if (index(result_code, "integer") == 0) then
            passed = .false.
            print *, "FAIL: Integer parameter types not preserved"
        end if
        
        ! Check that function result gets proper type
        if (index(result_code, "integer :: add_int") == 0) then
            passed = .false.
            print *, "FAIL: Function result variable not declared with correct type"
        end if
        
        if (passed) then
            print *, "PASS: test_parameter_type_preservation"
        else
            print *, "FAIL: test_parameter_type_preservation"
            print *, "Generated code:"
            print *, trim(result_code)
        end if
    end function test_parameter_type_preservation
    
    function test_mixed_variable_types() result(passed)
        ! GIVEN a function with variables of different types
        ! WHEN standardization is applied
        ! THEN all variable types are properly declared
        
        logical :: passed
        character(len=*), parameter :: source = &
            'function mixed_calc(n)' // char(10) // &
            'integer :: n' // char(10) // &
            'value = n * 3.14' // char(10) // &
            'is_positive = value > 0' // char(10) // &
            'name = "result"' // char(10) // &
            'mixed_calc = 42' // char(10) // &
            'end function'
        character(len=:), allocatable :: result_code, error_msg
        
        passed = .true.
        print *, "Given: function with variables of different types"
        
        call transform_lazy_fortran_string(source, result_code, error_msg)
        if (len_trim(error_msg) > 0) then
            passed = .false.
            print *, "FAIL: Compilation error: ", trim(error_msg)
            return
        end if
        
        print *, "When: standardization is applied"
        print *, "Then: all variable types are properly declared"
        
        ! Check for integer parameter
        if (index(result_code, "integer") == 0) then
            passed = .false.
            print *, "FAIL: Integer type not found"
        end if
        
        ! Check for real variable
        if (index(result_code, "real(8)") == 0) then
            passed = .false.
            print *, "FAIL: Real(8) type not found"
        end if
        
        ! Check for logical variable  
        if (index(result_code, "logical") == 0) then
            passed = .false.
            print *, "FAIL: Logical type not found"
        end if
        
        ! Check for character variable
        if (index(result_code, "character") == 0) then
            passed = .false.
            print *, "FAIL: Character type not found"
        end if
        
        if (passed) then
            print *, "PASS: test_mixed_variable_types"
        else
            print *, "FAIL: test_mixed_variable_types"
            print *, "Generated code:"
            print *, trim(result_code)
        end if
    end function test_mixed_variable_types
    
    function test_function_with_no_variables() result(passed)
        ! GIVEN a function with no local variables
        ! WHEN standardization is applied
        ! THEN no spurious variable declarations are generated
        
        logical :: passed
        character(len=*), parameter :: source = &
            "function constant_value()" // char(10) // &
            "constant_value = 42" // char(10) // &
            "end function"
        character(len=:), allocatable :: result_code, error_msg
        integer :: decl_count
        
        passed = .true.
        print *, "Given: function with no local variables"
        
        call transform_lazy_fortran_string(source, result_code, error_msg)
        if (len_trim(error_msg) > 0) then
            passed = .false.
            print *, "FAIL: Compilation error: ", trim(error_msg)
            return
        end if
        
        print *, "When: standardization is applied"
        print *, "Then: no spurious variable declarations are generated"
        
        ! Count type declarations - should only have the function result
        decl_count = 0
        if (index(result_code, "integer :: constant_value") > 0) decl_count = decl_count + 1
        if (index(result_code, "real(8) :: constant_value") > 0) decl_count = decl_count + 1
        
        ! Should have exactly one declaration (the function result)
        if (decl_count /= 1) then
            passed = .false.
            print *, "FAIL: Expected exactly 1 declaration, found", decl_count
        end if
        
        ! Check for implicit none
        if (index(result_code, "implicit none") == 0) then
            passed = .false.
            print *, "FAIL: 'implicit none' not added"
        end if
        
        if (passed) then
            print *, "PASS: test_function_with_no_variables"
        else
            print *, "FAIL: test_function_with_no_variables"
            print *, "Generated code:"
            print *, trim(result_code)
        end if
    end function test_function_with_no_variables
    
    function test_function_with_existing_declarations() result(passed)
        ! GIVEN a function with some existing variable declarations
        ! WHEN standardization is applied
        ! THEN existing declarations are preserved, no duplicates created
        
        logical :: passed
        character(len=*), parameter :: source = &
            "function mixed_decl(x)" // char(10) // &
            "real :: x" // char(10) // &
            "temp = x * 2" // char(10) // &
            "mixed_decl = temp" // char(10) // &
            "end function"
        character(len=:), allocatable :: result_code, error_msg
        integer :: x_decl_count
        
        passed = .true.
        print *, "Given: function with some existing variable declarations"
        
        call transform_lazy_fortran_string(source, result_code, error_msg)
        if (len_trim(error_msg) > 0) then
            passed = .false.
            print *, "FAIL: Compilation error: ", trim(error_msg)
            return
        end if
        
        print *, "When: standardization is applied"
        print *, "Then: existing declarations preserved, no duplicates"
        
        ! Count declarations of x - should appear only once
        x_decl_count = 0
        block
            integer :: pos, start_pos
            start_pos = 1
            do while (start_pos <= len(result_code))
                pos = index(result_code(start_pos:), ":: x")
                if (pos > 0) then
                    x_decl_count = x_decl_count + 1
                    start_pos = start_pos + pos + 3
                else
                    exit
                end if
            end do
        end block
        
        if (x_decl_count /= 1) then
            passed = .false.
            print *, "FAIL: Variable 'x' declared", x_decl_count, "times (expected 1)"
        end if
        
        ! Check that temp gets declared
        if (index(result_code, ":: temp") == 0) then
            passed = .false.
            print *, "FAIL: Local variable 'temp' not declared"
        end if
        
        if (passed) then
            print *, "PASS: test_function_with_existing_declarations"
        else
            print *, "FAIL: test_function_with_existing_declarations"
            print *, "Generated code:"
            print *, trim(result_code)
        end if
    end function test_function_with_existing_declarations
    
    function test_complex_function_declarations() result(passed)
        ! GIVEN a complex function with multiple variable types and scopes
        ! WHEN standardization is applied
        ! THEN all variables get appropriate declarations in correct order
        
        logical :: passed
        character(len=*), parameter :: source = &
            'function complex_calc(n, factor)' // char(10) // &
            'integer :: n' // char(10) // &
            'base_value = n * factor' // char(10) // &
            'squared = base_value ** 2' // char(10) // &
            'label = "computed"' // char(10) // &
            'is_large = squared > 100' // char(10) // &
            'complex_calc = squared' // char(10) // &
            'end function'
        character(len=:), allocatable :: result_code, error_msg
        
        passed = .true.
        print *, "Given: complex function with multiple variable types"
        
        call transform_lazy_fortran_string(source, result_code, error_msg)
        if (len_trim(error_msg) > 0) then
            passed = .false.
            print *, "FAIL: Compilation error: ", trim(error_msg)
            return
        end if
        
        print *, "When: standardization is applied"
        print *, "Then: all variables get appropriate declarations"
        
        ! Check for all expected variable types
        if (index(result_code, "integer") == 0) then
            passed = .false.
            print *, "FAIL: Integer variables not declared"
        end if
        
        if (index(result_code, "real(8)") == 0) then
            passed = .false.
            print *, "FAIL: Real variables not declared"
        end if
        
        if (index(result_code, "character") == 0) then
            passed = .false.
            print *, "FAIL: Character variables not declared"
        end if
        
        if (index(result_code, "logical") == 0) then
            passed = .false.
            print *, "FAIL: Logical variables not declared"
        end if
        
        if (passed) then
            print *, "PASS: test_complex_function_declarations"
        else
            print *, "FAIL: test_complex_function_declarations"
            print *, "Generated code:"
            print *, trim(result_code)
        end if
    end function test_complex_function_declarations
    
    function test_function_with_array_variables() result(passed)
        ! GIVEN a function with array variables
        ! WHEN standardization is applied
        ! THEN array variables get proper array declarations
        
        logical :: passed
        character(len=*), parameter :: source = &
            "function sum_array()" // char(10) // &
            "arr(1) = 1" // char(10) // &
            "arr(2) = 2" // char(10) // &
            "arr(3) = 3" // char(10) // &
            "sum_array = arr(1) + arr(2) + arr(3)" // char(10) // &
            "end function"
        character(len=:), allocatable :: result_code, error_msg
        
        passed = .true.
        print *, "Given: function with array variables"
        
        call transform_lazy_fortran_string(source, result_code, error_msg)
        if (len_trim(error_msg) > 0) then
            passed = .false.
            print *, "FAIL: Compilation error: ", trim(error_msg)
            return
        end if
        
        print *, "When: standardization is applied"
        print *, "Then: array variables get proper declarations"
        
        ! Check for array declaration
        if (index(result_code, "arr") == 0) then
            passed = .false.
            print *, "FAIL: Array variable 'arr' not found in declarations"
        end if
        
        ! Should include proper type
        if (index(result_code, "real(8)") == 0 .and. index(result_code, "integer") == 0) then
            passed = .false.
            print *, "FAIL: Array variable missing proper type"
        end if
        
        if (passed) then
            print *, "PASS: test_function_with_array_variables"
        else
            print *, "FAIL: test_function_with_array_variables"
            print *, "Generated code:"
            print *, trim(result_code)
        end if
    end function test_function_with_array_variables
    
    function test_function_with_character_variables() result(passed)
        ! GIVEN a function with character variables
        ! WHEN standardization is applied
        ! THEN character variables get proper length specifications
        
        logical :: passed
        character(len=*), parameter :: source = &
            'function make_greeting(name)' // char(10) // &
            'character(*) :: name' // char(10) // &
            'prefix = "Hello, "' // char(10) // &
            'make_greeting = prefix // name' // char(10) // &
            'end function'
        character(len=:), allocatable :: result_code, error_msg
        
        passed = .true.
        print *, "Given: function with character variables"
        
        call transform_lazy_fortran_string(source, result_code, error_msg)
        if (len_trim(error_msg) > 0) then
            passed = .false.
            print *, "FAIL: Compilation error: ", trim(error_msg)
            return
        end if
        
        print *, "When: standardization is applied"
        print *, "Then: character variables get proper specifications"
        
        ! Check for character declarations
        if (index(result_code, "character") == 0) then
            passed = .false.
            print *, "FAIL: Character variables not declared"
        end if
        
        ! Check that existing character(*) parameter is preserved
        if (index(result_code, "character(*)") == 0 .and. index(result_code, "character(len=*)") == 0) then
            passed = .false.
            print *, "FAIL: Parameter character(*) declaration not preserved"
        end if
        
        if (passed) then
            print *, "PASS: test_function_with_character_variables"
        else
            print *, "FAIL: test_function_with_character_variables"
            print *, "Generated code:"
            print *, trim(result_code)
        end if
    end function test_function_with_character_variables

end program test_issue_320_function_variable_declarations