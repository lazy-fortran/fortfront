program test_issue_254_parameter_declarations
    ! Test complex parameter declarations that cause parsing errors (Issue #254)
    use frontend, only: transform_lazy_fortran_string
    implicit none
    
    logical :: all_passed
    integer :: test_count, passed_count
    
    test_count = 0
    passed_count = 0
    all_passed = .true.
    
    print *, '=== Issue #254: Complex Parameter Declaration Tests ==='
    print *, 'Testing parameter declarations with attributes and initialization'
    print *
    
    ! Test 1: Basic parameter with integer type and initialization
    call run_test('Basic integer parameter declaration', test_basic_integer_parameter())
    
    ! Test 2: Real parameter with initialization
    call run_test('Real parameter with value initialization', test_real_parameter())
    
    ! Test 3: Character parameter with length specifier
    call run_test('Character parameter with length and value', test_character_parameter())
    
    ! Test 4: Array parameter with dimension and initialization
    call run_test('Array parameter with dimensions and values', test_array_parameter())
    
    ! Test 5: Multiple parameter declarations in sequence
    call run_test('Multiple parameter declarations together', test_multiple_parameters())
    
    ! Test 6: Parameter in program context (exact issue example)
    call run_test('Program with parameter declarations (exact issue)', test_exact_issue_example())
    
    ! Test 7: Parameter with complex expressions
    call run_test('Parameter with expression initialization', test_parameter_expressions())
    
    ! Test 8: Mixed parameter types and attributes
    call run_test('Mixed parameter types and attributes', test_mixed_parameter_types())
    
    ! Report results
    print *
    print *, 'Test Results:'
    print *, '  Total tests:', test_count
    print *, '  Tests passed:', passed_count
    print *, '  Tests failed:', test_count - passed_count
    print *
    
    if (passed_count == test_count) then
        print *, 'SUCCESS: All Issue #254 parameter declaration tests pass!'
        print *, 'Parameter declarations are working correctly'
        stop 0
    else
        print *, 'FAILURE: Some parameter declaration tests are failing'
        print *, 'Issue #254 parameter parsing problems still exist'
        stop 1
    end if

contains

    subroutine run_test(test_name, result)
        character(len=*), intent(in) :: test_name
        logical, intent(in) :: result
        
        test_count = test_count + 1
        if (result) then
            passed_count = passed_count + 1
            print *, '  PASS:', test_name
        else
            print *, '  FAIL:', test_name
        end if
    end subroutine

    function test_basic_integer_parameter() result(success)
        logical :: success
        character(len=:), allocatable :: input, output, error_msg
        
        ! Basic integer parameter declaration
        input = 'integer, parameter :: n = 10'
        
        call transform_lazy_fortran_string(input, output, error_msg)
        
        ! Should succeed without errors and include parameter in output
        success = (len_trim(error_msg) == 0) .and. &
                  (index(output, 'parameter') > 0) .and. &
                  (index(output, 'n') > 0) .and. &
                  (index(output, '10') > 0)
        
        if (.not. success) then
            print *, '    Error:', error_msg
            print *, '    Output length:', len_trim(output)
        end if
    end function

    function test_real_parameter() result(success)
        logical :: success
        character(len=:), allocatable :: input, output, error_msg
        
        ! Real parameter with initialization
        input = 'real, parameter :: pi = 3.14159'
        
        call transform_lazy_fortran_string(input, output, error_msg)
        
        ! Should succeed without errors and include parameter in output
        success = (len_trim(error_msg) == 0) .and. &
                  (index(output, 'parameter') > 0) .and. &
                  (index(output, 'pi') > 0) .and. &
                  (index(output, '3.14159') > 0)
        
        if (.not. success) then
            print *, '    Error:', error_msg
            print *, '    Output length:', len_trim(output)
        end if
    end function

    function test_character_parameter() result(success)
        logical :: success
        character(len=:), allocatable :: input, output, error_msg
        
        ! Character parameter with length specifier
        input = 'character(len=*), parameter :: name = "test"'
        
        call transform_lazy_fortran_string(input, output, error_msg)
        
        ! Should succeed without errors and include parameter in output
        success = (len_trim(error_msg) == 0) .and. &
                  (index(output, 'parameter') > 0) .and. &
                  (index(output, 'name') > 0) .and. &
                  (index(output, 'test') > 0)
        
        if (.not. success) then
            print *, '    Error:', error_msg
            print *, '    Output length:', len_trim(output)
        end if
    end function

    function test_array_parameter() result(success)
        logical :: success
        character(len=:), allocatable :: input, output, error_msg
        
        ! Array parameter with dimension and initialization
        input = 'integer, parameter, dimension(3) :: sizes = [1, 2, 3]'
        
        call transform_lazy_fortran_string(input, output, error_msg)
        
        ! Should succeed without errors and include parameter in output
        ! Note: dimension(3) attribute gets normalized to array syntax sizes(3)
        success = (len_trim(error_msg) == 0) .and. &
                  (index(output, 'parameter') > 0) .and. &
                  (index(output, 'sizes') > 0) .and. &
                  (index(output, '(3)') > 0)
        
        if (.not. success) then
            print *, '    Error:', error_msg
            print *, '    Output length:', len_trim(output)
        end if
    end function

    function test_multiple_parameters() result(success)
        logical :: success
        character(len=:), allocatable :: input, output, error_msg
        
        ! Multiple parameter declarations
        input = 'integer, parameter :: n = 10' // new_line('A') // &
                'real, parameter :: pi = 3.14159' // new_line('A') // &
                'character(len=*), parameter :: name = "fortfront"'
        
        call transform_lazy_fortran_string(input, output, error_msg)
        
        ! Should succeed without errors and include all parameters
        success = (len_trim(error_msg) == 0) .and. &
                  (index(output, 'parameter') > 0) .and. &
                  (index(output, 'n') > 0) .and. &
                  (index(output, 'pi') > 0) .and. &
                  (index(output, 'name') > 0) .and. &
                  (index(output, 'fortfront') > 0)
        
        if (.not. success) then
            print *, '    Error:', error_msg
            print *, '    Output length:', len_trim(output)
        end if
    end function

    function test_exact_issue_example() result(success)
        logical :: success
        character(len=:), allocatable :: input, output, error_msg
        
        ! Exact example from Issue #254
        input = 'program test_params' // new_line('A') // &
                '    implicit none' // new_line('A') // &
                '    integer, parameter :: n = 10' // new_line('A') // &
                '    real, parameter :: pi = 3.14159' // new_line('A') // &
                '    print *, n, pi' // new_line('A') // &
                'end program test_params'
        
        call transform_lazy_fortran_string(input, output, error_msg)
        
        ! Should succeed without errors and include program structure
        success = (len_trim(error_msg) == 0) .and. &
                  (index(output, 'program') > 0) .and. &
                  (index(output, 'parameter') > 0) .and. &
                  (index(output, 'n') > 0) .and. &
                  (index(output, 'pi') > 0) .and. &
                  (index(output, 'print') > 0)
        
        if (.not. success) then
            print *, '    Error:', error_msg
            print *, '    Output length:', len_trim(output)
        end if
    end function

    function test_parameter_expressions() result(success)
        logical :: success
        character(len=:), allocatable :: input, output, error_msg
        
        ! Parameter with complex expression initialization
        input = 'integer, parameter :: result = 2 * 5 + 3'
        
        call transform_lazy_fortran_string(input, output, error_msg)
        
        ! Should succeed without errors and include parameter
        success = (len_trim(error_msg) == 0) .and. &
                  (index(output, 'parameter') > 0) .and. &
                  (index(output, 'result') > 0)
        
        if (.not. success) then
            print *, '    Error:', error_msg
            print *, '    Output length:', len_trim(output)
        end if
    end function

    function test_mixed_parameter_types() result(success)
        logical :: success
        character(len=:), allocatable :: input, output, error_msg
        
        ! Mixed parameter types with different attributes
        input = 'program mixed_params' // new_line('A') // &
                '    implicit none' // new_line('A') // &
                '    integer, parameter :: count = 5' // new_line('A') // &
                '    real(kind=8), parameter :: tolerance = 1.0e-6' // new_line('A') // &
                '    logical, parameter :: debug = .true.' // new_line('A') // &
                '    character(len=10), parameter :: version = "1.0.0"' // new_line('A') // &
                '    print *, count, tolerance, debug, version' // new_line('A') // &
                'end program mixed_params'
        
        call transform_lazy_fortran_string(input, output, error_msg)
        
        ! Should succeed without errors and include all parameters
        success = (len_trim(error_msg) == 0) .and. &
                  (index(output, 'program') > 0) .and. &
                  (index(output, 'parameter') > 0) .and. &
                  (index(output, 'count') > 0) .and. &
                  (index(output, 'tolerance') > 0) .and. &
                  (index(output, 'debug') > 0) .and. &
                  (index(output, 'version') > 0)
        
        if (.not. success) then
            print *, '    Error:', error_msg
            print *, '    Output length:', len_trim(output)
        end if
    end function

end program test_issue_254_parameter_declarations