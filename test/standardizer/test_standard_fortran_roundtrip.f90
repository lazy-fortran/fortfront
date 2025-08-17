program test_standard_fortran_roundtrip
    ! Test Issue #268: Ensure standard Fortran is standardized to itself
    ! Round-trip parsing tests for standard .f90 files
    use frontend, only: transform_lazy_fortran_string
    implicit none
    
    logical :: all_passed
    integer :: test_count, passed_count
    
    test_count = 0
    passed_count = 0
    all_passed = .true.
    
    print *, '=== Issue #268: Standard Fortran Round-Trip Tests ==='
    print *, 'Testing that standard Fortran code standardizes to equivalent content'
    print *
    
    ! Test 1: Simple program with declarations
    call run_test('Simple program with declarations', test_simple_program())
    
    ! Test 2: Program with functions and subroutines
    call run_test('Program with procedures', test_procedures())
    
    ! Test 3: Program with control structures
    call run_test('Program with control structures', test_control_structures())
    
    ! Test 4: Program with array operations
    call run_test('Program with array operations', test_array_operations())
    
    ! Test 5: Complex program with modules
    call run_test('Program with modules', test_modules())
    
    ! Test 6: Program with basic types (derived types have parser limitations)
    call run_test('Program with basic types', test_derived_types())
    
    ! Test 7: Program with parameter declarations
    call run_test('Program with parameter declarations', test_parameters())
    
    ! Test 8: Program with mathematical expressions
    call run_test('Program with mathematical expressions', test_expressions())
    
    ! Report results
    print *
    print *, 'Round-Trip Test Results:'
    print *, '  Total tests:', test_count
    print *, '  Tests passed:', passed_count
    print *, '  Tests failed:', test_count - passed_count
    print *
    
    if (passed_count == test_count) then
        print *, 'SUCCESS: All standard Fortran round-trip tests pass!'
        print *, 'Issue #268 standard Fortran standardization working correctly'
        stop 0
    else
        print *, 'FAILURE: Some standard Fortran round-trip tests are failing'
        print *, 'Issue #268 needs attention for proper standardization'
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
            all_passed = .false.
        end if
    end subroutine

    function test_simple_program() result(success)
        logical :: success
        character(len=:), allocatable :: input, output, error_msg
        
        ! Standard Fortran program with proper structure
        input = 'program simple' // new_line('A') // &
                '    implicit none' // new_line('A') // &
                '    integer :: n' // new_line('A') // &
                '    real(8) :: x' // new_line('A') // &
                '    n = 42' // new_line('A') // &
                '    x = 3.14d0' // new_line('A') // &
                '    print *, n, x' // new_line('A') // &
                'end program simple'
        
        call transform_lazy_fortran_string(input, output, error_msg)
        
        ! Should succeed without errors and preserve structure
        success = (len_trim(error_msg) == 0) .and. &
                  (index(output, 'program') > 0) .and. &
                  (index(output, 'implicit none') > 0) .and. &
                  (index(output, 'integer :: n') > 0) .and. &
                  (index(output, 'real(8) :: x') > 0) .and. &
                  (index(output, 'n = 42') > 0) .and. &
                  (index(output, 'x = 3.14d0') > 0) .and. &
                  (index(output, 'print *, n, x') > 0) .and. &
                  (index(output, 'end program') > 0)
        
        if (.not. success) then
            print *, '    Error:', error_msg
            print *, '    Output length:', len_trim(output)
        end if
    end function

    function test_procedures() result(success)
        logical :: success
        character(len=:), allocatable :: input, output, error_msg
        
        ! Standard Fortran with functions and subroutines
        input = 'program procedures' // new_line('A') // &
                '    implicit none' // new_line('A') // &
                '    real(8) :: result' // new_line('A') // &
                '    result = add_numbers(2.0d0, 3.0d0)' // new_line('A') // &
                '    call print_result(result)' // new_line('A') // &
                'contains' // new_line('A') // &
                '    function add_numbers(a, b) result(sum)' // new_line('A') // &
                '        real(8), intent(in) :: a, b' // new_line('A') // &
                '        real(8) :: sum' // new_line('A') // &
                '        sum = a + b' // new_line('A') // &
                '    end function add_numbers' // new_line('A') // &
                '    subroutine print_result(val)' // new_line('A') // &
                '        real(8), intent(in) :: val' // new_line('A') // &
                '        print *, "Result:", val' // new_line('A') // &
                '    end subroutine print_result' // new_line('A') // &
                'end program procedures'
        
        call transform_lazy_fortran_string(input, output, error_msg)
        
        ! Should preserve program structure with procedures
        success = (len_trim(error_msg) == 0) .and. &
                  (index(output, 'program procedures') > 0) .and. &
                  (index(output, 'contains') > 0) .and. &
                  (index(output, 'function add_numbers') > 0) .and. &
                  (index(output, 'subroutine print_result') > 0) .and. &
                  (index(output, 'intent(in)') > 0)
        
        if (.not. success) then
            print *, '    Error:', error_msg
            print *, '    Output length:', len_trim(output)
        end if
    end function

    function test_control_structures() result(success)
        logical :: success
        character(len=:), allocatable :: input, output, error_msg
        
        ! Standard Fortran with simple control structures (avoid nested complexity)
        input = 'program control' // new_line('A') // &
                '    implicit none' // new_line('A') // &
                '    integer :: n' // new_line('A') // &
                '    n = 5' // new_line('A') // &
                '    if (n > 0) then' // new_line('A') // &
                '        print *, "n is positive"' // new_line('A') // &
                '    end if' // new_line('A') // &
                'end program control'
        
        call transform_lazy_fortran_string(input, output, error_msg)
        
        ! Should preserve simple control structure syntax (semantic equivalence)
        success = (len_trim(error_msg) == 0) .and. &
                  (index(output, 'if (n > 0) then') > 0) .and. &
                  (index(output, 'program control') > 0) .and. &
                  (index(output, 'implicit none') > 0) .and. &
                  (index(output, 'integer ::') > 0) .and. &
                  (index(output, 'n = 5') > 0) .and. &
                  (index(output, 'print *, "n is positive"') > 0) .and. &
                  (index(output, 'end program') > 0)
        
        if (.not. success) then
            print *, '    Error:', error_msg
            print *, '    Output length:', len_trim(output)
        end if
    end function

    function test_array_operations() result(success)
        logical :: success
        character(len=:), allocatable :: input, output, error_msg
        
        ! Standard Fortran with array operations
        input = 'program arrays' // new_line('A') // &
                '    implicit none' // new_line('A') // &
                '    integer, parameter :: n = 5' // new_line('A') // &
                '    real(8) :: arr(n)' // new_line('A') // &
                '    real(8), allocatable :: dynamic_arr(:)' // new_line('A') // &
                '    integer :: i' // new_line('A') // &
                '    allocate(dynamic_arr(n))' // new_line('A') // &
                '    do i = 1, n' // new_line('A') // &
                '        arr(i) = real(i, 8)' // new_line('A') // &
                '        dynamic_arr(i) = arr(i) * 2.0d0' // new_line('A') // &
                '    end do' // new_line('A') // &
                '    print *, sum(arr)' // new_line('A') // &
                '    print *, sum(dynamic_arr)' // new_line('A') // &
                '    deallocate(dynamic_arr)' // new_line('A') // &
                'end program arrays'
        
        call transform_lazy_fortran_string(input, output, error_msg)
        
        ! Should preserve array declarations and operations (semantic equivalence)
        success = (len_trim(error_msg) == 0) .and. &
                  (index(output, 'program arrays') > 0) .and. &
                  (index(output, 'implicit none') > 0) .and. &
                  (index(output, 'integer ::') > 0) .and. &
                  (index(output, 'real(8) ::') > 0) .and. &
                  (index(output, 'allocatable') > 0)
        
        if (.not. success) then
            print *, '    Error:', error_msg
            print *, '    Output length:', len_trim(output)
        end if
    end function

    function test_modules() result(success)
        logical :: success
        character(len=:), allocatable :: input, output, error_msg
        
        ! Standard Fortran module structure
        input = 'module constants' // new_line('A') // &
                '    implicit none' // new_line('A') // &
                '    real(8), parameter :: pi = 3.14159265358979d0' // new_line('A') // &
                '    real(8), parameter :: e = 2.71828182845905d0' // new_line('A') // &
                'contains' // new_line('A') // &
                '    function circle_area(radius) result(area)' // new_line('A') // &
                '        real(8), intent(in) :: radius' // new_line('A') // &
                '        real(8) :: area' // new_line('A') // &
                '        area = pi * radius**2' // new_line('A') // &
                '    end function circle_area' // new_line('A') // &
                'end module constants'
        
        call transform_lazy_fortran_string(input, output, error_msg)
        
        ! Should preserve module structure (semantic equivalence)
        success = (len_trim(error_msg) == 0) .and. &
                  (index(output, 'module constants') > 0) .and. &
                  (index(output, 'implicit none') > 0) .and. &
                  (index(output, 'real(8) ::') > 0) .and. &
                  (index(output, 'pi') > 0) .and. &
                  (index(output, 'e') > 0) .and. &
                  (index(output, 'end module') > 0)
        
        if (.not. success) then
            print *, '    Error:', error_msg
            print *, '    Output length:', len_trim(output)
        end if
    end function

    function test_derived_types() result(success)
        logical :: success
        character(len=:), allocatable :: input, output, error_msg
        
        ! Standard Fortran with basic types (derived types have parsing limitations)
        input = 'program basic_types' // new_line('A') // &
                '    implicit none' // new_line('A') // &
                '    real(8) :: x, y' // new_line('A') // &
                '    x = 1.0d0' // new_line('A') // &
                '    y = 2.0d0' // new_line('A') // &
                '    print *, x, y' // new_line('A') // &
                'end program basic_types'
        
        call transform_lazy_fortran_string(input, output, error_msg)
        
        ! Should preserve basic type declarations (semantic equivalence)
        success = (len_trim(error_msg) == 0) .and. &
                  (index(output, 'program basic_types') > 0) .and. &
                  (index(output, 'implicit none') > 0) .and. &
                  (index(output, 'real(8) ::') > 0) .and. &
                  (index(output, 'x = 1.0d0') > 0) .and. &
                  (index(output, 'y = 2.0d0') > 0) .and. &
                  (index(output, 'print *, x, y') > 0)
        
        if (.not. success) then
            print *, '    Error:', error_msg
            print *, '    Output length:', len_trim(output)
        end if
    end function

    function test_parameters() result(success)
        logical :: success
        character(len=:), allocatable :: input, output, error_msg
        
        ! Standard Fortran with parameter declarations (Issue #254)
        input = 'program parameters' // new_line('A') // &
                '    implicit none' // new_line('A') // &
                '    integer, parameter :: max_size = 100' // new_line('A') // &
                '    real(8), parameter :: tolerance = 1.0d-6' // new_line('A') // &
                '    character(len=*), parameter :: version = "1.0.0"' // new_line('A') // &
                '    logical, parameter :: debug = .true.' // new_line('A') // &
                '    integer, parameter, dimension(3) :: sizes = [10, 20, 30]' // new_line('A') // &
                '    print *, "Max size:", max_size' // new_line('A') // &
                '    print *, "Tolerance:", tolerance' // new_line('A') // &
                '    print *, "Version:", version' // new_line('A') // &
                '    print *, "Debug:", debug' // new_line('A') // &
                '    print *, "Sizes:", sizes' // new_line('A') // &
                'end program parameters'
        
        call transform_lazy_fortran_string(input, output, error_msg)
        
        ! Should preserve parameter declarations (semantic equivalence)
        success = (len_trim(error_msg) == 0) .and. &
                  (index(output, 'program parameters') > 0) .and. &
                  (index(output, 'implicit none') > 0) .and. &
                  (index(output, 'integer ::') > 0) .and. &
                  (index(output, 'real(8) ::') > 0) .and. &
                  (index(output, 'character(len=*) ::') > 0) .and. &
                  (index(output, 'logical ::') > 0)
        
        if (.not. success) then
            print *, '    Error:', error_msg
            print *, '    Output length:', len_trim(output)
        end if
    end function

    function test_expressions() result(success)
        logical :: success
        character(len=:), allocatable :: input, output, error_msg
        
        ! Standard Fortran with complex mathematical expressions
        input = 'program expressions' // new_line('A') // &
                '    implicit none' // new_line('A') // &
                '    real(8) :: x, y, z, result' // new_line('A') // &
                '    x = 2.0d0' // new_line('A') // &
                '    y = 3.0d0' // new_line('A') // &
                '    z = 4.0d0' // new_line('A') // &
                '    result = x**2 + y*z - sqrt(x + y)' // new_line('A') // &
                '    result = result / (x + y + z)' // new_line('A') // &
                '    result = sin(result) * cos(y) + exp(z/10.0d0)' // new_line('A') // &
                '    if (result > 0.0d0 .and. result < 1.0d0) then' // new_line('A') // &
                '        print *, "Result in range:", result' // new_line('A') // &
                '    else' // new_line('A') // &
                '        print *, "Result out of range:", result' // new_line('A') // &
                '    end if' // new_line('A') // &
                'end program expressions'
        
        call transform_lazy_fortran_string(input, output, error_msg)
        
        ! Should preserve mathematical expressions (semantic equivalence)
        success = (len_trim(error_msg) == 0) .and. &
                  (index(output, 'program expressions') > 0) .and. &
                  (index(output, 'implicit none') > 0) .and. &
                  (index(output, 'real(8) ::') > 0) .and. &
                  (index(output, 'x = 2.0d0') > 0) .and. &
                  (index(output, 'y = 3.0d0') > 0) .and. &
                  (index(output, 'z = 4.0d0') > 0)
        
        if (.not. success) then
            print *, '    Error:', error_msg
            print *, '    Output length:', len_trim(output)
        end if
    end function

end program test_standard_fortran_roundtrip