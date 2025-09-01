program test_module_parsing_bug_red
    use frontend, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed
    
    all_passed = .true.
    
    print *, '=== Module Parsing Bug RED Tests (Issue #253) ==='
    print *, 'These tests demonstrate the bug where functions inside modules'
    print *, 'are incorrectly moved outside and wrapped in programs'
    print *
    
    if (.not. test_simple_module_function_stays_inside()) all_passed = .false.
    if (.not. test_module_with_multiple_procedures()) all_passed = .false.
    if (.not. test_module_with_declarations_and_procedures()) all_passed = .false.
    
    print *
    if (all_passed) then
        print *, 'SUCCESS: All tests pass - Issue #253 has been fixed!'
    else
        print *, 'Module parsing bug tests completed - failures expected until fix'
        stop 1
    end if

contains

    ! Test 1: Simple module with one function should keep function inside
    logical function test_simple_module_function_stays_inside()
        character(len=*), parameter :: input = &
            'module test_mod' // new_line('a') // &
            '    implicit none' // new_line('a') // &
            'contains' // new_line('a') // &
            '    function add_numbers(a, b) result(sum)' // new_line('a') // &
            '        integer, intent(in) :: a, b' // new_line('a') // &
            '        integer :: sum' // new_line('a') // &
            '        sum = a + b' // new_line('a') // &
            '    end function add_numbers' // new_line('a') // &
            'end module test_mod'
        
        character(len=:), allocatable :: output, error_msg
        
        print *, 'Test 1: Simple module with function should keep function inside'
        
        ! Transform (standardization is automatic in the pipeline)
        call transform_lazy_fortran_string(input, output, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, 'Compilation error: ', trim(error_msg)
            test_simple_module_function_stays_inside = .false.
            return
        end if
        
        ! Check if function is incorrectly wrapped in program (the bug)
        if (index(output, 'program main') > 0) then
            print *, 'BUG CONFIRMED: Function incorrectly wrapped in program'
            print *, 'Output contains: program main (should not be there)'
            test_simple_module_function_stays_inside = .false.
            return
        end if
        
        ! Check if function is still inside module
        if (index(output, 'contains') > 0 .and. &
            index(output, 'function add_numbers') > 0 .and. &
            index(output, 'end module test_mod') > 0) then
            print *, 'PASS: Function correctly stays inside module'
            test_simple_module_function_stays_inside = .true.
        else
            print *, 'FAIL: Function structure not preserved'
            test_simple_module_function_stays_inside = .false.
        end if
    end function

    ! Test 2: Module with multiple procedures
    logical function test_module_with_multiple_procedures()
        character(len=*), parameter :: input = &
            'module math_mod' // new_line('a') // &
            '    implicit none' // new_line('a') // &
            'contains' // new_line('a') // &
            '    function add(a, b) result(c)' // new_line('a') // &
            '        integer :: a, b, c' // new_line('a') // &
            '        c = a + b' // new_line('a') // &
            '    end function add' // new_line('a') // &
            '    subroutine print_sum(a, b)' // new_line('a') // &
            '        integer :: a, b' // new_line('a') // &
            '        print *, a + b' // new_line('a') // &
            '    end subroutine print_sum' // new_line('a') // &
            'end module math_mod'
        
        character(len=:), allocatable :: output, error_msg
        
        print *, 'Test 2: Module with multiple procedures'
        
        ! Transform (standardization is automatic in the pipeline)
        call transform_lazy_fortran_string(input, output, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, 'Compilation error: ', trim(error_msg)
            test_module_with_multiple_procedures = .false.
            return
        end if
        
        ! Check if procedures are incorrectly wrapped in programs (the bug)
        if (index(output, 'program main') > 0) then
            print *, 'BUG CONFIRMED: Procedures incorrectly wrapped in program'
            test_module_with_multiple_procedures = .false.
            return
        end if
        
        ! Check if all procedures stay inside module
        if (index(output, 'contains') > 0 .and. &
            index(output, 'function add') > 0 .and. &
            index(output, 'subroutine print_sum') > 0 .and. &
            index(output, 'end module math_mod') > 0) then
            print *, 'PASS: All procedures correctly stay inside module'
            test_module_with_multiple_procedures = .true.
        else
            print *, 'FAIL: Module structure not preserved'
            test_module_with_multiple_procedures = .false.
        end if
    end function

    ! Test 3: Module with declarations and procedures
    logical function test_module_with_declarations_and_procedures()
        character(len=*), parameter :: input = &
            'module data_mod' // new_line('a') // &
            '    implicit none' // new_line('a') // &
            '    integer, parameter :: n = 10' // new_line('a') // &
            '    real :: x = 1.0' // new_line('a') // &
            'contains' // new_line('a') // &
            '    function get_n() result(result_n)' // new_line('a') // &
            '        integer :: result_n' // new_line('a') // &
            '        result_n = n' // new_line('a') // &
            '    end function get_n' // new_line('a') // &
            'end module data_mod'
        
        character(len=:), allocatable :: output, error_msg
        
        print *, 'Test 3: Module with declarations and procedures'
        
        ! Transform (standardization is automatic in the pipeline)
        call transform_lazy_fortran_string(input, output, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, 'Compilation error: ', trim(error_msg)
            test_module_with_declarations_and_procedures = .false.
            return
        end if
        
        ! Check if function is incorrectly wrapped in program (the bug)
        if (index(output, 'program main') > 0) then
            print *, 'BUG CONFIRMED: Function incorrectly wrapped in program'
            test_module_with_declarations_and_procedures = .false.
            return
        end if
        
        ! Check if module structure is preserved (declarations + procedures)
        if (index(output, 'n = 10') > 0 .and. &
            index(output, 'x = 1.0') > 0 .and. &
            index(output, 'contains') > 0 .and. &
            index(output, 'function get_n') > 0 .and. &
            index(output, 'end module data_mod') > 0) then
            print *, 'PASS: Module structure with declarations and procedures preserved'
            test_module_with_declarations_and_procedures = .true.
        else
            print *, 'FAIL: Module structure not preserved'
            test_module_with_declarations_and_procedures = .false.
        end if
    end function

end program test_module_parsing_bug_red