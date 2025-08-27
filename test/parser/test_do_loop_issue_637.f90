program test_do_loop_issue_637
    ! Test that parser handles do loops with expressions (Issue #637)
    use frontend, only: compile_source, compilation_options_t
    use iso_fortran_env, only: error_unit
    implicit none
    
    logical :: all_passed
    
    print *, "=== Testing Do Loop Expression Parsing (Issue #637) ==="
    
    all_passed = .true.
    all_passed = all_passed .and. test_simple_literals()
    all_passed = all_passed .and. test_with_variables()
    all_passed = all_passed .and. test_with_expressions()
    all_passed = all_passed .and. test_with_function_calls()
    all_passed = all_passed .and. test_complex_expressions()
    
    if (all_passed) then
        print *, "ALL TESTS PASSED - Issue #637 FIXED!"
        stop 0
    else
        print *, "TESTS FAILED - Issue #637 not fully resolved"
        stop 1
    end if
    
contains

    logical function test_simple_literals()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit
        
        test_simple_literals = .true.
        print *, "Test 1: Simple literals (do i = 1, 10)..."
        
        input_file = 'test_do_simple.f90'
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') 'program test'
        write(unit, '(a)') '  integer :: i'
        write(unit, '(a)') '  do i = 1, 10'
        write(unit, '(a)') '    print *, i'
        write(unit, '(a)') '  end do'
        write(unit, '(a)') 'end program test'
        close(unit)
        
        output_file = 'test_do_simple_out.f90'
        options%output_file = output_file
        
        call compile_source(input_file, options, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: ', trim(error_msg)
            test_simple_literals = .false.
        else
            print *, '  PASS: Simple literals work'
        end if
    end function
    
    logical function test_with_variables()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit
        
        test_with_variables = .true.
        print *, "Test 2: With variables (do i = 1, n)..."
        
        input_file = 'test_do_variables.f90'
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') 'program test'
        write(unit, '(a)') '  integer :: i, n'
        write(unit, '(a)') '  n = 10'
        write(unit, '(a)') '  do i = 1, n'
        write(unit, '(a)') '    print *, i'
        write(unit, '(a)') '  end do'
        write(unit, '(a)') 'end program test'
        close(unit)
        
        output_file = 'test_do_variables_out.f90'
        options%output_file = output_file
        
        call compile_source(input_file, options, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: ', trim(error_msg)
            test_with_variables = .false.
        else
            print *, '  PASS: Variables work'
        end if
    end function
    
    logical function test_with_expressions()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit
        
        test_with_expressions = .true.
        print *, "Test 3: With expressions (do i = n-5, n+5) - CRITICAL TEST..."
        
        input_file = 'test_do_expressions.f90'
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') 'program test'
        write(unit, '(a)') '  integer :: i, n'
        write(unit, '(a)') '  n = 10'
        write(unit, '(a)') '  do i = n-5, n+5'
        write(unit, '(a)') '    print *, i'
        write(unit, '(a)') '  end do'
        write(unit, '(a)') 'end program test'
        close(unit)
        
        output_file = 'test_do_expressions_out.f90'
        options%output_file = output_file
        
        call compile_source(input_file, options, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: ', trim(error_msg)
            test_with_expressions = .false.
        else
            print *, '  PASS: Expressions work - ISSUE #637 FIXED!'
        end if
    end function
    
    logical function test_with_function_calls()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit
        
        test_with_function_calls = .true.
        print *, "Test 4: With function calls (do i = 1, size(array))..."
        
        input_file = 'test_do_function.f90'
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') 'program test'
        write(unit, '(a)') '  integer :: i'
        write(unit, '(a)') '  integer :: array(10)'
        write(unit, '(a)') '  do i = 1, size(array)'
        write(unit, '(a)') '    print *, i'
        write(unit, '(a)') '  end do'
        write(unit, '(a)') 'end program test'
        close(unit)
        
        output_file = 'test_do_function_out.f90'
        options%output_file = output_file
        
        call compile_source(input_file, options, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: ', trim(error_msg)
            test_with_function_calls = .false.
        else
            print *, '  PASS: Function calls work'
        end if
    end function
    
    logical function test_complex_expressions()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit
        
        test_complex_expressions = .true.
        print *, "Test 5: Complex expressions (do i = n/2-5, n/2+5, step*2)..."
        
        input_file = 'test_do_complex.f90'
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') 'program test'
        write(unit, '(a)') '  integer :: i, n, step'
        write(unit, '(a)') '  n = 20'
        write(unit, '(a)') '  step = 1'
        write(unit, '(a)') '  do i = n/2-5, n/2+5, step*2'
        write(unit, '(a)') '    print *, i'
        write(unit, '(a)') '  end do'
        write(unit, '(a)') 'end program test'
        close(unit)
        
        output_file = 'test_do_complex_out.f90'
        options%output_file = output_file
        
        call compile_source(input_file, options, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: ', trim(error_msg)
            test_complex_expressions = .false.
        else
            print *, '  PASS: Complex expressions work'
        end if
    end function

end program test_do_loop_issue_637