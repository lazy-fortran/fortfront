program test_do_loop_codegen_issue
    ! Test that do loops generate correct code, not broken declarations
    use frontend, only: compile_source, compilation_options_t
    use iso_fortran_env, only: error_unit
    implicit none
    
    integer :: unit, iostat, i
    character(len=256) :: line
    character(len=:), allocatable :: input_file, output_file
    character(len=256) :: error_msg
    type(compilation_options_t) :: options
    logical :: test_passed
    character(len=:), allocatable :: test_name, expected, actual_output
    integer :: line_count
    
    print *, "=== Testing Do Loop Code Generation - CRITICAL ISSUE ==="
    
    ! Test 1: Simple do loop
    print *, ""
    print *, "Test 1: Simple do loop (do i = 1, 10)"
    input_file = 'test_simple_do.f90'
    output_file = 'test_simple_do_out.f90'
    
    open(newunit=unit, file=input_file, status='replace')
    write(unit, '(a)') 'program test'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') '  integer :: i'
    write(unit, '(a)') '  do i = 1, 10'
    write(unit, '(a)') '    print *, i'
    write(unit, '(a)') '  end do'
    write(unit, '(a)') 'end program test'
    close(unit)
    
    options%output_file = output_file
    call compile_source(input_file, options, error_msg)
    
    if (len_trim(error_msg) > 0) then
        print *, '  FAIL: Compilation error:', trim(error_msg)
        stop 1
    end if
    
    ! Check output
    test_passed = .false.
    actual_output = ""
    open(newunit=unit, file=output_file, status='old', iostat=iostat)
    if (iostat /= 0) then
        print *, '  FAIL: Cannot open output file'
        stop 1
    end if
    
    line_count = 0
    do
        read(unit, '(a)', iostat=iostat) line
        if (iostat /= 0) exit
        line_count = line_count + 1
        actual_output = actual_output // trim(adjustl(line)) // new_line('A')
        
        ! Check for correct do loop
        if (index(adjustl(line), 'do i = 1, 10') > 0) then
            print *, '  ✓ Found correct do loop:', trim(line)
            test_passed = .true.
        end if
        
        ! Check for WRONG output (what Patrick saw)
        if (index(adjustl(line), 'integer :: do') > 0) then
            print *, '  ✗ ERROR: Found broken declaration:', trim(line)
            test_passed = .false.
        end if
    end do
    close(unit)
    
    if (.not. test_passed) then
        print *, '  FAIL: Do loop not generated correctly!'
        print *, '  Actual output:'
        print *, trim(actual_output)
        stop 1
    end if
    
    ! Test 2: Do loop with variable bounds
    print *, ""
    print *, "Test 2: Do loop with variables (do i = 1, n)"
    input_file = 'test_var_do.f90'
    output_file = 'test_var_do_out.f90'
    
    open(newunit=unit, file=input_file, status='replace')
    write(unit, '(a)') 'program test'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') '  integer :: i, n'
    write(unit, '(a)') '  n = 10'
    write(unit, '(a)') '  do i = 1, n'
    write(unit, '(a)') '    print *, i'
    write(unit, '(a)') '  end do'
    write(unit, '(a)') 'end program test'
    close(unit)
    
    options%output_file = output_file
    call compile_source(input_file, options, error_msg)
    
    if (len_trim(error_msg) > 0) then
        print *, '  FAIL: Compilation error:', trim(error_msg)
        stop 1
    end if
    
    ! Check output
    test_passed = .false.
    open(newunit=unit, file=output_file, status='old', iostat=iostat)
    if (iostat /= 0) then
        print *, '  FAIL: Cannot open output file'
        stop 1
    end if
    
    do
        read(unit, '(a)', iostat=iostat) line
        if (iostat /= 0) exit
        
        if (index(adjustl(line), 'do i = 1, n') > 0) then
            print *, '  ✓ Found correct do loop:', trim(line)
            test_passed = .true.
        end if
        
        if (index(adjustl(line), 'integer :: do') > 0) then
            print *, '  ✗ ERROR: Found broken declaration:', trim(line)
            test_passed = .false.
        end if
    end do
    close(unit)
    
    if (.not. test_passed) then
        print *, '  FAIL: Do loop with variables not generated correctly!'
        stop 1
    end if
    
    ! Test 3: Do loop with expressions (CRITICAL TEST)
    print *, ""
    print *, "Test 3: Do loop with expressions (do i = n-5, n+5) - CRITICAL"
    input_file = 'test_expr_do.f90'
    output_file = 'test_expr_do_out.f90'
    
    open(newunit=unit, file=input_file, status='replace')
    write(unit, '(a)') 'program test'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') '  integer :: i, n'
    write(unit, '(a)') '  n = 10'
    write(unit, '(a)') '  do i = n-5, n+5'
    write(unit, '(a)') '    print *, i'
    write(unit, '(a)') '  end do'
    write(unit, '(a)') 'end program test'
    close(unit)
    
    options%output_file = output_file
    call compile_source(input_file, options, error_msg)
    
    if (len_trim(error_msg) > 0) then
        print *, '  FAIL: Compilation error:', trim(error_msg)
        stop 1
    end if
    
    ! Check output
    test_passed = .false.
    actual_output = ""
    open(newunit=unit, file=output_file, status='old', iostat=iostat)
    if (iostat /= 0) then
        print *, '  FAIL: Cannot open output file'
        stop 1
    end if
    
    do
        read(unit, '(a)', iostat=iostat) line
        if (iostat /= 0) exit
        actual_output = actual_output // trim(adjustl(line)) // new_line('A')
        
        ! Look for proper do loop with expressions
        if (index(adjustl(line), 'do i =') > 0 .and. &
            (index(line, 'n-5') > 0 .or. index(line, 'n - 5') > 0)) then
            print *, '  ✓ Found do loop with expressions:', trim(line)
            test_passed = .true.
        end if
        
        if (index(adjustl(line), 'integer :: do') > 0) then
            print *, '  ✗ ERROR: Found broken declaration:', trim(line)
            test_passed = .false.
        end if
    end do
    close(unit)
    
    if (.not. test_passed) then
        print *, '  FAIL: Do loop with expressions not generated correctly!'
        print *, '  Actual output:'
        print *, trim(actual_output)
        stop 1
    end if
    
    print *, ""
    print *, "ALL TESTS PASSED! Do loops generate correct code!"
    
end program test_do_loop_codegen_issue