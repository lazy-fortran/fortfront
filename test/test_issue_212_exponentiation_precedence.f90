program test_issue_212_exponentiation_precedence
    use frontend, only: compile_source, compilation_options_t
    implicit none
    
    logical :: all_passed
    
    all_passed = .true.
    
    print *, '=== Issue #212: Wrong grouping for exponentiation ==='
    
    if (.not. test_exponentiation_precedence()) all_passed = .false.
    
    print *
    if (all_passed) then
        print *, 'Issue #212 test passed!'
        stop 0
    else
        print *, 'Issue #212 test failed!'
        stop 1
    end if
    
contains
    
    logical function test_exponentiation_precedence()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg, line
        type(compilation_options_t) :: options
        integer :: unit, iostat
        logical :: has_correct_precedence
        
        test_exponentiation_precedence = .true.
        print *, 'Testing exponentiation precedence...'
        
        ! Create test input - exact code from issue #212
        input_file = 'test_issue_212.lf'
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') 'r = 10'
        write(unit, '(a)') 'area = 3.14 * r**2'
        write(unit, '(a)') 'print*,area'
        close(unit)
        
        ! Compile
        output_file = 'test_issue_212_out.f90'
        options%output_file = output_file
        
        call compile_source(input_file, options, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_exponentiation_precedence = .false.
            return
        end if
        
        ! Check that output file was actually created
        inquire(file=output_file, exist=has_correct_precedence)
        if (.not. has_correct_precedence) then
            print *, '  FAIL: Output file was not created:', trim(output_file)
            test_exponentiation_precedence = .false.
            return
        end if
        
        print *, '  INFO: Compilation succeeded, checking output in:', trim(output_file)
        
        ! Check generated code - should have "3.14d0*r**2", NOT "(3.14d0*r) ** 2"
        has_correct_precedence = .false.
        
        open(newunit=unit, file=output_file, status='old')
        do
            read(unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            
            ! Check for correct precedence (remove any trailing whitespace/line endings)
            if (index(trim(line), '3.14d0*r**2') > 0) then
                has_correct_precedence = .true.
                print *, '  OK: Found correct precedence: ', trim(line)
            end if
            
            ! Check for wrong grouping that indicates the bug
            if (index(trim(line), '(3.14d0*r) ** 2') > 0) then
                print *, '  FAIL: Found incorrect grouping: ', trim(line)
                test_exponentiation_precedence = .false.
                close(unit)
                return
            end if
        end do
        close(unit)
        
        if (.not. has_correct_precedence) then
            print *, '  FAIL: Did not find expected "3.14d0*r**2" in output'
            print *, '  Generated output was:'
            
            ! Re-read and dump the entire output for debugging
            open(newunit=unit, file=output_file, status='old')
            do
                read(unit, '(a)', iostat=iostat) line
                if (iostat /= 0) exit
                print *, '    ', trim(line)
            end do
            close(unit)
            
            test_exponentiation_precedence = .false.
        end if
        
    end function test_exponentiation_precedence
    
end program test_issue_212_exponentiation_precedence