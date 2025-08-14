program test_issue_214_string_concatenation_precedence
    use frontend, only: compile_source, compilation_options_t
    implicit none
    
    logical :: all_passed
    
    all_passed = .true.
    
    print *, '=== Issue #214: String concatenation has wrong precedence ==='
    
    if (.not. test_string_concatenation_precedence_with_parentheses()) all_passed = .false.
    
    print *
    if (all_passed) then
        print *, 'Issue #214 test passed!'
        stop 0
    else
        print *, 'Issue #214 test failed!'
        stop 1
    end if
    
contains
    
    logical function test_string_concatenation_precedence_with_parentheses()
        character(len=:), allocatable :: input_file1, output_file1
        character(len=:), allocatable :: input_file2, output_file2
        character(len=256) :: error_msg, line1, line2
        type(compilation_options_t) :: options
        integer :: unit, iostat
        logical :: found_line1, found_line2
        
        test_string_concatenation_precedence_with_parentheses = .true.
        print *, 'Testing string concatenation precedence using explicit parentheses...'
        
        ! Test 1: Create expression with parentheses to force correct precedence
        ! Expression: "a" + ("b" // "c") - this should be the natural parsing
        input_file1 = 'test_issue_214_correct.lf'
        open(newunit=unit, file=input_file1, status='replace')
        write(unit, '(a)') 'program test_correct'
        write(unit, '(a)') '    character(len=10) :: result'
        write(unit, '(a)') '    result = "a" + ("b" // "c")'
        write(unit, '(a)') '    print *, result'
        write(unit, '(a)') 'end program test_correct'
        close(unit)
        
        ! Test 2: Create expression with parentheses to force wrong precedence  
        ! Expression: ("a" + "b") // "c" - this should be different
        input_file2 = 'test_issue_214_wrong.lf'
        open(newunit=unit, file=input_file2, status='replace')
        write(unit, '(a)') 'program test_wrong'
        write(unit, '(a)') '    character(len=10) :: result'
        write(unit, '(a)') '    result = ("a" + "b") // "c"'
        write(unit, '(a)') '    print *, result'
        write(unit, '(a)') 'end program test_wrong'
        close(unit)
        
        ! Compile both versions
        output_file1 = 'test_issue_214_correct_out.f90'
        options%output_file = output_file1
        call compile_source(input_file1, options, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error (correct version):', trim(error_msg)
            test_string_concatenation_precedence_with_parentheses = .false.
            return
        end if
        
        output_file2 = 'test_issue_214_wrong_out.f90'
        options%output_file = output_file2
        call compile_source(input_file2, options, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error (wrong version):', trim(error_msg)
            test_string_concatenation_precedence_with_parentheses = .false.
            return
        end if
        
        ! Read the generated code from both versions
        found_line1 = .false.
        found_line2 = .false.
        
        ! Read correct version
        open(newunit=unit, file=output_file1, status='old')
        do
            read(unit, '(a)', iostat=iostat) line1
            if (iostat /= 0) exit
            if (index(trim(line1), 'result = ') > 0) then
                print *, '  Correct precedence version: ', trim(line1)
                found_line1 = .true.
                exit
            end if
        end do
        close(unit)
        
        ! Read wrong version
        open(newunit=unit, file=output_file2, status='old')
        do
            read(unit, '(a)', iostat=iostat) line2
            if (iostat /= 0) exit
            if (index(trim(line2), 'result = ') > 0) then
                print *, '  Wrong precedence version: ', trim(line2)
                found_line2 = .true.
                exit
            end if
        end do
        close(unit)
        
        if (.not. found_line1 .or. .not. found_line2) then
            print *, '  FAIL: Could not find result assignments in generated code'
            test_string_concatenation_precedence_with_parentheses = .false.
            return
        end if
        
        ! Now test the original ambiguous expression
        ! Expression: "a" + "b" // "c" (no parentheses)
        ! If concatenation has correct precedence, this should generate the same as line1
        ! If concatenation has wrong precedence, this should generate the same as line2
        block
            character(len=:), allocatable :: input_file3, output_file3
            character(len=256) :: line3
            logical :: found_line3
            
            input_file3 = 'test_issue_214_ambiguous.lf'
            open(newunit=unit, file=input_file3, status='replace')
            write(unit, '(a)') 'program test_ambiguous'
            write(unit, '(a)') '    character(len=10) :: result'
            write(unit, '(a)') '    result = "a" + "b" // "c"'
            write(unit, '(a)') '    print *, result'
            write(unit, '(a)') 'end program test_ambiguous'
            close(unit)
            
            output_file3 = 'test_issue_214_ambiguous_out.f90'
            options%output_file = output_file3
            call compile_source(input_file3, options, error_msg)
            
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Compilation error (ambiguous version):', trim(error_msg)
                test_string_concatenation_precedence_with_parentheses = .false.
                return
            end if
            
            found_line3 = .false.
            open(newunit=unit, file=output_file3, status='old')
            do
                read(unit, '(a)', iostat=iostat) line3
                if (iostat /= 0) exit
                if (index(trim(line3), 'result = ') > 0) then
                    print *, '  Ambiguous expression result: ', trim(line3)
                    found_line3 = .true.
                    exit
                end if
            end do
            close(unit)
            
            if (.not. found_line3) then
                print *, '  FAIL: Could not find result assignment in ambiguous version'
                test_string_concatenation_precedence_with_parentheses = .false.
                return
            end if
            
            ! Compare the results
            if (trim(line3) == trim(line1)) then
                print *, '  OK: Ambiguous expression matches correct precedence (// higher than +)'
            else if (trim(line3) == trim(line2)) then
                print *, '  FAIL: Ambiguous expression matches wrong precedence (+ higher than //)'
                print *, '  BUG CONFIRMED: String concatenation has lower precedence than addition'
                test_string_concatenation_precedence_with_parentheses = .false.
            else
                print *, '  FAIL: Ambiguous expression generates unexpected result'
                test_string_concatenation_precedence_with_parentheses = .false.
            end if
        end block
        
    end function test_string_concatenation_precedence_with_parentheses
    
end program test_issue_214_string_concatenation_precedence