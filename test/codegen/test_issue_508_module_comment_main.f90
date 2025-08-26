program test_issue_508_module_comment_main
    use frontend, only: compile_source, compilation_options_t
    implicit none
    
    logical :: all_passed
    
    all_passed = .true.
    
    print *, '=== Issue #508: Comment in module causes main program discard ==='
    
    if (.not. test_module_with_comment_preserves_main()) all_passed = .false.
    if (.not. test_module_without_comment_preserves_main()) all_passed = .false.
    if (.not. test_module_with_multiple_comments()) all_passed = .false.
    
    print *
    if (all_passed) then
        print *, 'Issue #508 fixed - all tests passed!'
    else
        print *, 'Issue #508 test failed - main program discarded!'
        stop 1
    end if

contains

    logical function test_module_with_comment_preserves_main()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg, line
        type(compilation_options_t) :: options
        integer :: unit, iostat
        logical :: found_main_program, found_module, found_use_stmt
        
        test_module_with_comment_preserves_main = .true.
        print *, 'Testing module with comment preserves main program...'
        
        ! Test case from Issue #508 - module with comment should preserve main
        input_file = 'test_issue_508_comment.lf'
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') 'module m'
        write(unit, '(a)') 'contains'
        write(unit, '(a)') 'subroutine foo()'
        write(unit, '(a)') 'print*,"foo"'
        write(unit, '(a)') 'end subroutine'
        write(unit, '(a)') ''
        write(unit, '(a)') 'subroutine bar()'
        write(unit, '(a)') 'print*,"bar"'
        write(unit, '(a)') 'end subroutine bar'
        write(unit, '(a)') ''
        write(unit, '(a)') '! abc'  ! The critical comment line
        write(unit, '(a)') 'end module'
        write(unit, '(a)') ''
        write(unit, '(a)') 'use m'
        write(unit, '(a)') 'call foo()'
        write(unit, '(a)') 'call bar()'
        write(unit, '(a)') 'end'
        close(unit)
        
        ! Compile
        output_file = 'test_issue_508_comment_out.f90'
        options%output_file = output_file
        
        call compile_source(input_file, options, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_module_with_comment_preserves_main = .false.
            return
        end if
        
        ! Check generated code
        found_main_program = .false.
        found_module = .false.
        found_use_stmt = .false.
        
        open(newunit=unit, file=output_file, status='old')
        do
            read(unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            
            if (index(line, "program main") > 0) then
                found_main_program = .true.
                print *, '  Found: program main'
            end if
            if (index(line, "module m") > 0) then
                found_module = .true.
                print *, '  Found: module m'
            end if
            if (index(line, "use m") > 0) then
                found_use_stmt = .true.
                print *, '  Found: use m'
            end if
        end do
        close(unit)
        
        if (.not. found_main_program) then
            print *, '  FAIL: Main program not found in generated code!'
            print *, '  This is the exact bug from Issue #508'
            test_module_with_comment_preserves_main = .false.
        else if (.not. found_module) then
            print *, '  FAIL: Module not found in generated code!'
            test_module_with_comment_preserves_main = .false.
        else if (.not. found_use_stmt) then
            print *, '  FAIL: Use statement not found in generated code!'
            test_module_with_comment_preserves_main = .false.
        else
            print *, '  PASS: Module with comment preserves main program'
        end if
    end function

    logical function test_module_without_comment_preserves_main()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg, line
        type(compilation_options_t) :: options
        integer :: unit, iostat
        logical :: found_main_program
        
        test_module_without_comment_preserves_main = .true.
        print *, 'Testing baseline: module without comment...'
        
        ! Same test but without the comment to verify baseline works
        input_file = 'test_issue_508_nocomment.lf'
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') 'module m'
        write(unit, '(a)') 'contains'
        write(unit, '(a)') 'subroutine foo()'
        write(unit, '(a)') 'print*,"foo"'
        write(unit, '(a)') 'end subroutine'
        write(unit, '(a)') ''
        write(unit, '(a)') 'subroutine bar()'
        write(unit, '(a)') 'print*,"bar"'
        write(unit, '(a)') 'end subroutine bar'
        ! No comment line here
        write(unit, '(a)') 'end module'
        write(unit, '(a)') ''
        write(unit, '(a)') 'use m'
        write(unit, '(a)') 'call foo()'
        write(unit, '(a)') 'call bar()'
        write(unit, '(a)') 'end'
        close(unit)
        
        ! Compile
        output_file = 'test_issue_508_nocomment_out.f90'
        options%output_file = output_file
        
        call compile_source(input_file, options, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_module_without_comment_preserves_main = .false.
            return
        end if
        
        ! Check generated code
        found_main_program = .false.
        
        open(newunit=unit, file=output_file, status='old')
        do
            read(unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            
            if (index(line, "program main") > 0) then
                found_main_program = .true.
            end if
        end do
        close(unit)
        
        if (.not. found_main_program) then
            print *, '  FAIL: Main program not found even without comment!'
            test_module_without_comment_preserves_main = .false.
        else
            print *, '  PASS: Module without comment preserves main program'
        end if
    end function

    logical function test_module_with_multiple_comments()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg, line
        type(compilation_options_t) :: options
        integer :: unit, iostat
        logical :: found_main_program, found_module
        
        test_module_with_multiple_comments = .true.
        print *, 'Testing module with multiple comments...'
        
        ! Test with multiple comments in different positions
        input_file = 'test_issue_508_multicomment.lf'
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') '! Top comment'
        write(unit, '(a)') 'module m'
        write(unit, '(a)') '! Module comment'
        write(unit, '(a)') 'contains'
        write(unit, '(a)') '! Before subroutine'
        write(unit, '(a)') 'subroutine foo()'
        write(unit, '(a)') '! Inside subroutine'
        write(unit, '(a)') 'print*,"foo"'
        write(unit, '(a)') 'end subroutine'
        write(unit, '(a)') '! Between subroutines'
        write(unit, '(a)') 'subroutine bar()'
        write(unit, '(a)') 'print*,"bar"'
        write(unit, '(a)') 'end subroutine bar'
        write(unit, '(a)') '! Before end module'
        write(unit, '(a)') 'end module'
        write(unit, '(a)') '! After module'
        write(unit, '(a)') ''
        write(unit, '(a)') '! Before use'
        write(unit, '(a)') 'use m'
        write(unit, '(a)') '! Before calls'
        write(unit, '(a)') 'call foo()'
        write(unit, '(a)') 'call bar()'
        write(unit, '(a)') '! Before end'
        write(unit, '(a)') 'end'
        close(unit)
        
        ! Compile
        output_file = 'test_issue_508_multicomment_out.f90'
        options%output_file = output_file
        
        call compile_source(input_file, options, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_module_with_multiple_comments = .false.
            return
        end if
        
        ! Check generated code
        found_main_program = .false.
        found_module = .false.
        
        open(newunit=unit, file=output_file, status='old')
        do
            read(unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            
            if (index(line, "program main") > 0) then
                found_main_program = .true.
            end if
            if (index(line, "module m") > 0) then
                found_module = .true.
            end if
        end do
        close(unit)
        
        if (.not. found_main_program) then
            print *, '  FAIL: Main program not found with multiple comments!'
            test_module_with_multiple_comments = .false.
        else if (.not. found_module) then
            print *, '  FAIL: Module not found with multiple comments!'
            test_module_with_multiple_comments = .false.
        else
            print *, '  PASS: Module with multiple comments preserves main program'
        end if
    end function

end program test_issue_508_module_comment_main