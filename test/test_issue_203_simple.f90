program test_issue_203_simple
    use frontend, only: compile_source, compilation_options_t
    implicit none
    
    logical :: all_passed
    
    all_passed = .true.
    
    print *, '=== Issue #203: Redundant dimension in allocatable declaration ==='
    
    if (.not. test_exact_issue_203_example()) all_passed = .false.
    
    print *
    if (all_passed) then
        print *, 'Issue #203 fixed!'
    else
        print *, 'Issue #203 test failed!'
        error stop 1
    end if
    
contains
    
    logical function test_exact_issue_203_example()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg, line
        type(compilation_options_t) :: options
        integer :: unit, iostat
        logical :: found_no_redundant_dimension
        
        test_exact_issue_203_example = .true.
        print *, 'Testing exact example from issue #203...'
        
        ! Create test input - exact code from issue
        input_file = 'test_issue_203.lf'
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') 'v = [10]'
        write(unit, '(a)') 'v = [v, v**2]'
        write(unit, '(a)') 'print*,v'
        close(unit)
        
        ! Compile
        output_file = 'test_issue_203_out.f90'
        options%output_file = output_file
        
        call compile_source(input_file, options, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_exact_issue_203_example = .false.
            return
        end if
        
        ! Check generated code - the key issue is NO "dimension(1)" or "dimension(2)"
        found_no_redundant_dimension = .true.
        
        open(newunit=unit, file=output_file, status='old')
        do
            read(unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            ! Check for the specific issue: redundant dimension attribute
            if (index(line, 'dimension(1)') > 0 .or. &
                index(line, 'dimension(2)') > 0) then
                print *, '  FAIL: Found redundant dimension attribute:', trim(line)
                found_no_redundant_dimension = .false.
            end if
            ! Also check that allocatable is present
            if (index(line, 'allocatable') > 0 .and. index(line, 'v') > 0) then
                print *, '  Generated declaration:', trim(line)
                ! Verify it has deferred shape
                if (index(line, 'v(:)') == 0) then
                    print *, '  WARNING: Missing deferred shape (:) on allocatable'
                end if
            end if
        end do
        close(unit)
        
        if (found_no_redundant_dimension) then
            print *, '  PASS: No redundant dimension attribute found'
        else
            print *, '  FAIL: Redundant dimension attribute still present'
            test_exact_issue_203_example = .false.
        end if
        
        ! Clean up test files
        call execute_command_line('rm -f ' // input_file // ' ' // output_file, &
                                  wait=.true.)
    end function test_exact_issue_203_example
    
    
end program test_issue_203_simple