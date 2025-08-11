program test_implied_do_standardization
    use frontend, only: compile_source, compilation_options_t
    implicit none

    logical :: all_passed

    print *, '=== Testing Implied Do Loop Standardization ==='
    
    all_passed = .true.
    
    if (.not. test_simple_implied_do()) all_passed = .false.
    if (.not. test_implied_do_with_step()) all_passed = .false.
    if (.not. test_negative_range_implied_do()) all_passed = .false.
    
    if (all_passed) then
        print *, 'PASS: All implied do loop standardization tests passed'
        stop 0
    else
        print *, 'FAIL: Some implied do loop standardization tests failed'
        stop 1
    end if

contains

    logical function test_simple_implied_do()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg, line
        type(compilation_options_t) :: options
        integer :: unit, iostat
        logical :: found_correct_decl
        
        test_simple_implied_do = .false.
        
        ! Create test input with implied do loop
        input_file = 'test_implied_do.lf'
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') 'squares = [(i**2, i=1,10)]'
        write(unit, '(a)') 'print*, squares'
        close(unit)
        
        ! Compile with standardization
        output_file = 'test_implied_do_out.f90'
        options%output_file = output_file
        
        call compile_source(input_file, options, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, 'Compilation error:', trim(error_msg)
            return
        end if
        
        ! Check if squares got a declaration with correct size
        found_correct_decl = .false.
        open(newunit=unit, file=output_file, status='old')
        do
            read(unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            
            ! Look for squares declaration with size 10
            if (index(line, 'squares') > 0 .and. index(line, '::') > 0) then
                print *, 'Found declaration:', trim(line)
                if (index(line, 'squares(10)') > 0 .or. &
                    index(line, 'squares(:)') > 0) then
                    found_correct_decl = .true.
                else
                    print *, 'ERROR: Wrong array size in declaration'
                end if
            end if
        end do
        close(unit)
        
        if (.not. found_correct_decl) then
            print *, 'FAIL: Declaration has wrong size for implied do array'
            return
        end if
        
        test_simple_implied_do = .true.
        
    end function test_simple_implied_do
    
    logical function test_implied_do_with_step()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg, line
        type(compilation_options_t) :: options
        integer :: unit, iostat
        logical :: found_correct_decl
        
        test_implied_do_with_step = .false.
        print *, 'Testing implied do with step...'
        
        ! Create test input with step value
        input_file = 'test_implied_step.lf'
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') 'evens = [(i, i=2,20,2)]'
        write(unit, '(a)') 'print*, evens'
        close(unit)
        
        ! Compile with standardization
        output_file = 'test_implied_step_out.f90'
        options%output_file = output_file
        
        call compile_source(input_file, options, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, 'Compilation error:', trim(error_msg)
            return
        end if
        
        ! Check for correct declaration (should be size 10: 2,4,6,...,20)
        found_correct_decl = .false.
        open(newunit=unit, file=output_file, status='old')
        do
            read(unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            
            if (index(line, 'evens') > 0 .and. index(line, '::') > 0) then
                print *, '  Found declaration:', trim(line)
                if (index(line, 'evens(10)') > 0) then
                    found_correct_decl = .true.
                else
                    print *, '  ERROR: Wrong array size (expected 10)'
                end if
            end if
        end do
        close(unit)
        
        test_implied_do_with_step = found_correct_decl
        
    end function test_implied_do_with_step
    
    logical function test_negative_range_implied_do()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg, line
        type(compilation_options_t) :: options
        integer :: unit, iostat
        logical :: found_correct_decl
        
        test_negative_range_implied_do = .false.
        print *, 'Testing implied do with negative range...'
        
        ! Create test input with negative step
        input_file = 'test_implied_neg.lf'
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') 'countdown = [(i, i=10,1,-1)]'
        write(unit, '(a)') 'print*, countdown'
        close(unit)
        
        ! Compile with standardization
        output_file = 'test_implied_neg_out.f90'
        options%output_file = output_file
        
        call compile_source(input_file, options, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, 'Compilation error:', trim(error_msg)
            return
        end if
        
        ! Check for correct declaration (should be size 10)
        found_correct_decl = .false.
        open(newunit=unit, file=output_file, status='old')
        do
            read(unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            
            if (index(line, 'countdown') > 0 .and. index(line, '::') > 0) then
                print *, '  Found declaration:', trim(line)
                if (index(line, 'countdown(10)') > 0) then
                    found_correct_decl = .true.
                else
                    print *, '  ERROR: Wrong array size (expected 10)'
                end if
            end if
        end do
        close(unit)
        
        test_negative_range_implied_do = found_correct_decl
        
    end function test_negative_range_implied_do

end program test_implied_do_standardization