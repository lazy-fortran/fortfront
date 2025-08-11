program test_implied_do_standardization
    use frontend, only: compile_source, compilation_options_t
    implicit none

    logical :: test_passed

    print *, '=== Testing Implied Do Loop Standardization ==='
    
    test_passed = test_simple_implied_do()
    
    if (test_passed) then
        print *, 'PASS: Implied do loop standardization test passed'
        stop 0
    else
        print *, 'FAIL: Implied do loop standardization test failed'
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

end program test_implied_do_standardization