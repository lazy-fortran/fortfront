program test_implied_do_standardization
    use frontend, only: compile_source, compilation_options_t
    implicit none

    logical :: all_passed

    print *, '=== Testing Implied Do Loop Standardization ==='
    
    all_passed = .true.
    
    if (.not. test_simple_implied_do()) all_passed = .false.
    if (.not. test_implied_do_with_step()) all_passed = .false.
    if (.not. test_negative_range_implied_do()) all_passed = .false.
    if (.not. test_non_literal_bounds()) all_passed = .false.
    
    ! Cleanup temporary files
    call cleanup_temp_files()
    
    if (all_passed) then
        print *, 'PASS: All implied do loop standardization tests passed'
        stop 0
    else
        print *, 'FAIL: Some implied do loop standardization tests failed'
        stop 1
    end if

contains

    ! Helper to run a test case
    logical function run_implied_do_test(input_code, expected_var, &
                                         expected_size_str, test_name)
        character(len=*), intent(in) :: input_code(:)
        character(len=*), intent(in) :: expected_var, expected_size_str
        character(len=*), intent(in) :: test_name
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg, line
        type(compilation_options_t) :: options
        integer :: unit, iostat, i
        
        run_implied_do_test = .false.
        
        ! Create test input
        input_file = 'test_' // test_name // '.lf'
        open(newunit=unit, file=input_file, status='replace')
        do i = 1, size(input_code)
            write(unit, '(a)') input_code(i)
        end do
        close(unit)
        
        ! Compile
        output_file = 'test_' // test_name // '_out.f90'
        options%output_file = output_file
        call compile_source(input_file, options, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, 'Compilation error:', trim(error_msg)
            return
        end if
        
        ! Check output
        open(newunit=unit, file=output_file, status='old')
        do
            read(unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            
            if (index(line, expected_var) > 0 .and. index(line, '::') > 0) then
                print *, '  Found:', trim(line)
                if (index(line, expected_size_str) > 0) then
                    run_implied_do_test = .true.
                else
                    print *, '  ERROR: Expected', expected_size_str
                end if
            end if
        end do
        close(unit)
    end function run_implied_do_test

    logical function test_simple_implied_do()
        character(len=50) :: code(2)
        print *, 'Testing simple implied do loop...'
        code = ['squares = [(i**2, i=1,10)]                      ', &
                'print*, squares                                 ']
        test_simple_implied_do = run_implied_do_test(code, 'squares', &
                                                     'squares(10)', 'simple')
    end function test_simple_implied_do
    
    logical function test_implied_do_with_step()
        character(len=50) :: code(2)
        print *, 'Testing implied do with step...'
        code = ['evens = [(i, i=2,20,2)]                         ', &
                'print*, evens                                   ']
        test_implied_do_with_step = run_implied_do_test(code, 'evens', &
                                                        'evens(10)', 'step')
    end function test_implied_do_with_step
    
    logical function test_negative_range_implied_do()
        character(len=50) :: code(2)
        print *, 'Testing implied do with negative step...'
        code = ['countdown = [(i, i=10,1,-1)]                    ', &
                'print*, countdown                               ']
        test_negative_range_implied_do = run_implied_do_test(code, &
                                            'countdown', 'countdown(10)', 'neg')
    end function test_negative_range_implied_do
    
    logical function test_non_literal_bounds()
        character(len=50) :: code(3)
        print *, 'Testing implied do with non-literal bounds...'
        code = ['n = 10                                          ', &
                'arr = [(i, i=1,n)]                              ', &
                'print*, arr                                     ']
        test_non_literal_bounds = run_implied_do_test(code, 'arr', &
                                                      'allocatable', 'var')
    end function test_non_literal_bounds
    
    subroutine cleanup_temp_files()
        logical :: file_exists
        character(len=30) :: filename
        integer :: i, unit
        character(len=10), dimension(4) :: test_names = &
            ['simple    ', 'step      ', 'neg       ', 'var       ']
        
        do i = 1, 4
            ! Delete .lf file
            filename = 'test_' // trim(test_names(i)) // '.lf'
            inquire(file=filename, exist=file_exists)
            if (file_exists) then
                open(newunit=unit, file=filename, status='old')
                close(unit, status='delete')
            end if
            
            ! Delete _out.f90 file
            filename = 'test_' // trim(test_names(i)) // '_out.f90'
            inquire(file=filename, exist=file_exists)
            if (file_exists) then
                open(newunit=unit, file=filename, status='old')
                close(unit, status='delete')
            end if
        end do
    end subroutine cleanup_temp_files

end program test_implied_do_standardization