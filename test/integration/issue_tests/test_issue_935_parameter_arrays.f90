program test_issue_935
    use frontend, only: compile_source, compilation_options_t
    implicit none
    
    logical :: all_passed
    
    all_passed = .true.
    
    print *, '=== Issue #935: Parameter constants with array dimensions ==='
    
    if (.not. test_parameter_with_dimension()) all_passed = .false.
    if (.not. test_parameter_in_allocate()) all_passed = .false.
    if (.not. test_multidim_array_with_params()) all_passed = .false.
    
    print *
    if (all_passed) then
        print *, 'Issue #935 fixed!'
    else
        print *, 'Issue #935 test failed!'
        stop 1
    end if
    
contains
    
    logical function test_parameter_with_dimension()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg, line
        type(compilation_options_t) :: options
        integer :: unit, iostat
        logical :: found_array_dim
        
        test_parameter_with_dimension = .true.
        print *, 'Testing parameter with dimension attribute...'
        
        ! Create test input
        input_file = 'test_param_dim.f90'
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') 'program test'
        write(unit, '(a)') '    implicit none'
        write(unit, '(a)') '    integer, parameter :: n = 10'
        write(unit, '(a)') '    integer, dimension(n) :: arr'
        write(unit, '(a)') '    arr = 0'
        write(unit, '(a)') 'end program test'
        close(unit)
        
        ! Compile
        output_file = 'test_param_dim_out.f90'
        options%output_file = output_file
        
        call compile_source(input_file, options, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_parameter_with_dimension = .false.
            return
        end if
        
        ! Check output contains array declaration with dimensions
        found_array_dim = .false.
        open(newunit=unit, file=output_file, status='old', iostat=iostat)
        if (iostat == 0) then
            do
                read(unit, '(a)', iostat=iostat) line
                if (iostat /= 0) exit
                ! Check for array declaration with dimensions
                if ((index(line, 'integer :: arr(') > 0 .or. &
                     index(line, 'integer, dimension(') > 0) .and. &
                    index(line, 'arr') > 0) then
                    found_array_dim = .true.
                    exit
                end if
            end do
            close(unit)
        end if
        
        if (found_array_dim) then
            print *, '  PASS: Array declaration with dimensions preserved'
        else
            print *, '  FAIL: Array dimensions lost in output'
            test_parameter_with_dimension = .false.
        end if
        
        ! Clean up
        call execute_command_line('rm -f ' // input_file // ' ' // output_file, &
                                  exitstat=iostat)
    end function test_parameter_with_dimension
    
    logical function test_parameter_in_allocate()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg, line
        type(compilation_options_t) :: options
        integer :: unit, iostat
        logical :: found_allocate_with_param
        
        test_parameter_in_allocate = .true.
        print *, 'Testing parameter in allocate statement...'
        
        ! Create test input
        input_file = 'test_param_alloc.f90'
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') 'program test'
        write(unit, '(a)') '    implicit none'
        write(unit, '(a)') '    integer, parameter :: size = 100'
        write(unit, '(a)') '    integer, allocatable :: dyn_arr(:)'
        write(unit, '(a)') '    allocate(dyn_arr(size))'
        write(unit, '(a)') '    dyn_arr = 1'
        write(unit, '(a)') '    deallocate(dyn_arr)'
        write(unit, '(a)') 'end program test'
        close(unit)
        
        ! Compile
        output_file = 'test_param_alloc_out.f90'
        options%output_file = output_file
        
        call compile_source(input_file, options, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_parameter_in_allocate = .false.
            return
        end if
        
        ! Check output contains allocate with parameter
        found_allocate_with_param = .false.
        open(newunit=unit, file=output_file, status='old', iostat=iostat)
        if (iostat == 0) then
            do
                read(unit, '(a)', iostat=iostat) line
                if (iostat /= 0) exit
                ! Check for allocate statement with size parameter
                if (index(line, 'allocate(dyn_arr(size))') > 0 .or. &
                    index(line, 'allocate(dyn_arr(100))') > 0) then
                    found_allocate_with_param = .true.
                    exit
                end if
            end do
            close(unit)
        end if
        
        if (found_allocate_with_param) then
            print *, '  PASS: Parameter in allocate statement preserved'
        else
            print *, '  FAIL: Parameter usage in allocate lost'
            test_parameter_in_allocate = .false.
        end if
        
        ! Clean up
        call execute_command_line('rm -f ' // input_file // ' ' // output_file, &
                                  exitstat=iostat)
    end function test_parameter_in_allocate
    
    logical function test_multidim_array_with_params()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg, line
        type(compilation_options_t) :: options
        integer :: unit, iostat
        logical :: found_multidim_array
        
        test_multidim_array_with_params = .true.
        print *, 'Testing multi-dimensional arrays with parameter dimensions...'
        
        ! Create test input
        input_file = 'test_multidim.f90'
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') 'program test'
        write(unit, '(a)') '    implicit none'
        write(unit, '(a)') '    integer, parameter :: m = 5, n = 10'
        write(unit, '(a)') '    real, dimension(m, n) :: matrix'
        write(unit, '(a)') '    integer, dimension(m, n, 3) :: tensor'
        write(unit, '(a)') '    matrix = 0.0'
        write(unit, '(a)') '    tensor = 0'
        write(unit, '(a)') 'end program test'
        close(unit)
        
        ! Compile
        output_file = 'test_multidim_out.f90'
        options%output_file = output_file
        
        call compile_source(input_file, options, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_multidim_array_with_params = .false.
            return
        end if
        
        ! Check output contains multi-dimensional array declarations
        found_multidim_array = .false.
        open(newunit=unit, file=output_file, status='old', iostat=iostat)
        if (iostat == 0) then
            do
                read(unit, '(a)', iostat=iostat) line
                if (iostat /= 0) exit
                ! Check for array declarations with multiple dimensions
                if ((index(line, 'matrix(') > 0 .and. index(line, ',') > 0) .or. &
                    (index(line, 'tensor(') > 0 .and. index(line, ',') > 0)) then
                    found_multidim_array = .true.
                    exit
                end if
            end do
            close(unit)
        end if
        
        if (found_multidim_array) then
            print *, '  PASS: Multi-dimensional array declarations preserved'
        else
            print *, '  FAIL: Multi-dimensional array dimensions lost'
            test_multidim_array_with_params = .false.
        end if
        
        ! Clean up
        call execute_command_line('rm -f ' // input_file // ' ' // output_file, &
                                  exitstat=iostat)
    end function test_multidim_array_with_params
    
end program test_issue_935
