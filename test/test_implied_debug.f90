program test_implied_debug
    use frontend
    implicit none
    
    type(compilation_options_t) :: options
    character(len=:), allocatable :: error_msg
    character(len=256) :: input_file
    integer :: unit
    character(len=256) :: line
    integer :: iostat
    
    ! Create test file
    open(newunit=unit, file='test_implied_std.lf', status='replace')
    write(unit, '(a)') 'squares = [(i*i, i=1,5)]'
    write(unit, '(a)') 'print *, squares'
    close(unit)
    
    ! Test implied do
    input_file = 'test_implied_std.lf'
    options%output_file = 'test_implied_debug_out.f90'
    
    call compile_source(input_file, options, error_msg)
    
    if (len_trim(error_msg) > 0) then
        print *, 'ERROR:', trim(error_msg)
    else
        print *, 'SUCCESS: Compilation completed'
        print *, 'Generated code:'
        print *, '---'
        open(newunit=unit, file='test_implied_debug_out.f90', status='old')
        do
            read(unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            print *, trim(line)
        end do
        close(unit)
        print *, '---'
    end if
    
end program test_implied_debug