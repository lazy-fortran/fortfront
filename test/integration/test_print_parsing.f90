program test_print_parsing
    use iso_fortran_env, only: error_unit
    use frontend_core
    use ast_nodes_core
    use ast_nodes_io
    implicit none
    
    character(len=:), allocatable :: source
    type(compilation_options_t) :: options
    character(len=1024) :: error_msg
    integer :: output_unit, io_status
    character(len=256) :: line
    logical :: found_print
    
    print *, "Testing print statement parsing"
    
    ! Create a temporary source file with print
    open(newunit=output_unit, file='test_parse.f90', status='replace')
    write(output_unit, '(A)') 'program test'
    write(output_unit, '(A)') '    print *, "Hello"'
    write(output_unit, '(A)') 'end program test'
    close(output_unit)
    
    ! Set up minimal options
    options%output_file = 'test_parse_out.f90'
    
    ! Compile
    call compile_source('test_parse.f90', options, error_msg)
    
    if (len_trim(error_msg) > 0) then
        print *, "Compilation error:", trim(error_msg)
        stop 1
    end if
    
    ! Check output
    found_print = .false.
    open(newunit=output_unit, file='test_parse_out.f90', status='old', iostat=io_status)
    if (io_status == 0) then
        do
            read(output_unit, '(A)', iostat=io_status) line
            if (io_status /= 0) exit
            if (index(line, 'print') > 0) then
                found_print = .true.
                print *, "Found:", trim(line)
            end if
        end do
        close(output_unit)
    end if
    
    if (.not. found_print) then
        print *, "FAIL: No print statement in output"
        print *, "Output file contents:"
        call execute_command_line('cat test_parse_out.f90')
        stop 1
    end if
    
    print *, "SUCCESS: Print statement preserved"
    
    ! Cleanup
    call execute_command_line('rm -f test_parse.f90 test_parse_out.f90')
    
end program test_print_parsing