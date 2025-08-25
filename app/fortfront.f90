program fortfront_cli
    use iso_fortran_env, only: input_unit, output_unit, error_unit, iostat_end
    use frontend, only: transform_lazy_fortran_string
    implicit none
    
    character(len=:), allocatable :: input_text, output_text, error_msg
    character(len=4096) :: line
    integer :: iostat
    
    ! Read all input from stdin - robust approach for both pipes and redirection
    allocate(character(len=0) :: input_text)
    
    ! Read data line by line with comprehensive error handling
    do
        read(input_unit, '(A)', iostat=iostat) line
        
        ! Handle EOF - this is the normal termination condition
        if (iostat == iostat_end) exit
        
        ! Handle successful reads
        if (iostat == 0) then
            input_text = input_text // trim(line) // new_line('A')
            cycle
        end if
        
        ! Handle error conditions
        ! Positive iostat values indicate system errors
        ! Negative iostat values (other than iostat_end) indicate format errors
        if (iostat > 0) then
            ! System error - could be transient, but likely serious
            write(error_unit, '(A,I0)') 'System error reading from stdin: ', iostat
            stop 1
        else
            ! Format or other error - treat as fatal for consistency
            write(error_unit, '(A,I0)') 'Format error reading from stdin: ', iostat
            stop 1
        end if
    end do
    
    ! Allow empty input - let the frontend handle it gracefully
    ! Empty input should generate a minimal valid program
    
    ! Transform lazy fortran to standard fortran
    call transform_lazy_fortran_string(input_text, output_text, error_msg)
    
    ! For syntax/parsing errors, the error information is included in output_text
    ! Only stop with error code for system-level failures
    if (error_msg /= "" .and. index(error_msg, "Cannot open") > 0) then
        ! System-level error (file I/O, etc.) - report to stderr and exit with error
        write(error_unit, '(A)') trim(error_msg)
        stop 1
    else if (error_msg /= "") then
        ! Syntax/parsing error - report to stderr but continue with output
        write(error_unit, '(A)') trim(error_msg)
    end if
    
    ! Write output to stdout
    if (allocated(output_text) .and. len(output_text) > 0) then
        write(output_unit, '(A)', advance='no') output_text
    else
        write(error_unit, '(A)') 'No output generated'
        stop 1
    end if
    
end program fortfront_cli