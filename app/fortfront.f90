program fortfront_cli
    use iso_fortran_env, only: input_unit, output_unit, error_unit, iostat_end
    use iso_c_binding, only: c_int
    use frontend, only: transform_lazy_fortran_string
    implicit none
    
    interface
        function getchar() bind(c, name="getchar")
            import :: c_int
            integer(c_int) :: getchar
        end function getchar
    end interface
    
    character(len=:), allocatable :: input_text, output_text, error_msg
    integer(c_int) :: ch
    
    ! GCC 15.x compatibility: Use C interop approach for reliable stdin reading
    ! This works around issues with Fortran I/O redirection in GCC 15.x
    allocate(character(len=0) :: input_text)
    
    ! Use C getchar() approach for maximum GCC 15.x compatibility
    do
        ch = getchar()
        if (ch == -1) exit  ! EOF
        input_text = input_text // achar(ch)
        
        ! Safety limit to prevent infinite reads
        if (len(input_text) > 1000000) then
            write(error_unit, '(A)') 'Input too large (>1MB), stopping'
            stop 1
        end if
    end do
    
    ! GCC 15.2.1 compatibility note: If no input was read, check if this might be
    ! a redirection issue and provide helpful error message
    if (len(input_text) == 0) then
        write(error_unit, '(A)') 'WARNING: No input received.'
        write(error_unit, '(A)') 'If you used file redirection (< file.lf), this may be a GCC 15.2.1 compatibility issue.'
        write(error_unit, '(A)') 'Please use pipes instead: cat file.lf | fortfront'
        ! Continue processing - empty input should still generate a minimal program
    end if
    
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