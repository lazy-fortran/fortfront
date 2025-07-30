program fortfront_cli
    use iso_fortran_env, only: input_unit, output_unit, error_unit, iostat_end
    use frontend, only: transform_lazy_fortran_string
    implicit none
    
    character(len=:), allocatable :: input_text, output_text, error_msg
    character(len=1000) :: line
    integer :: iostat
    
    ! Read all input from stdin
    allocate(character(len=0) :: input_text)
    
    do
        read(input_unit, '(A)', iostat=iostat) line
        if (iostat == iostat_end) exit
        if (iostat /= 0) then
            write(error_unit, '(A)') 'Error reading from stdin'
            stop 1
        end if
        input_text = input_text // trim(line) // new_line('A')
    end do
    
    ! Check if we got input
    if (len(input_text) == 0) then
        write(error_unit, '(A)') 'No input received from stdin'
        stop 1
    end if
    
    ! Transform lazy fortran to standard fortran
    call transform_lazy_fortran_string(input_text, output_text, error_msg)
    
    if (error_msg /= "") then
        write(error_unit, '(A)') trim(error_msg)
        stop 1
    end if
    
    ! Write output to stdout
    if (allocated(output_text) .and. len(output_text) > 0) then
        write(output_unit, '(A)', advance='no') output_text
    else
        write(error_unit, '(A)') 'No output generated'
        stop 1
    end if
    
end program fortfront_cli