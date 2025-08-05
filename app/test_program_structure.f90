program test_program_structure
    use fortfront, only: transform_lazy_fortran_string_with_format, format_options_t
    implicit none
    
    character(len=:), allocatable :: input_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    type(format_options_t) :: options
    
    ! Simple program test from issue #98
    input_code = 'program test' // new_line('A') // &
                 '    implicit none' // new_line('A') // &
                 '    integer :: x' // new_line('A') // &
                 '    x = 42' // new_line('A') // &
                 'end program test'
    
    print *, "Input code:"
    print *, input_code
    print *, ""
    
    ! Transform with default format options
    call transform_lazy_fortran_string_with_format(input_code, output_code, error_msg, options)
    
    if (len_trim(error_msg) > 0) then
        print *, "ERROR during transformation:", error_msg
        return
    end if
    
    print *, "Output code:"
    print *, output_code
    print *, ""
    
    ! Check if program structure is preserved
    if (index(output_code, "program test") == 0) then
        print *, "ERROR: 'program test' declaration missing from output!"
    else
        print *, "OK: program declaration found"
    end if
    
    if (index(output_code, "end program test") == 0) then
        print *, "ERROR: 'end program test' missing from output!"
    else
        print *, "OK: end program found"
    end if
    
    if (index(output_code, "implicit none") == 0) then
        print *, "ERROR: 'implicit none' missing from output!"
    else
        print *, "OK: implicit none found"
    end if
    
end program test_program_structure