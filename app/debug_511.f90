program debug_511
    use frontend
    implicit none
    
    character(len=:), allocatable :: input, output, error_msg
    
    input = &
        "type :: a"//new_line('A')// &
        "    integer :: t"//new_line('A')// &
        "end type :: a"//new_line('A')// &
        ""//new_line('A')// &
        "program pro"//new_line('A')// &
        "    type(a) :: testy"//new_line('A')// &
        "    testy%t = 3"//new_line('A')// &
        "end program pro"
    
    print *, "Input:"
    print *, input
    print *, ""
    
    call transform_lazy_fortran_string(input, output, error_msg)
    
    print *, "Output:"
    print *, output
    
end program debug_511