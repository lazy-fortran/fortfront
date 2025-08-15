program debug_string_ast
    use frontend
    implicit none
    
    character(len=:), allocatable :: source, result, error_msg
    
    source = 's = "a"' // new_line('a') // &
             'print*,s' // new_line('a') // &
             's = "ab"' // new_line('a') // &
             'print*,s'
    
    call transform_lazy_fortran_string(source, result, error_msg)
    
    print *, "Generated code:"
    print *, result
    
end program debug_string_ast