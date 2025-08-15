program simple_debug
    use lexer_core, only: token_t, tokenize_core, TK_EOF, TK_KEYWORD
    implicit none
    
    character(len=*), parameter :: source = &
        "module test_mod" // new_line('a') // &
        "    contains" // new_line('a') // &
        "    function test_func(x)" // new_line('a') // &
        "        real :: test_func, x" // new_line('a') // &
        "        test_func = x + 1.0" // new_line('a') // &
        "    end function" // new_line('a') // &
        "end module test_mod" // new_line('a') // &
        "" // new_line('a') // &
        "use test_mod" // new_line('a') // &
        "print *, test_func(5.0)"
    
    type(token_t), allocatable :: tokens(:)
    integer :: i
    
    write(*,*) "Source:"
    write(*,*) trim(source)
    write(*,*) ""
    
    call tokenize_core(source, tokens)
    
    write(*,*) "Tokens:"
    do i = 1, size(tokens)
        if (tokens(i)%kind == TK_EOF) then
            write(*,'(A,I0,A)') "Token ", i, ": EOF"
        else
            write(*,'(A,I0,A,A,A,I0,A,I0,A)') "Token ", i, ": '", trim(tokens(i)%text), &
                "' (line=", tokens(i)%line, ", col=", tokens(i)%column, ")"
        end if
    end do
end program simple_debug