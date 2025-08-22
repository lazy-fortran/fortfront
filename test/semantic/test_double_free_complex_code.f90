program test_double_free_complex_code
    use frontend
    implicit none
    
    character(len=:), allocatable :: test_code
    character(len=:), allocatable :: output_code, error_msg
    logical :: success
    
    print *, "Testing double free issue with complex nested code..."
    
    ! Test the exact code from issue #88
    test_code = "program test" // new_line('a') // &
                "    implicit none" // new_line('a') // &
                "    integer :: i, j, n" // new_line('a') // &
                "    real :: matrix(100, 100)" // new_line('a') // &
                "    n = 100" // new_line('a') // &
                "    do i = 1, n" // new_line('a') // &
                "        do j = 1, n" // new_line('a') // &
                "            matrix(j, i) = real(i * j)" // new_line('a') // &
                "        end do" // new_line('a') // &
                "    end do" // new_line('a') // &
                "    call some_proc(matrix, n)" // new_line('a') // &
                "contains" // new_line('a') // &
                "    subroutine some_proc(mat, size)" // new_line('a') // &
                "        real, intent(in) :: mat(:,:)" // new_line('a') // &
                "        integer, intent(in) :: size" // new_line('a') // &
                "        print *, sum(mat)" // new_line('a') // &
                "    end subroutine" // new_line('a') // &
                "end program test"
    
    call transform_lazy_fortran_string(test_code, output_code, error_msg)
    success = (len(error_msg) == 0)
    
    if (success) then
        print *, "✓ Complex nested code compiled successfully"
        print *, "✓ No double free error occurred"
        print *, "✓ implicit_statement_node handled correctly"
    else
        print *, "ERROR: Compilation failed"
        print *, "Error message:", trim(error_msg)
        stop 1
    end if
    
    print *, "All tests passed!"
    
end program test_double_free_complex_code