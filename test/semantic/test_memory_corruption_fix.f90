program test_memory_corruption_fix
    ! Test for issue #71: Memory corruption in semantic analyzer
    ! This test reproduces the conditions that cause double-free errors
    use fortfront
    implicit none
    
    character(len=*), parameter :: test_code = &
        "program test_binary_ops" // new_line('a') // &
        "implicit none" // new_line('a') // &
        "real :: a, b, c" // new_line('a') // &
        "integer :: i, j, k" // new_line('a') // &
        "a = 1.0" // new_line('a') // &
        "b = 2.0" // new_line('a') // &
        "c = a + b * 3.0" // new_line('a') // &  ! Complex binary expression
        "i = 10" // new_line('a') // &
        "j = 20" // new_line('a') // &
        "k = i + j - 5" // new_line('a') // &     ! Another complex expression  
        "c = c + real(k)" // new_line('a') // &  ! Mixed type operation
        "end program test_binary_ops"
    
    character(len=:), allocatable :: output_code, error_msg
    
    print *, "Testing memory corruption fix..."
    
    ! Test multiple rounds of semantic analysis to trigger potential double-free
    block
        integer :: round
        do round = 1, 5
            print *, "Round ", round
            
            ! Use the high-level API to test the full pipeline
            call transform_lazy_fortran_string(test_code, output_code, error_msg)
            
            ! Check for errors
            if (allocated(error_msg) .and. len(error_msg) > 0) then
                print *, "ERROR in round ", round, ": ", error_msg
                error stop 1
            end if
            
            ! Check basic functionality
            if (len(output_code) == 0) then
                print *, "ERROR: No output generated in round ", round
                error stop 1
            end if
            
            print *, "Round ", round, " completed successfully"
        end do
    end block
    
    print *, "Memory corruption fix test PASSED"
    print *, "All 5 rounds of semantic analysis completed without crashes"
    
end program test_memory_corruption_fix