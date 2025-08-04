program test_memory_corruption_fix
    ! Test for issue #71: Memory corruption in semantic analyzer
    ! This test reproduces the conditions that cause double-free errors
    use fortfront
    implicit none
    
    character(len=*), parameter :: test_code = &
        "program test_memory_corruption" // new_line('a') // &
        "implicit none" // new_line('a') // &
        "real :: a, b, c, matrix(3,3)" // new_line('a') // &
        "integer :: i, j, k, n" // new_line('a') // &
        "n = 3" // new_line('a') // &
        "! Test nested loops with matrix operations (original crash scenario)" // new_line('a') // &
        "do i = 1, n" // new_line('a') // &
        "    do j = 1, n" // new_line('a') // &
        "        matrix(i,j) = real(i) * real(j)" // new_line('a') // &
        "        if (i == j) then" // new_line('a') // &
        "            matrix(i,j) = matrix(i,j) + 1.0" // new_line('a') // &
        "        end if" // new_line('a') // &
        "    end do" // new_line('a') // &
        "end do" // new_line('a') // &
        "! Test complex binary expressions" // new_line('a') // &
        "a = 1.0" // new_line('a') // &
        "b = 2.0" // new_line('a') // &
        "c = a + b * 3.0" // new_line('a') // &
        "k = i + j - 5" // new_line('a') // &
        "c = c + real(k) + matrix(1,1)" // new_line('a') // &
        "end program test_memory_corruption"
    
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