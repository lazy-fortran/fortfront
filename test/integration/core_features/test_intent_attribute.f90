program test_intent_attribute
    implicit none
    real :: input_data(5), output_data(5), work_array(5)
    integer :: i
    
    ! Initialize test data
    input_data = [1.0, 2.0, 3.0, 4.0, 5.0]
    work_array = 0.0
    
    ! Call subroutine with intent parameters
    call process_data(input_data, output_data, work_array)
    
    ! Verify results
    do i = 1, 5
        if (abs(output_data(i) - input_data(i) * 2.0) > 1e-6) then
            print *, "INTENT test failed: output incorrect"
            stop 1
        end if
    end do
    
    ! Verify work array was modified
    if (all(work_array == 0.0)) then
        print *, "INTENT test failed: work array not modified"
        stop 1
    end if
    
    print *, "All INTENT tests passed!"
    
contains
    
    subroutine process_data(input, output, workspace)
        real, intent(in) :: input(:)
        real, intent(out) :: output(:)
        real, intent(inout) :: workspace(:)
        
        ! This should work - reading input
        workspace = input
        
        ! This should work - modifying output
        output = input * 2.0
        
        ! This should work - modifying workspace
        workspace = workspace + 1.0
        
        ! This would be an error if semantic checking was implemented:
        ! input = 0.0  ! Error: can't modify intent(in)
    end subroutine process_data
    
end program test_intent_attribute