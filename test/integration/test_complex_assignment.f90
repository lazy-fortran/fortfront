program test_complex_assignment
    use fortfront
    implicit none

    character(len=:), allocatable :: output, error_msg
    logical :: success

    ! Test: Complex literal assignment (type should be inferred as complex)
    call transform_lazy_fortran_string("z = (3.0, 4.0)" // new_line('A') // &
        "print *, 'Complex:', z", output, error_msg)
    success = len_trim(error_msg) == 0
    print *, "Test - Complex assignment inference: ", success
    if (.not. success) print *, "Error: ", error_msg
    if (success) then
        print *, "Output: ", trim(output)
        ! Check that the declaration is complex, not real
        if (index(output, "complex :: z") > 0) then
            print *, "SUCCESS: Variable z correctly inferred as complex"
        else if (index(output, "real :: z") > 0) then
            print *, "FAILURE: Variable z incorrectly inferred as real"
            stop 1
        else
            print *, "FAILURE: No declaration for z found"
            stop 1
        end if
    end if

end program test_complex_assignment
