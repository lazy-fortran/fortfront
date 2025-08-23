program external_tool_example
    ! Example of an external Fortran tool using libfortfront.a
    ! This demonstrates pure Fortran integration without C API
    use fortfront_external_interface
    use error_handling
    implicit none
    
    type(fortfront_result_t) :: result
    character(len=1024) :: test_source
    integer :: i
    
    print *, "=== External Tool Example Using libfortfront.a ==="
    print *, ""
    
    ! Simple Fortran source to process
    test_source = &
        "program hello" // new_line('a') // &
        "    print *, 'Hello, World!'" // new_line('a') // &
        "end program hello"
    
    print *, "Input source:"
    print *, trim(test_source)
    print *, ""
    
    ! Transform the source using fortfront
    result = fortfront_transform_source(test_source)
    
    if (result%success) then
        print *, "Transformation successful!"
        print *, "Output:"
        print *, trim(result%output)
    else
        print *, "Transformation failed!"
        if (allocated(result%error_message)) then
            print *, "Error: ", trim(result%error_message)
        end if
    end if
    
    print *, ""
    print *, "=== Example Complete ==="
    
end program external_tool_example