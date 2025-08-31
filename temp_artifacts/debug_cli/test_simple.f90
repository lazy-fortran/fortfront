program test_simple_debug
    use frontend, only: transform_lazy_fortran_string
    implicit none
    
    character(len=:), allocatable :: input_text, output_text, error_msg
    integer :: i
    
    ! Simple test input
    input_text = "x = 42"
    
    ! Debug: Print input character by character
    print *, "=== INPUT DEBUG ==="
    print *, "Input length:", len(input_text)
    do i = 1, len(input_text)
        print '(A,I3,A,I4,A)', " Input[", i, "] = ASCII ", iachar(input_text(i:i)), " ('" // input_text(i:i) // "')"
    end do
    print *, "Full input: '", input_text, "'"
    
    ! Call the transformation
    call transform_lazy_fortran_string(input_text, output_text, error_msg)
    
    ! Print results
    print *, "=== OUTPUT DEBUG ==="
    if (error_msg /= "") then
        print *, "Error length:", len(error_msg)
        print *, "Error (first 100 chars):", error_msg(1:min(100, len(error_msg)))
        print *, "Error contains ELF?", index(error_msg, "ELF") > 0
    end if
    
end program test_simple_debug