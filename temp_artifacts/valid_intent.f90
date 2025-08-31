subroutine process(input, output)
    real, intent(in) :: input
    real, intent(out) :: output
    
    output = input * 2.0  ! OK: reading IN, writing OUT
end subroutine process
