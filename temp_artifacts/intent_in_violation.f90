subroutine bad_modify(input)
    real, intent(in) :: input
    
    input = 0.0  ! ERROR: Cannot modify INTENT(IN)
end subroutine bad_modify
