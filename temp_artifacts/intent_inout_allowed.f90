subroutine modify_value(value)
    real, intent(inout) :: value
    
    value = value + 1.0  ! OK: Can read and modify INTENT(INOUT)
end subroutine modify_value
