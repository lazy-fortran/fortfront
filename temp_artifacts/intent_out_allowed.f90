subroutine set_output(result)
    real, intent(out) :: result
    
    result = 42.0  ! OK: Can modify INTENT(OUT)
end subroutine set_output
