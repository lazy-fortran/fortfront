module generic_resolution_query_example
    implicit none

    interface scale
        module procedure scale_int, scale_real
    end interface scale

contains

    subroutine scale_int(value)
        integer, intent(inout) :: value

        value = 2 * value
    end subroutine scale_int

    subroutine scale_real(value)
        real(8), intent(inout) :: value

        value = 3.0d0 * value
    end subroutine scale_real

end module generic_resolution_query_example

program generic_resolution_query_driver
    use generic_resolution_query_example, only: scale
    implicit none
    integer :: integer_value
    real(8) :: real_value

    integer_value = 2
    real_value = 2.0d0
    call scale(integer_value)
    call scale(real_value)
    print *, integer_value, real_value
end program generic_resolution_query_driver
