module call_argument_mapping_example
    implicit none
contains
    subroutine apply(value, scale, limit)
        real(8), intent(inout) :: value
        real(8), intent(in), optional :: scale
        real(8), intent(in), optional :: limit

        if (present(scale)) value = value * scale
        if (present(limit)) value = value + limit
    end subroutine apply

    function evaluate(value, scale, limit) result(output)
        real(8), intent(in) :: value
        real(8), intent(in), optional :: scale
        real(8), intent(in), optional :: limit
        real(8) :: output

        output = value
        if (present(scale)) output = output * scale
        if (present(limit)) output = output + limit
    end function evaluate
end module call_argument_mapping_example

program call_argument_mapping_driver
    use call_argument_mapping_example, only: apply, evaluate
    implicit none
    real(8) :: seed, answer

    seed = 1.0d0
    call apply(value=seed, scale=2.0d0)
    call apply(seed)
    answer = evaluate(scale=3.0d0, value=seed)
    print *, answer
end program call_argument_mapping_driver
