program test_function_returns_array
    implicit none
    integer :: i
    real, dimension(5) :: result

    result = generate_sequence(5, 2.0)
    print *, 'Sequence:', result

contains

    function generate_sequence(n, step) result(seq)
        integer, intent(in) :: n
        real, intent(in) :: step
        real, dimension(n) :: seq
        integer :: i

        do i = 1, n
            seq(i) = real(i) * step
        end do
    end function generate_sequence

end program test_function_returns_array
