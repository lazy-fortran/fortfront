program issue_1810_parameter_array_allocatable
    implicit none
    integer, parameter :: n = 3
    real, parameter :: values(n) = [1.0, 2.0, 3.0]
    integer, parameter :: matrix(2,2) = reshape([1, 2, 3, 4], [2, 2])
    print *, values
    print *, matrix
end program issue_1810_parameter_array_allocatable
