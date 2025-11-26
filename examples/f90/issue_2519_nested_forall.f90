! Nested FORALL with body inside inner FORALL
program test_nested_forall
    implicit none
    integer :: a(10, 10)
    integer :: i, j
    a(:, :) = 0
    forall (i = 1:10)
        forall (j = 1:10)
            a(i, j) = 1
        end forall
    end forall
    if (sum(a) /= 100) stop 1
end program test_nested_forall
