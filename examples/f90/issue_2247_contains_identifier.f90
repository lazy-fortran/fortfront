program issue_2247_contains_identifier
    implicit none
    real :: contains(2)

contains = 2.0
    contains(2) = contains(1) + 3.0
    contains(int(contains(1))) = contains(2) - 1.0

    print *, contains(1), contains(2)
end program issue_2247_contains_identifier
