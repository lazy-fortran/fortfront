program issue_2559_select_type_case_variations
    implicit none

    class(*), allocatable :: x
    integer :: i

    allocate (x, source=1)

    select type (x)
        type Is (integer)
        i = x
    class DeFaUlT
        i = 0
    end select

    print *, i
end program issue_2559_select_type_case_variations
