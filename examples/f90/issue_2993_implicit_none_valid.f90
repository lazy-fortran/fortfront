module issue_2993_use_mod
    implicit none
    integer :: exported = 7
contains
    subroutine used_sub(value)
        implicit none
        integer, intent(in) :: value
        print *, value
    end subroutine used_sub
end module issue_2993_use_mod

program issue_2993_valid
    use issue_2993_use_mod, only: exported, used_sub
    implicit none
        integer :: total, i
        class(*), allocatable :: boxed
    external external_sub

    total = exported
    call used_sub(total)
    call external_sub(total)
    print *, abs(total)

    do i = 1, total
        total = total + i
    end do

        associate (alias => total)
            total = alias
        end associate

        allocate (boxed, source=total)
        select type (typed => boxed)
        type is (integer)
            total = typed
        class default
            total = 0
        end select

    contains
    subroutine internal_sub(value)
        integer, intent(in) :: value
        print *, value
    end subroutine internal_sub
end program issue_2993_valid

subroutine external_sub(value)
    integer :: value
end subroutine external_sub
