program rank_pool
    implicit none
    integer, parameter :: n = 60000
    type :: box
        integer :: value
    end type
    type(box), parameter :: preset(*) = [(box(i), i=1,n)]
    class(*), allocatable :: payload(:)
    integer :: i
    do i = 1, 50
        call populate(payload)
        if (.not. allocated(payload)) stop 1
    end do
contains
    subroutine populate(target)
        class(*), allocatable, intent(out) :: target(..)
        select rank(target)
            rank(1)
            target = preset
            rank default
            stop 2
        end select
    end subroutine populate
end program rank_pool
