subroutine select_rank_bounds(values, rank_seen, extent_seen)
    real, intent(inout) :: values(..)
    integer, intent(out) :: rank_seen, extent_seen

    select rank (values)
        rank (1)
        rank_seen = 1
        extent_seen = size(values)
        values = 2.0
        rank (2)
        rank_seen = 2
        extent_seen = size(values, 1) * size(values, 2)
        values = 3.0
        rank default
        rank_seen = -1
        extent_seen = -1
    end select
end subroutine select_rank_bounds

program select_rank_bounds_runtime
    implicit none

    interface
        subroutine select_rank_bounds(values, rank_seen, extent_seen)
            real, intent(inout) :: values(..)
            integer, intent(out) :: rank_seen, extent_seen
        end subroutine select_rank_bounds
    end interface

    real :: vector(3), matrix(2, 2)
    integer :: rank_seen, extent_seen

    vector = 0.0
    call select_rank_bounds(vector, rank_seen, extent_seen)
    if (rank_seen /= 1 .or. extent_seen /= 3 .or. any(vector /= 2.0)) then
        error stop 'rank-one SELECT RANK behavior is wrong'
    end if

    matrix = 0.0
    call select_rank_bounds(matrix, rank_seen, extent_seen)
    if (rank_seen /= 2 .or. extent_seen /= 4 .or. any(matrix /= 3.0)) then
        error stop 'rank-two SELECT RANK behavior is wrong'
    end if

    print *, 'PASS: SELECT RANK bounds runtime behavior'
end program select_rank_bounds_runtime
