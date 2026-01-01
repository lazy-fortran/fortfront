! Regression reproducer for issue 2236: optional assumed-rank argument handling
program optional_rank_bridge
    implicit none

    integer :: payload(1)

    payload = 5

    if (echo() /= 1) stop 1
    if (echo(payload) /= 2) stop 2
contains

    integer function echo(sample)
        type(*), optional, dimension(..) :: sample

        if (present(sample)) then
            echo = 2
        else
            echo = 1
        end if
    end function echo
end program optional_rank_bridge
