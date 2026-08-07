program missing_continuation
    implicit none
    integer :: value

    value = 2
    select case (value)
    case (1,
        ! The comma is not continued: this source must be rejected.
        2)
        print *, 'FAIL'
    end select
end program missing_continuation
