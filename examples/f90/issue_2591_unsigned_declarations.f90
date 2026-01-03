program issue_2591_unsigned_declarations
    implicit none

    integer, unsigned :: count
    integer(8), unsigned :: big_count
    integer :: i

    count = uint(i)
    i = int(count)
    big_count = uint(i)
end program issue_2591_unsigned_declarations
