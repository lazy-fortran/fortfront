! Statement preservation round-trip test
program roundtrip_statement_preservation
    implicit none
    integer :: i, n

    n = 10

    do i = 1, n
        continue
    end do

    if (n > 5) then
        n = n + 1
    else
        n = n - 1
    end if

end program roundtrip_statement_preservation
