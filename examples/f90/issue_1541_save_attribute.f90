! Minimal reproducer: SAVE attribute mishandled
subroutine counter()
    integer, save :: count = 0
    count = count + 1
    print *, count
end subroutine counter

program main
    call counter()
    call counter()
end program main
