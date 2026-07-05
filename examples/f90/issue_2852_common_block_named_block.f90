program p
    integer :: x
    common /block/ x
    x = 5
    call q
end program p

subroutine q
    integer :: x
    common /block/ x
    print *, x
end subroutine q
