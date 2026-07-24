program p
    implicit none
    integer :: arr(5)
    arr(:3) = arr(:3)
    arr(2:) = arr(2:)
    arr(:)  = arr(:)
    arr(2::2) = arr(2::2)
    arr(::2) = arr(::2)
end program p
