program test_common_block
    implicit none
    integer :: a, b
    common /myblock/ a, b
    a = 10
    b = 20
    print *, 'a =', a, 'b =', b
end program test_common_block
