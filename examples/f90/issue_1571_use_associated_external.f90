module math_funcs
contains
    elemental function add_one(x) result(y)
        integer, intent(in) :: x
        integer :: y
        y = x + 1
    end function add_one
end module math_funcs

program test_pure
    use math_funcs
    implicit none
    integer :: arr(3)
    arr = [1, 2, 3]
    arr = add_one(arr)
    print *, arr
end program test_pure
