program test_elemental_function
    implicit none
    integer, dimension(5) :: arr
    integer :: i

    arr = [(i, i = 1, 5)]
    print *, 'Input:', arr
    print *, 'Doubled:', double(arr)

contains

    elemental integer function double(x)
        integer, intent(in) :: x
        double = x * 2
    end function double

end program test_elemental_function
