module my_module
    implicit none
    interface my_func
        module procedure func_impl
    end interface my_func
contains
    function func_impl(x) result(y)
        integer, intent(in) :: x
        integer :: y
        y = x * 2
    end function func_impl
end module my_module

program main
    use my_module
    implicit none
    integer :: result
    result = my_func(5)
    print *, result
end program main
