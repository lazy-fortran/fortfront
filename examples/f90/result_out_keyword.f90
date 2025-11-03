program p
contains
    function test_func(arr) result(out)
        real, dimension(3), intent(in) :: arr
        real, dimension(3) :: out
        out = arr * 2.0
    end function
end program p
