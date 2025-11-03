program p
contains
    function f(vals) result(out)
        integer, intent(in) :: vals
        integer :: out
        out = vals
    end function f
end program p
