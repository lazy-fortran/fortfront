program p
contains
    function f(vals) result(in)
        integer, intent(in) :: vals
        integer :: in
        in = vals
    end function f
end program p
