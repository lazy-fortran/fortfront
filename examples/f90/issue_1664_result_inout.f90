program p
contains
    function f(vals) result(inout)
        integer, intent(in) :: vals
        integer :: inout
        inout = vals
    end function f
end program p
