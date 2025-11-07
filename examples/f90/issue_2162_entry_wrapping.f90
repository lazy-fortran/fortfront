function scale_value(a) result(res)
    integer, intent(in) :: a
    integer :: res

    res = a * 2
    entry triple_value(a) result(res)
    res = a * 3
end function scale_value

