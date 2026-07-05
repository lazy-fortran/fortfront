function compute(val)
    integer, intent(in) :: val
    integer :: compute
    interface
        function helper(a) result(r)
            integer, intent(in) :: a
            integer :: r
        end function
    end interface
    y = 2
    compute = helper(val) + y
end function
