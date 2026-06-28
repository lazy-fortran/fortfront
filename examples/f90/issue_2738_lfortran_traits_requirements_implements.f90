! Issue 2738: Parse LFortran traits/requirements/implements

trait IComparable
end trait IComparable

requirement Ordered(T)
end requirement Ordered

implements IComparable(T)
end implements IComparable

type, implements(IComparable) :: int_wrapper
    integer :: value
end type int_wrapper

function min_value{IComparable :: T} (a, b) result(res)
    type(T), intent(in) :: a
    type(T), intent(in) :: b
    type(T) :: res

    res = a
end function min_value
