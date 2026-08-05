module m
type test_type
integer :: id = 1
end type test_type
contains
real function fun1(t,x)
real, intent(in) :: x
type(test_type) :: t
fun1 = cos(x)
end function fun1
end module m
program p
use m
implicit none
call test(fun1)
contains
subroutine test(proc)
interface
real function proc(t,x)
import :: test_type
real, intent(in) :: x
class(test_type) :: t
end function proc
end interface
end subroutine test
end program p
