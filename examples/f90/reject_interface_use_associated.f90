module provider
interface
subroutine foo(a)
real :: a(:)
end subroutine foo
end interface
end module provider
module caller
use provider
contains
subroutine invoke(a)
real :: a(:)
call foo(a)
end subroutine invoke
end module caller
subroutine foo(a)
real :: a(:)
end subroutine foo
