! Issue 2817: inline instantiation caret syntax name^(T)(...)
program test_inline_caret
    implicit none

    integer :: a, b, c
    real :: x, y, z

    a = 1
    b = 2
    x = 1.0
    y = 2.0

    ! Inline instantiation with caret - subroutine call
    call swap^(integer)(a, b)
    call swap^(real)(x, y)

    ! Inline instantiation with caret - function call
    c = mysum^(integer)(a, b)
    z = mysum^(real)(x, y)

end program test_inline_caret
