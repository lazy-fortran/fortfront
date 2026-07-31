! Corrected neighbor of pr91660_1.f90: the type-spec parenthesis is closed.
program pr91660_1_ok
    type t
        integer :: i
    end type t
    type(t) :: x
    x%i = 1
    print *, x%i
end program pr91660_1_ok
