! Corrected neighbor of pr91715.f90: the CHARACTER type-spec is closed.
program pr91715_ok
    print *, f()
contains
    character(1) function f()
        f = 'a'
    end function f
end program pr91715_ok
