! Corrected neighbour of pr89943_3.f90 / pr89943_4.f90: the separate module
! subprograms repeat the binding label of their interface bodies.
module submodule_bind_mod
    implicit none
    interface
        module subroutine run_foo(ndim) bind(c, name="runFoo")
            integer, intent(in) :: ndim
        end subroutine run_foo
        module function scale_foo(ndim) bind(c, name="scaleFoo")
            integer, intent(in) :: ndim
            integer :: scale_foo
        end function scale_foo
    end interface
end module submodule_bind_mod

submodule (submodule_bind_mod) submodule_bind_sub
contains
    module subroutine run_foo(ndim) bind(c, name="runFoo")
        integer, intent(in) :: ndim
        print *, 'run_foo', ndim
    end subroutine run_foo

    module function scale_foo(ndim) bind(c, name="scaleFoo")
        integer, intent(in) :: ndim
        integer :: scale_foo
        scale_foo = 2 * ndim
    end function scale_foo
end submodule submodule_bind_sub

program submodule_bind_c_name_valid
    use submodule_bind_mod, only: run_foo, scale_foo
    implicit none
    call run_foo(3)
    print *, scale_foo(21)
end program submodule_bind_c_name_valid
