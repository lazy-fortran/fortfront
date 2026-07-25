! Corrected neighbour of submodule_36.f90: the separate module subprogram
! keeps the MODULE prefix, and a submodule-local helper without the prefix
! stays legal because no interface body declares it.
module submodule_prefix_mod
    implicit none
    interface h
        real module function realg2(arg1, arg2)
            real, intent(in) :: arg1, arg2
        end function realg2
    end interface h
end module submodule_prefix_mod

submodule (submodule_prefix_mod) submodule_prefix_sub
contains
    real module function realg2(arg1, arg2)
        real, intent(in) :: arg1, arg2
        realg2 = scale_local(arg1) + arg2
    end function realg2

    real function scale_local(arg1)
        real, intent(in) :: arg1
        scale_local = 2.0 * arg1
    end function scale_local
end submodule submodule_prefix_sub

program submodule_module_prefix_valid
    use submodule_prefix_mod, only: h
    implicit none
    print *, h(1.0, 1.0)
end program submodule_module_prefix_valid
