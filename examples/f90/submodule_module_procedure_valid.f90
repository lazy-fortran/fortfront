! Corrected neighbour of pr93423.f90: the separate module procedure repeats
! only the name, never the dummy argument list.
module submodule_mp_mod
    implicit none
    interface
        module function bp(s) result(res)
            integer, intent(in) :: s
            integer :: res
        end function bp
    end interface
end module submodule_mp_mod

submodule (submodule_mp_mod) submodule_mp_sub
contains
    module procedure bp
        res = s + 1
    end procedure bp
end submodule submodule_mp_sub

program submodule_module_procedure_valid
    use submodule_mp_mod, only: bp
    implicit none
    print *, bp(41)
end program submodule_module_procedure_valid
