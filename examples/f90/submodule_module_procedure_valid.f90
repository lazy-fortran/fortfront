! Corrected neighbour of pr93423.f90: the separate module procedure repeats
! only the name, never the dummy argument list.
module submodule_mp_mod
    implicit none
    interface
        module subroutine bp(s)
            integer, intent(inout) :: s
        end subroutine bp
    end interface
end module submodule_mp_mod

submodule (submodule_mp_mod) submodule_mp_sub
contains
    module procedure bp
        s = s + 1
    end procedure bp
end submodule submodule_mp_sub

program submodule_module_procedure_valid
    use submodule_mp_mod, only: bp
    implicit none
    integer :: value
    value = 41
    call bp(value)
    print *, value
end program submodule_module_procedure_valid
