! Corrected neighbour of submodule_twice.f90 / submodule_unexp.f90: the
! submodule is written at file scope, and "submodule" is still usable as an
! ordinary variable name inside a program body.
module submodule_placement_mod
    implicit none
    interface
        module subroutine report(value)
            integer, intent(in) :: value
        end subroutine report
    end interface
end module submodule_placement_mod

submodule (submodule_placement_mod) submodule_placement_sub
contains
    module subroutine report(value)
        integer, intent(in) :: value
        print *, 'report', value
    end subroutine report
end submodule submodule_placement_sub

program submodule_placement_valid
    use submodule_placement_mod, only: report
    implicit none
    integer :: submodule
    integer :: t(3)
    submodule = 7
    t = 0
    t(2) = submodule
    call report(t(2))
end program submodule_placement_valid
