! Invalid: an interface block holds interface bodies, never another INTERFACE
! statement. Reduced from gfortran.dg/unexpected_interface.f90.
module unexpected_interface
    implicit none
    interface
        interface pseudo_scalar
        end interface pseudo_scalar
    end interface
end module unexpected_interface
