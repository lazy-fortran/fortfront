module c_structs
    use iso_c_binding
    implicit none

    type, bind(c) :: particle_t
        integer(c_int) :: id
        real(c_double) :: mass
        real(c_double) :: position(3)
        type(c_ptr) :: payload
    end type particle_t

    type, bind(c) :: pair_t
        type(particle_t) :: a
        type(particle_t) :: b
    end type pair_t

contains

    subroutine scale_mass(p, factor) bind(c)
        type(particle_t), intent(inout) :: p
        real(c_double), intent(in) :: factor
        p%mass = p%mass*factor
    end subroutine scale_mass

end module c_structs
