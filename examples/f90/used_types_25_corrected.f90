! Corrected neighbour of used_types_25.f90 (issue #2888).
! A locally defined type with its own name coexists with the use associated
! one.
module used_types_25_corrected_mod
    implicit none
    type t
        integer :: k
    end type t
end module used_types_25_corrected_mod

program used_types_25_corrected
    use used_types_25_corrected_mod
    implicit none
    type local_t
        integer :: j
    end type local_t
    type(t) :: a
    type(local_t) :: b

    a%k = 1
    b%j = 2
    print *, a%k, b%j
end program used_types_25_corrected
