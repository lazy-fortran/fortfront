! Corrected neighbour of iso_fortran_env_4.f90 (issue #2887).
! Two USE statements for the same module with the SAME stated nature are
! legal, and one scoping unit may state different natures for different
! modules. Only a disagreement about a single module inside a single scoping
! unit is an error.
module use_nature_probe
    implicit none
    integer, parameter :: local_marker = 1
end module use_nature_probe

program foo
    use, non_intrinsic :: use_nature_probe
    use, non_intrinsic :: use_nature_probe, only: local_marker
    implicit none
    print *, local_marker
end program foo

subroutine truc
    use, intrinsic :: iso_fortran_env, only: output_unit
    use, non_intrinsic :: use_nature_probe, only: local_marker
    implicit none
    write (output_unit, '(I0)') local_marker
end subroutine truc
