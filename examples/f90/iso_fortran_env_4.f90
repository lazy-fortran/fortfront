! Negative fixture for issue #2887 (reject-use-01).
! A scoping unit accesses the same module under two different module natures.
! F2023 14.2.2 / C8102: all USE statements in a scoping unit that reference the
! same module shall agree on the module nature. Rejected in both orders.
module iso_fortran_env
end module iso_fortran_env

program foo
    use, intrinsic :: iso_fortran_env
    use, non_intrinsic :: iso_fortran_env
end program foo

subroutine truc
    use, non_intrinsic :: iso_fortran_env
    use, intrinsic :: iso_fortran_env
end subroutine truc
