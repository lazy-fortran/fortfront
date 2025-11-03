! Intrinsic USE ONLY clause should be preserved
program use_intrinsic_only
    use, intrinsic :: iso_fortran_env, only: int32
    implicit none
    integer(int32) :: value
    value = 123_int32
    print *, value
end program use_intrinsic_only
