! Type preservation round-trip test
program roundtrip_type_preservation
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    implicit none
    integer :: int_default
    integer(kind=int32) :: int32_val
    integer(kind=int64) :: int64_val
    real :: real_default
    real(kind=real64) :: real64_val
    double precision :: dbl_val
    logical :: log_val
    character(len=10) :: char_val

    int_default = 42
    int32_val = 100
    int64_val = 1000_int64
    real_default = 3.14
    real64_val = 2.718_real64
    dbl_val = 1.414d0
    log_val = .true.
    char_val = 'test'

    print *, int_default, real_default
end program roundtrip_type_preservation
