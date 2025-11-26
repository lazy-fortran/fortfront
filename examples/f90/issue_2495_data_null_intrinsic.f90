program issue_2495_data_null_intrinsic
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
    integer, pointer :: ptr1 => null()
    real(dp), pointer :: ptr2
    data ptr2 /null()/
    if (associated(ptr1)) error stop 'ptr1 should be null'
    if (associated(ptr2)) error stop 'ptr2 should be null'
    print *, 'PASS: DATA with null() works correctly'
end program issue_2495_data_null_intrinsic
