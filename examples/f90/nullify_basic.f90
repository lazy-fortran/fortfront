program test_nullify_basic
    implicit none
    integer, pointer :: ptr1, ptr2

    nullify (ptr1, ptr2)
    print *, 'Done'
end program test_nullify_basic
