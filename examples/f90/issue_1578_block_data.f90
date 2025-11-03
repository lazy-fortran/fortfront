program test_blockdata
    implicit none
    integer :: global_val
    common /shared/ global_val

    print *, global_val
end program test_blockdata

block data init_data
    implicit none
    integer :: global_val
    common /shared/ global_val
    data global_val /999/
end block data init_data
