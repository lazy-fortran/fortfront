program test_block_construct
    implicit none
    integer :: x

    x = 10
    print *, 'Before block:', x

    block
        integer :: x
        x = 20
        print *, 'Inside block:', x
    end block

    print *, 'After block:', x
end program test_block_construct
