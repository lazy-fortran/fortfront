! BLOCK construct scoping (F2018 11.1.4): entities declared in a BLOCK
! specification part are local to the construct and are not visible after
! END BLOCK. A BLOCK-local name may shadow an outer name of a different type
! without disturbing it.
program block_local_scope
    implicit none
    integer :: total
    integer :: shadowed

    total = 0
    shadowed = 7

    block
        integer :: k
        k = 5
        total = total + k
    end block

    block
        real :: shadowed
        shadowed = 2.5
        total = total + int(shadowed)
    end block

    outer_block: block
        character(len=3) :: tag
        tag = 'abc'
        total = total + len(tag)
    end block outer_block

    print *, total, shadowed
end program block_local_scope
