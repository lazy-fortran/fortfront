! Reproducer for issue #1353: class pointer declaration preserved
program class_decl
    implicit none

    type :: base_t
        real :: x
    end type base_t

    type(base_t) :: storage
    class(base_t), pointer :: p

    storage%x = 1.0
    p => storage
    print *, p%x
end program class_decl
