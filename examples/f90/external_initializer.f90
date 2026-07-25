program external_initializer
    implicit none

    integer, external :: helper = 1

    print *, helper
end program external_initializer
