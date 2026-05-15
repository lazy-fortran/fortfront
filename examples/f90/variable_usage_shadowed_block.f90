program variable_usage_shadowed_block
    implicit none
    integer :: value

    value = 1
    block
        integer :: value
        value = 2
        print *, value
    end block
    print *, value
end program variable_usage_shadowed_block
