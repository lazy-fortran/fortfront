program test
    type :: recursive_type
        type(recursive_type), pointer :: next
    end type
    type(recursive_type) :: node
end program test
