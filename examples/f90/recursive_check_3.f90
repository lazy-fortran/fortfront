program recursive_check_3
    ! INVALID: F2008 R1229 and C1240. A prefix shall not specify the same
    ! prefix-spec more than once, so a repeated PURE keyword is an error.
    implicit none

contains

    pure pure subroutine a1(b)
        real, intent(in) :: b
    end subroutine a1

end program recursive_check_3
