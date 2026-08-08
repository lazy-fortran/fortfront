module abstract_dispatch_target_query
    implicit none

    type, abstract :: base_t
    contains
        procedure :: work => base_work
    end type base_t

    type, extends(base_t), abstract :: middle_t
    contains
        procedure :: work => middle_work
    end type middle_t

    type, extends(middle_t) :: leaf_t
    end type leaf_t

contains

    subroutine base_work(self)
        class(base_t), intent(inout) :: self
    end subroutine base_work

    subroutine middle_work(self)
        class(middle_t), intent(inout) :: self
    end subroutine middle_work

end module abstract_dispatch_target_query
