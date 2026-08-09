module type_bound_generic_dispatch_ambiguous
    implicit none

    type :: ambiguous_t
    contains
        procedure, nopass :: choose_left
        procedure, nopass :: choose_right
        generic :: choose => choose_left, choose_right
    end type ambiguous_t

contains

    subroutine choose_left()
    end subroutine choose_left

    subroutine choose_right()
    end subroutine choose_right

    subroutine ambiguous_call(object)
        type(ambiguous_t), intent(inout) :: object

        call object%choose()
    end subroutine ambiguous_call

end module type_bound_generic_dispatch_ambiguous
