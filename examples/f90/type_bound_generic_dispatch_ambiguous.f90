module type_bound_generic_dispatch_ambiguous
    implicit none

    type :: ambiguous_t
    contains
        procedure, nopass :: choose_left
        procedure, nopass :: choose_right
        generic :: choose => choose_left, choose_right
    end type ambiguous_t

contains

    subroutine choose_left(value)
        integer, intent(in) :: value
    end subroutine choose_left

    subroutine choose_right(value)
        integer, intent(in) :: value
    end subroutine choose_right

    subroutine ambiguous_call(object, value)
        type(ambiguous_t), intent(inout) :: object
        integer, intent(in) :: value

        call object%choose(value)
    end subroutine ambiguous_call

end module type_bound_generic_dispatch_ambiguous
