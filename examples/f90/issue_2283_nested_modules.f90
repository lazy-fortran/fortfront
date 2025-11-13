module issue_2283_outer
    implicit none
    module issue_2283_inner
        implicit none
        integer :: inner_value
    contains
        subroutine set_inner(value)
            integer, intent(in) :: value
            inner_value = value
        end subroutine set_inner
    end module issue_2283_inner
contains
    subroutine touch_inner()
        use issue_2283_inner, only: set_inner
        call set_inner(1)
    end subroutine touch_inner
end module issue_2283_outer
