module issue_2388_uppercase_sections
    implicit none

    type :: worker_t
    CONTAINS
        procedure :: do_work
    end type worker_t

CONTAINS

    subroutine run()
    CONTAINS
        integer function bump(x)
            integer, intent(in) :: x
            bump = x + 1
        end function bump
    end subroutine run

    subroutine do_work(this)
        class(worker_t), intent(inout) :: this
    end subroutine do_work
end module issue_2388_uppercase_sections
