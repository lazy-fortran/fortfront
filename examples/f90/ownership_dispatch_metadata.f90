module ownership_dispatch_metadata
    implicit none

    type, abstract :: base_t
        integer, allocatable :: owned(:)
        integer, pointer :: link(:)
    contains
        procedure(work_interface), deferred, pass(self) :: work
        generic :: run => work
    end type base_t

    type, extends(base_t) :: child_t
        integer :: value
    contains
        procedure :: work => child_work
    end type child_t

    integer, save :: global_counter
contains

    subroutine child_work(self)
        class(child_t), intent(inout) :: self
        integer, allocatable :: temporary(:)

        allocate (temporary(2))
        temporary = 1
        call move_alloc(temporary, self%owned)
        nullify (self%link)
        global_counter = global_counter + self%owned(1)
    end subroutine child_work

    subroutine use_base(self)
        class(base_t), intent(inout) :: self
        integer :: common_value
        common /state/ common_value

        call self%work()
        common_value = common_value + 1
    end subroutine use_base

end module ownership_dispatch_metadata
