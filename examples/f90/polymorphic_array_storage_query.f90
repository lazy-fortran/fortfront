module polymorphic_array_storage_query
    implicit none

    type, abstract :: base_t
    contains
        procedure(run_base), deferred :: run
    end type base_t

    type, extends(base_t) :: child_t
        integer :: value
    contains
        procedure :: run => run_child
    end type child_t

    type :: holder_t
        class(base_t), allocatable :: owner
        type(child_t) :: child
    end type holder_t

    abstract interface
        subroutine run_base(self)
            import base_t
            class(base_t), intent(inout) :: self
        end subroutine run_base
    end interface

contains

    subroutine run_child(self)
        class(child_t), intent(inout) :: self
    end subroutine run_child

    subroutine inspect(values, holders, i)
        class(base_t), intent(inout) :: values(:)
        type(child_t), intent(inout) :: children(:)
        type(holder_t), intent(inout) :: holders(:)
        integer, intent(in) :: i

        call values(i)%run()
        call children(i)%run()
        holders(i)%child%value = i
        allocate (holders(i)%owner)
    end subroutine inspect

end module polymorphic_array_storage_query
