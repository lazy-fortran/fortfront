! Issue #1607: final procedures must be preserved and not reordered
module final_proc_examples
    implicit none

    type :: myclass_t
        integer :: value
    contains
        final :: myclass_destroy
    end type myclass_t

    type :: resource_t
        real :: x
    contains
        procedure :: init
        final :: cleanup_scalar
        final :: cleanup_array
    end type resource_t
contains
    subroutine myclass_destroy(this)
        type(myclass_t) :: this
        this%value = 0
    end subroutine myclass_destroy

    subroutine init(this, value)
        class(resource_t), intent(inout) :: this
        real, intent(in) :: value
        this%x = value
    end subroutine init

    subroutine cleanup_scalar(this)
        type(resource_t) :: this
        this%x = 0.0
    end subroutine cleanup_scalar

    subroutine cleanup_array(this)
        type(resource_t) :: this(:)
        this%x = 0.0
    end subroutine cleanup_array
end module final_proc_examples
