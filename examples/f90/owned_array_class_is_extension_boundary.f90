module owned_array_extension_boundary
    implicit none
    type, abstract :: base_t
    contains
        procedure(run_interface), deferred :: run
    end type
    type, extends(base_t) :: child_t
    contains
        procedure :: run => child_run
    end type
    type, extends(child_t) :: grandchild_t
    contains
        procedure :: run => grandchild_run
    end type
    abstract interface
        subroutine run_interface(self, value)
            import base_t
            class(base_t), intent(in) :: self
            integer, intent(out) :: value
        end subroutine
    end interface
contains
    subroutine exercise(value)
        integer, intent(out) :: value
        class(base_t), allocatable :: items(:)
        type(grandchild_t), allocatable :: seed(:)
        allocate (seed(1))
        call move_alloc(seed, items)
        select type (items)
        class is (child_t)
            call items(1)%run(value)
        end select
    end subroutine
    subroutine exercise_defined(value)
        integer, intent(out) :: value
        class(base_t), allocatable :: items(:)
        type(child_t), allocatable :: seed(:)
        type :: trigger_t
            integer :: n
        end type
        type(trigger_t) :: trigger
        interface assignment(=)
            procedure assign_trigger
        end interface
        allocate (seed(1))
        call move_alloc(seed, items)
        trigger = 1
        select type (items)
        class is (child_t)
            call items(1)%run(value)
        end select
    contains
        subroutine assign_trigger(dst, src)
            type(trigger_t), intent(out) :: dst
            integer, intent(in) :: src
            dst%n = src
            call replace(items)
        end subroutine
    end subroutine
    subroutine exercise_changed(value)
        integer, intent(out) :: value
        class(base_t), allocatable :: items(:)
        type(child_t), allocatable :: seed(:)
        allocate (seed(1))
        call move_alloc(seed, items)
        call replace(items)
        select type (items)
        class is (child_t)
            call items(1)%run(value)
        end select
    end subroutine
    subroutine replace(items)
        class(base_t), allocatable, intent(out) :: items(:)
        type(grandchild_t), allocatable :: seed(:)
        allocate (seed(1))
        call move_alloc(seed, items)
    end subroutine
    subroutine child_run(self, value)
        class(child_t), intent(in) :: self
        integer, intent(out) :: value
        value = 1
    end subroutine
    subroutine grandchild_run(self, value)
        class(grandchild_t), intent(in) :: self
        integer, intent(out) :: value
        value = 2
    end subroutine
end module
program check_owned_array_extension
  use owned_array_extension_boundary, only: exercise, exercise_changed, exercise_defined
    implicit none
    integer :: value
    value = 0
    call exercise(value)
    if (value /= 2) error stop 'CLASS IS lost extension dispatch'
    call exercise_changed(value)
    if (value /= 2) error stop 'CLASS IS lost changed dynamic type'
    call exercise_defined(value)
    if (value /= 2) error stop 'CLASS IS lost defined assignment effect'
end program
