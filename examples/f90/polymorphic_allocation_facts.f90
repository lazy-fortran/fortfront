module polymorphic_allocation_facts
    implicit none

    type, abstract :: base_t
        integer :: value
    end type base_t

    type, extends(base_t) :: child_t
        integer :: scale
    end type child_t

    type :: holder_t
        type(child_t) :: concrete
        class(base_t), allocatable :: payload
    end type holder_t

contains

    function make_child() result(value)
        type(child_t) :: value

        value%value = 1
        value%scale = 2
    end function make_child

    subroutine exercise(concrete_child, owner, universal, box, poly_source)
        type(child_t), intent(in) :: concrete_child
        class(base_t), allocatable :: owner
        class(*), allocatable :: universal
        type(holder_t), intent(inout) :: box
        class(base_t), allocatable, intent(in) :: poly_source
        class(base_t), allocatable :: repeated
        class(base_t), allocatable :: factory_owner
        class(base_t), allocatable :: alias_source_owner

        allocate (owner, source=concrete_child)
        allocate (universal, source=concrete_child)
        allocate (box%payload, source=box%concrete)
        allocate (repeated, source=concrete_child)
        allocate (repeated, source=concrete_child)
        allocate (factory_owner, source=make_child())
        allocate (alias_source_owner, source=poly_source)

        associate (alias => concrete_child)
            allocate (alias_source_owner, source=alias)
        end associate
    end subroutine exercise

end module polymorphic_allocation_facts
