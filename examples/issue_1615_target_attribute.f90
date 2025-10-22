module target_attribute_module
    implicit none
    private

    public :: Array, array_init, test_target_attribute

    type :: Array
        real, allocatable :: values(:)
    end type Array

contains

    subroutine array_init(this, n)
        type(Array), target, intent(inout) :: this
        integer, intent(in) :: n
        allocate (this%values(n))
    end subroutine array_init

    subroutine pointer_association_test(arr_target, arr_ptr)
        type(Array), target, intent(inout) :: arr_target
        type(Array), pointer, intent(out) :: arr_ptr
        arr_ptr => arr_target
    end subroutine pointer_association_test

    subroutine test_target_attribute()
        type(Array) :: my_array
        type(Array), pointer :: array_ptr

        call array_init(my_array, 5)
        my_array%values = [1.0, 2.0, 3.0, 4.0, 5.0]

        call pointer_association_test(my_array, array_ptr)

        if (associated(array_ptr)) then
            print *, "Pointer association successful"
            print *, "First value:", array_ptr%values(1)
        end if
    end subroutine test_target_attribute

end module target_attribute_module

program test_target
    use target_attribute_module
    implicit none

    call test_target_attribute()

end program test_target
