! Issue #1604: type-bound procedure bindings must be preserved
module test_mod_type_bound
    implicit none
    type :: atype
        integer :: value
    contains
        procedure :: get_value
        procedure :: set_value => set_val
    end type atype
contains
    integer function get_value(this)
        class(atype), intent(in) :: this
        get_value = this%value
    end function get_value

    subroutine set_val(this, new_value)
        class(atype), intent(inout) :: this
        integer, intent(in) :: new_value
        this%value = new_value
    end subroutine set_val
end module test_mod_type_bound

module more_type_bound
    implicit none
    type :: mytype
        real :: x
    contains
        procedure :: init
        procedure :: compute => do_compute
        procedure :: cleanup
    end type mytype
contains
    subroutine init(this, value)
        class(mytype), intent(inout) :: this
        real, intent(in) :: value
        this%x = value
    end subroutine init

    real function do_compute(this)
        class(mytype), intent(in) :: this
        do_compute = this%x * 2.0
    end function do_compute

    subroutine cleanup(this)
        class(mytype), intent(inout) :: this
        this%x = 0.0
    end subroutine cleanup
end module more_type_bound
