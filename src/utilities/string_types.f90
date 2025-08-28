module string_types
    ! Simple string type to replace fpm_string dependency
    implicit none
    private

    public :: string_t

    type :: string_t
        character(len=:), allocatable :: s
    contains
        procedure :: assign_string
        generic :: assignment(=) => assign_string
    end type string_t

contains

    subroutine assign_string(this, str)
        class(string_t), intent(out) :: this
        character(len=*), intent(in) :: str
        this%s = str
    end subroutine assign_string

end module string_types