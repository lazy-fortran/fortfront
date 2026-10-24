module string_types
    ! Simple string type to replace fpm_string dependency
    implicit none
    private

    public :: string_t, append_string

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

    subroutine append_string(list, text)
        ! Grow list by one element holding text. The new component is
        ! assigned directly instead of through a string_t(...) structure
        ! constructor: GNU Fortran 12 builds such a constructor with the
        ! wrong length when its argument is trim(...) or a deferred-length
        ! component reference such as token%text, and the resulting heap
        ! corruption aborted Fo on SUNDIALS bind(c) enums.
        type(string_t), allocatable, intent(inout) :: list(:)
        character(len=*), intent(in) :: text
        type(string_t), allocatable :: grown(:)
        integer :: i, n

        n = 0
        if (allocated(list)) n = size(list)
        allocate (grown(n + 1))
        do i = 1, n
            call move_alloc(list(i)%s, grown(i)%s)
        end do
        grown(n + 1)%s = text
        call move_alloc(grown, list)
    end subroutine append_string

end module string_types
