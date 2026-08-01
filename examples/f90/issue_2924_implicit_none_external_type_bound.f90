module m_tbp_external
    implicit none (type, external)
    type :: box_t
        integer :: v = 1
    contains
        procedure :: show => box_show
    end type box_t
contains
    subroutine box_show(self)
        class(box_t), intent(in) :: self
        print *, self%v
    end subroutine box_show
end module m_tbp_external
program p_tbp_external
    use m_tbp_external, only: box_t
    implicit none (type, external)
    type(box_t) :: b
    call b%show()
end program p_tbp_external
