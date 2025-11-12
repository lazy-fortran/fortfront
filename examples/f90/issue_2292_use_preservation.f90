subroutine reorder(a)
    use iso_c_binding, only: c_ptr
    implicit none
    integer :: i
    type(c_ptr), intent(in) :: a
    integer :: j

    i = 0
    j = 1
end subroutine reorder
