program recursive_interface
  call c(b1)
contains
  subroutine a1(x)
    real :: x
  end subroutine a1
  subroutine a2(i)
    integer :: i
  end subroutine a2
  subroutine b1(f1)
    procedure(a1) :: f1
  end subroutine b1
  subroutine b2(f2)
    procedure(a2) :: f2
  end subroutine b2
  subroutine c(g)
    procedure(b1) :: g
  end subroutine c
end program recursive_interface
