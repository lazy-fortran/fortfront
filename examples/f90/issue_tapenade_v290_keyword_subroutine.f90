module data_types
  integer, parameter :: mcell = 100
  type griddata
    sequence
    real(kind=8), dimension(mcell) :: x
    real(kind=8), dimension(mcell) :: y
  end type griddata
  type solutiondata
    sequence
    integer :: n
    real(kind=8), dimension(mcell) :: a, b, c
  end type solutiondata
end module data_types

subroutine function(grddat, soldat)
  use data_types
  implicit none
  type(griddata) :: grddat
  type(solutiondata), dimension(2) :: soldat
  soldat(1)%a = soldat(2)%b * grddat%x + soldat(2)%c + grddat%y
end subroutine function

subroutine main(grddat, soldat)
  use data_types
  implicit none
  type(griddata) :: grddat
  type(solutiondata), dimension(2) :: soldat
  interface
    subroutine function(grddat, soldat)
      use data_types
      type(griddata) :: grddat
      type(solutiondata), dimension(2) :: soldat
    end subroutine function
  end interface
  call function(grddat, soldat)
end subroutine main
