C Fixed-form OpenACC directive test
      program testopenacc
      use iso_fortran_env, only: dp => real64
      implicit none
      integer :: i, n
      real(dp) :: a(100), b(100), c(100)

      n = 100
!$acc parallel loop
      do i = 1, n
        c(i) = a(i) + b(i)
      end do
!$acc end parallel loop

      print *, 'Done'
      end program testopenacc
