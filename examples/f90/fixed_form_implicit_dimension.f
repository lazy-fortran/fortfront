      program fixed_form_implicit_dimension
      real pair(3)
      pair(1) = 2.0
      pair(2) = 3.0
      pair(3) = 5.0
      call scale_pair(pair)
      if (abs(pair(1)-4.0) .gt. 1.0e-6) error stop 1
      if (abs(pair(2)-9.0) .gt. 1.0e-6) error stop 1
      if (abs(pair(3)-5.0) .gt. 1.0e-6) error stop 1
      end
      subroutine scale_pair(x)
      dimension x(2)
      if (size(x) .ne. 2) error stop 1
      x(1) = 2.0*x(1)
      x(2) = 3.0*x(2)
      end
