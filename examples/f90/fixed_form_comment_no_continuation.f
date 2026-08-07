C A fixed-form comment must not become an executable statement.
      program fixed_form_comment_no_continuation
      implicit none
      real x
      x = 3.0
      if (abs(x*x-9.0).gt.1.0e-6) error stop 1
      end
