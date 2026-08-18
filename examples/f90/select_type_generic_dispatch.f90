module select_type_generic_dispatch_fixture
  implicit none
  type :: generic_t
  contains
    generic :: choose => choose_int, choose_real
  end type generic_t
  type :: ambiguous_t
  contains
    generic :: choose => choose_left, choose_right
  end type ambiguous_t
  contains
  subroutine inspect_int(object, integer_value)
    class(*), intent(inout) :: object
    integer, intent(in) :: integer_value
    select type (object)
    type is (generic_t)
      call object%choose(integer_value)
    end select
  end subroutine inspect_int
  subroutine inspect_real(object, real_value)
    class(*), intent(inout) :: object
    real(8), intent(in) :: real_value
    select type (object)
    type is (generic_t)
      call object%choose(real_value)
    end select
  end subroutine inspect_real
  subroutine inspect_pointer(object, value)
    class(*), pointer, intent(inout) :: object
    integer, intent(in) :: value
    select type (object)
    type is (generic_t)
      call object%choose(value)
    end select
  end subroutine inspect_pointer
  subroutine inspect_allocatable(object, value)
    class(*), allocatable, intent(inout) :: object
    integer, intent(in) :: value
    select type (object)
    type is (generic_t)
      call object%choose(value)
    end select
  end subroutine inspect_allocatable
  subroutine inspect_ambiguous(object, value)
    class(*), intent(inout) :: object
    integer, intent(in) :: value
    select type (object)
    type is (ambiguous_t)
      call object%choose(value)
    end select
  end subroutine inspect_ambiguous
  subroutine choose_int(self, value)
    type(generic_t), intent(inout) :: self
    integer, intent(in) :: value
  end subroutine choose_int
  subroutine choose_real(self, value)
    type(generic_t), intent(inout) :: self
    real(8), intent(in) :: value
  end subroutine choose_real
  subroutine choose_left(self, value)
    type(ambiguous_t), intent(inout) :: self
    integer, intent(in) :: value
  end subroutine choose_left
  subroutine choose_right(self, value)
    type(ambiguous_t), intent(inout) :: self
    integer, intent(in) :: value
  end subroutine choose_right
end module select_type_generic_dispatch_fixture
