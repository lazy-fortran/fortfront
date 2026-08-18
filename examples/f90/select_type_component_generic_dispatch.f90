module select_type_component_generic_fixture
  implicit none
  type :: leaf_t
  contains
    generic :: choose => choose_int, choose_real
  end type leaf_t
  type :: ambiguous_leaf_t
  contains
    generic :: choose => choose_left, choose_right
  end type ambiguous_leaf_t
  type :: container_t
    type(leaf_t) :: leaf
    type(ambiguous_leaf_t) :: ambiguous
    type(leaf_t), pointer :: pointer_leaf
    type(leaf_t), allocatable :: allocatable_leaf
  end type container_t
contains
  subroutine inspect_int(object, value)
    class(*), intent(inout) :: object
    integer, intent(in) :: value
    select type (typed => object)
    type is (container_t)
      call typed%leaf%choose(value)
    end select
  end subroutine inspect_int
  subroutine inspect_real(object, value)
    class(*), intent(inout) :: object
    real(8), intent(in) :: value
    select type (typed => object)
    type is (container_t)
      call typed%leaf%choose(value)
    end select
  end subroutine inspect_real
  subroutine inspect_ambiguous(object, value)
    class(*), intent(inout) :: object
    integer, intent(in) :: value
    select type (typed => object)
    type is (container_t)
      call typed%ambiguous%choose(value)
    end select
  end subroutine inspect_ambiguous
  subroutine inspect_pointer(object, value)
    class(*), intent(inout) :: object
    integer, intent(in) :: value
    select type (typed => object)
    type is (container_t)
      call typed%pointer_leaf%choose(value)
    end select
  end subroutine inspect_pointer
  subroutine inspect_allocatable(object, value)
    class(*), intent(inout) :: object
    integer, intent(in) :: value
    select type (typed => object)
    type is (container_t)
      call typed%allocatable_leaf%choose(value)
    end select
  end subroutine inspect_allocatable
  subroutine choose_int(self, value)
    type(leaf_t), intent(inout) :: self
    integer, intent(in) :: value
  end subroutine choose_int
  subroutine choose_real(self, value)
    type(leaf_t), intent(inout) :: self
    real(8), intent(in) :: value
  end subroutine choose_real
  subroutine choose_left(self, value)
    type(ambiguous_leaf_t), intent(inout) :: self
    integer, intent(in) :: value
  end subroutine choose_left
  subroutine choose_right(self, value)
    type(ambiguous_leaf_t), intent(inout) :: self
    integer, intent(in) :: value
  end subroutine choose_right
end module select_type_component_generic_fixture
