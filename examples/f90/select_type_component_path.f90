module select_type_component_path_fixture
  implicit none
  type, abstract :: base_t
  end type base_t
  type :: leaf_t
    real :: value
  end type leaf_t
  type, extends(base_t) :: child_t
    type(leaf_t) :: leaf
    class(base_t), pointer :: dynamic
  end type child_t
contains
  subroutine inspect(box)
    class(base_t), intent(inout) :: box
    select type (typed => box)
    type is (child_t)
      typed%leaf%value = typed%leaf%value + 1.0
      typed%dynamic => box
    class default
      continue
    end select
  end subroutine inspect
end module select_type_component_path_fixture
