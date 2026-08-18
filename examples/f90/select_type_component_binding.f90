module select_type_component_binding_fixture
  implicit none
  type, abstract :: base_t
  contains
    procedure(run_interface), deferred, pass(self) :: run
  end type base_t
  type, extends(base_t) :: impl_t
  contains
    procedure, pass(self) :: run => impl_run
  end type impl_t
  type, extends(impl_t) :: leaf_t
    real :: value
  end type leaf_t
  type :: generic_t
    integer :: value
  contains
    generic :: choose => choose_left, choose_right
  end type generic_t
  type, extends(base_t) :: container_t
    type(leaf_t) :: leaf
    type(generic_t) :: generic
    class(base_t), pointer :: dynamic
    type(leaf_t), allocatable :: owned
  contains
    procedure, pass(self) :: run => container_run
  end type container_t
  abstract interface
    subroutine run_interface(self)
      import base_t
      class(base_t), intent(inout) :: self
    end subroutine run_interface
  end interface
contains
  subroutine inspect(box)
    class(base_t), intent(inout) :: box
    select type (typed => box)
    type is (container_t)
      typed%leaf%value = typed%leaf%value
      typed%generic%value = typed%generic%value
      typed%dynamic => box
      typed%owned%value = typed%owned%value
    end select
  end subroutine inspect
  subroutine impl_run(self)
    class(impl_t), intent(inout) :: self
  end subroutine impl_run
  subroutine container_run(self)
    class(container_t), intent(inout) :: self
  end subroutine container_run
  subroutine choose_left(self)
    type(generic_t), intent(inout) :: self
  end subroutine choose_left
  subroutine choose_right(self)
    type(generic_t), intent(inout) :: self
  end subroutine choose_right
end module select_type_component_binding_fixture
