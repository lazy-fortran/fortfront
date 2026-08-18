module dispatch_refusals
  implicit none
  type, abstract :: base_t
  contains
    procedure(run_interface), deferred :: run
    procedure, nopass :: first
    procedure, nopass :: second
    generic :: ambiguous => first, second
  end type base_t
  type, abstract, extends(base_t) :: middle_t
  end type middle_t
  type, extends(middle_t) :: leaf_t
  contains
    procedure :: run => leaf_run
  end type leaf_t
  type :: unrelated_t
  contains
    procedure :: run => unrelated_run
  end type unrelated_t
  abstract interface
    subroutine run_interface(self)
      import base_t
      class(base_t) :: self
    end subroutine run_interface
  end interface
contains
  subroutine inspect(value)
    class(base_t), intent(inout) :: value
    select type (value)
    type is (middle_t)
      call value%run()
    class is (unrelated_t)
      call value%run()
    class default
      call value%run()
    end select
  end subroutine inspect
  subroutine inspect_pointer(value)
    class(base_t), pointer, intent(inout) :: value
    select type (value)
    type is (leaf_t)
      call value%run()
    end select
  end subroutine inspect_pointer
  subroutine inspect_owned(value)
    class(base_t), allocatable, intent(inout) :: value
    select type (value)
    type is (leaf_t)
      call value%run()
    end select
  end subroutine inspect_owned
  subroutine leaf_run(self)
    class(leaf_t) :: self
  end subroutine leaf_run
  subroutine unrelated_run(self)
    type(unrelated_t) :: self
  end subroutine unrelated_run
  subroutine first()
  end subroutine first
  subroutine second()
  end subroutine second
end module dispatch_refusals
