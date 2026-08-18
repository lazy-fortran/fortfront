! gfortran.dg proc_ptr_comp_3.f90: malformed procedure-pointer component
! declarations must be rejected with a diagnostic.
program p
  type :: t
    procedure(), pointer, nopass ptr4              ! Expected '::'
    procedure(), pointer, nopass, pointer :: ptr5  ! Duplicate POINTER attribute
  end type t
end program p
