! Corrected neighbours: the `::` separator and a single POINTER attribute.
! NOPASS is required because the procedure pointer has no explicit interface.
program p
  type :: t
    procedure(), pointer, nopass :: ptr4
    procedure(), pointer, nopass :: ptr5
  end type t
end program p
