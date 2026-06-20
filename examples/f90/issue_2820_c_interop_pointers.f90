! Issue #2820: ISO_C_BINDING pointer intrinsics (C_LOC, C_F_POINTER,
! C_ASSOCIATED) must round-trip in user code, including the SHAPE argument
! of C_F_POINTER and the two-argument form of C_ASSOCIATED.
program issue_2820_c_interop_pointers
    use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer, &
                                           c_associated, c_int
    implicit none
    integer(c_int), target :: x
    integer(c_int), target :: a(4)
    integer(c_int), pointer :: p
    integer(c_int), pointer :: parr(:)
    type(c_ptr) :: cp, cp2

    x = 5
    cp = c_loc(x)
    if (c_associated(cp)) then
        call c_f_pointer(cp, p)
        print *, p
    end if

    a = [1, 2, 3, 4]
    cp2 = c_loc(a)
    if (c_associated(cp2, cp2)) then
        call c_f_pointer(cp2, parr, [4])
        print *, parr
    end if
end program issue_2820_c_interop_pointers
