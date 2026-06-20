! Issue #2820: ISO_C_BINDING pointer intrinsics (C_LOC, C_F_POINTER,
! C_ASSOCIATED) must round-trip in user code.
program issue_2820_c_interop_pointers
    use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer, &
                                           c_associated, c_int
    implicit none
    integer(c_int), target :: x
    integer(c_int), pointer :: p
    type(c_ptr) :: cp

    x = 5
    cp = c_loc(x)
    if (c_associated(cp)) then
        call c_f_pointer(cp, p)
        print *, p
    end if
end program issue_2820_c_interop_pointers
