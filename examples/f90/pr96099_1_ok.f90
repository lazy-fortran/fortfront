! Corrected neighbor of pr96099_1.f90: the letter-spec list holds letters.
module pr96099_1_ok_mod
    type t
        integer :: i = 1
    end type t
end module pr96099_1_ok_mod

program pr96099_1_ok
    use pr96099_1_ok_mod, only: t
    implicit class(t) (x)
    print *, 'ok'
end program pr96099_1_ok
