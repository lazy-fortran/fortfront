! Corrected neighbor of class_is_1.f90: CLASS IS names a type.
program class_is_1_ok
    type t
    end type t
    class(t), allocatable :: x
    allocate (t :: x)
    call s
contains
    subroutine s
        select type (x)
        class is (t)
            print *, 'class is t'
        end select
    end subroutine s
end program class_is_1_ok
