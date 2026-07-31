! Corrected neighbor of type_is_1.f90: TYPE IS names a type.
program type_is_1_ok
    type t
    end type t
    class(t), allocatable :: x
    allocate (t :: x)
    call s
contains
    subroutine s
        select type (x)
        type is (t)
            print *, 'type is t'
        end select
    end subroutine s
end program type_is_1_ok
