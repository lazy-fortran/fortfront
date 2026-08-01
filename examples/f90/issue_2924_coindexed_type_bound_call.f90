module m_coarray_call
    implicit none
    type :: t0
    contains
        procedure, nopass :: sub => t0_sub
    end type t0
    type :: t1
        type(t0) :: nopoly
    end type t1
    type :: t2
        type(t1) :: c
    end type t2
contains
    subroutine t0_sub()
        print *, 1
    end subroutine t0_sub
end module m_coarray_call
program p_coarray_call
    use m_coarray_call, only: t2
    implicit none
    type(t2) :: x[*]
    call x[1]%c%nopoly%sub()
end program p_coarray_call
