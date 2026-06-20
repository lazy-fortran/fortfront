program test_c_f_procpointer
    use, intrinsic :: iso_c_binding
    implicit none
    type(c_funptr) :: cfptr

    abstract interface
        subroutine sub()
        end subroutine
    end interface

    procedure(sub), pointer :: fsub
    call c_f_procpointer(cfptr, fsub)
end program test_c_f_procpointer
