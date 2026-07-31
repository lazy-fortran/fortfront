program pure_formal_proc_3
    ! INVALID: F2008 C1290. A dummy procedure of a PURE procedure shall itself
    ! be PURE, otherwise the pure procedure could reference impure code.
    implicit none

contains

    pure function f(proc) result(res)
        integer :: res
        interface
            function proc()
                integer :: proc
            end function proc
        end interface
        res = 0
    end function f

end program pure_formal_proc_3
