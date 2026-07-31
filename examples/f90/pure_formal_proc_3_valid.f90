program pure_formal_proc_3_valid
    ! VALID neighbour of pure_formal_proc_3.f90. The dummy procedure of the
    ! PURE function is itself declared PURE, and the impure dummy procedure is
    ! only used by an impure function.
    implicit none

contains

    pure function f(proc) result(res)
        integer :: res
        interface
            pure function proc()
                integer :: proc
            end function proc
        end interface
        res = proc()
    end function f

    function g(proc) result(res)
        integer :: res
        interface
            function proc()
                integer :: proc
            end function proc
        end interface
        res = proc()
    end function g

end program pure_formal_proc_3_valid
