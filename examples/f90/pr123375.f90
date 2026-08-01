! Negative fixture for issue #2888 (reject-scope-02), after gfortran.dg.
! F2023 C8105: an interface body that accesses AA by use association may not
! also IMPORT it from the host scoping unit.
module pr123375_mod
    implicit none
    integer :: aa
end module pr123375_mod

module pr123375_bad
    implicit none
    interface
        subroutine bah()
            use pr123375_mod
            import aa
        end subroutine bah
    end interface
end module pr123375_bad
