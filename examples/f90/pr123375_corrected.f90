! Corrected neighbour of pr123375.f90 (issue #2888).
! AA is use associated in the host scoping unit and imported into the
! interface body, which is the legal shape.
module pr123375_corrected_mod
    implicit none
    integer :: aa
end module pr123375_corrected_mod

module pr123375_corrected
    use pr123375_corrected_mod
    implicit none
    interface
        subroutine bah()
            import aa
        end subroutine bah
    end interface
end module pr123375_corrected
