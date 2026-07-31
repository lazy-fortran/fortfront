! Negative fixture for issue #2888 (reject-scope-02).
! The name Xx is used as a derived-type name and as a variable name in the
! same scoping unit. F2023 19.3.1: within a scoping unit a local identifier
! of class (1) shall not be the same as another class (1) local identifier.
program main
    type Xx
    end type Xx
    real :: Xx

end program main
