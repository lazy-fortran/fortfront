module semantic_operating_mode
    ! Operating mode selection for LFortran standard compliance.
    !
    ! STRICT mode: Forbid global inference of undeclared variables and forbid
    !              bare-statement inputs (enforced in transformation pipeline).
    ! INFER mode:  Allow global type inference for lazy Fortran inputs.
    implicit none
    private

    public :: OPERATING_MODE_INFER
    public :: OPERATING_MODE_STRICT

    integer, parameter :: OPERATING_MODE_INFER = 1
    integer, parameter :: OPERATING_MODE_STRICT = 2

end module semantic_operating_mode
