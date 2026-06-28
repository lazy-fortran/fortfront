module semantic_input_mode
    ! Input mode enumeration for semantic analysis
    ! Defines whether we are processing lazy Fortran or standard Fortran
    implicit none
    private

    public :: INPUT_MODE_LAZY, INPUT_MODE_STANDARD

    ! Input mode enumeration
    integer, parameter :: INPUT_MODE_LAZY = 1 ! Lazy Fortran (.lf files)
    integer, parameter :: INPUT_MODE_STANDARD = 2 ! Standard Fortran (.f90, .f, etc.)

end module semantic_input_mode
