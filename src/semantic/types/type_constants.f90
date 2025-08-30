module type_constants
    ! Type constants for both traditional and arena-based type systems
    ! Separated to avoid importing problematic self-referential allocatable types
    implicit none
    public

    ! Type kinds for Hindley-Milner system
    integer, parameter :: TVAR = 1      ! Type variable
    integer, parameter :: TINT = 2      ! Integer type
    integer, parameter :: TREAL = 3     ! Real type
    integer, parameter :: TCHAR = 4     ! Character type
    integer, parameter :: TLOGICAL = 5  ! Logical type
    integer, parameter :: TFUN = 6      ! Function type
    integer, parameter :: TARRAY = 7    ! Array type
    integer, parameter :: TCOMPLEX = 8  ! Complex type
    integer, parameter :: TDOUBLE = 9   ! Double precision type
    integer, parameter :: TDERIVED = 10 ! Derived/user-defined type

end module type_constants
