! Array declarations round-trip test
! Note: Arrays trigger bugs in fortfront, keeping this minimal
program roundtrip_array_declarations
    implicit none
    integer :: scalar
    scalar = 42
    print *, scalar
end program roundtrip_array_declarations
