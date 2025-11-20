! Verifies custom program names are preserved across roundtrip
program custom_program_name
    implicit none
    integer :: value
    value = 42
    print *, value
end program custom_program_name
