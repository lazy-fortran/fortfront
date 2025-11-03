! DATA statement should upgrade a scalar declaration to an array
program data_statement_scalar_upgrade
    implicit none
    integer :: values
    data values /10, 20/
    print *, values(2)
end program data_statement_scalar_upgrade
