! ERROR-TEST: DATA statement with more values than variables
program data_statement_scalar_upgrade
    implicit none
    integer :: values
    data values/10, 20/
    print *, values
end program data_statement_scalar_upgrade
