! DATA statement should emit array literal assignment in generated code
program data_statement_array_literal
    implicit none
    integer :: common_array(3)
    common_array = 0
    data common_array /1, 2, 3/
    print *, common_array(1)
end program data_statement_array_literal
