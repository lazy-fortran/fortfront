program issue_2836_io_expression_format
    implicit none
    character(20) :: buffer
    character(4) :: fmt
    integer :: stat, value
    value = 5
    fmt = 'i4'
    write (buffer, '('//fmt//')', iostat=stat) value
end program issue_2836_io_expression_format
