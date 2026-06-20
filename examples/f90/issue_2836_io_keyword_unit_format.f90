program issue_2836_io_keyword_unit_format
    implicit none
    character(10) :: str1
    read (unit=10, fmt='(a10)') str1
end program issue_2836_io_keyword_unit_format
