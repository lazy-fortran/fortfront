program inquire_nested_specifiers
    character(64) :: filename
    logical :: found
    filename = '  absent_entry'
    inquire(file=trim(adjustl(filename)), exist=found)
    print *, found
    inquire(file=')', exist=found)
    print *, found
end program inquire_nested_specifiers
