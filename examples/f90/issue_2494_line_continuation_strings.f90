program test_long_print_line
    integer :: array(50)
    array = 1
    print "(a, i8)", "Line 1", array(2), "Line 3", array(4), "Line 5", array(6), &
        "Line 7", array(8), "Line 9", array(10), "Line 11", array(12), &
        "Line 13", array(14)
end program
