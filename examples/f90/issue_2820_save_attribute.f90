! Issue #2820: SAVE attribute must round-trip through the frontend.
program issue_2820_save_attribute
    implicit none
    integer, save :: counter = 0

    counter = counter + 1
    print *, counter
end program issue_2820_save_attribute
