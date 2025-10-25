! Issue #1409: USE renames rewritten incorrectly
module constants
    implicit none
    integer, parameter :: ten = 10
end module constants

program rename_test
    use constants, only: dozen => ten
    implicit none
    print *, dozen
end program rename_test
