! Test that fortfront preserves explicit result types in function signatures
! when using result() clause (fixes #1735)
module test_issue_1735_module
    implicit none
contains

    integer function square(x) result(result)
        implicit none
        integer :: x
        result = x * x
    end function square
    
end module test_issue_1735_module

program test_issue_1735
    use test_issue_1735_module
    implicit none
    integer :: val, squared
    val = 5
    squared = square(val)
    print *, squared
end program test_issue_1735
