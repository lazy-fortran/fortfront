program test_comments
    implicit none
    !! This is a comment that should be preserved
    integer :: x

    !! Another comment
    x = 42
    print *, x

contains
    function test_func()
        implicit none
        !! Function comment
        integer :: test_func
        test_func = 123
    end function test_func
end program test_comments
