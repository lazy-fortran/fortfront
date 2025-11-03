! Issue #1611: plain allocate without type-spec remains untouched
program test_allocate_plain
    implicit none

    integer, allocatable :: arr(:)
    allocate(arr(10))
    arr = [(i, i = 1, 10)]
end program test_allocate_plain
