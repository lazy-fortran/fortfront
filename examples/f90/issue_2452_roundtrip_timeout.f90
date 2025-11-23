! Test case covering patterns from roundtrip_timeout issue #2452
! This file tests that complex array intrinsics with nested calls
! can successfully roundtrip (parse -> emit -> parse again)
program test_roundtrip_timeout_patterns
    implicit none
    integer :: arr(3,3), arr2(3,3)
    integer :: lb(3), ub(3)
    integer :: res(3), idx(2)

    ! Pattern 1: lbound/ubound with spread and array slices
    arr = reshape([1,2,3,4,5,6,7,8,9], [3,3])
    lb = lbound(spread(arr(:,1:2), dim=2, ncopies=3))
    ub = ubound(spread(arr(:,1:2), dim=2, ncopies=3))

    ! Pattern 2: maxloc/minloc with dim and mask
    res = maxloc(arr, dim=1, mask=arr > 5)
    res = minloc(arr, dim=1, mask=arr < 5)

    ! Pattern 3: maxval/minval with dim and mask
    res = maxval(arr, dim=1, mask=arr > 3)
    res = minval(arr, dim=1, mask=arr < 7)

    ! Pattern 4: Nested array operations with padding
    arr2 = reshape(maxval(arr, dim=1), [3,3], pad=[0])

    ! Pattern 5: Multiple intrinsic arguments
    idx = maxloc(arr)
    idx = minloc(arr)

    print *, "lbound:", lb
    print *, "ubound:", ub
    print *, "results:", res
    print *, "indices:", idx
end program test_roundtrip_timeout_patterns
