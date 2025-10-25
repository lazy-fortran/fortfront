program typed_array_issue_1897
    implicit none
    real :: arr1(5)
    integer :: arr2(3)

    arr1 = (/ real :: 1, 2, 3, 4, 5 /)
    arr2 = (/ integer :: 1.5, 2.7, 3.9 /)

    print *, 'Real array:', arr1
    print *, 'Integer array:', arr2
end program typed_array_issue_1897
