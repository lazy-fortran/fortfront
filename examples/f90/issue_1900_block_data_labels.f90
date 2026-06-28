program issue_1900_block_data_labels
    implicit none
    integer :: a, b
    real :: x, y
    common /myblock/ a, b, x, y

    print *, 'a, b:', a, b
    print *, 'x, y:', x, y
end program issue_1900_block_data_labels

123 block data init_data
implicit none
integer :: a, b
real :: x, y
common /myblock/ a, b, x, y
data a, b / 10, 20 /
data x, y / 3.5, 7.2 /
123 end block data init_data
