program issue_1900_block_data_after_program
    implicit none
    integer :: a, b
    real :: x, y
    common /myblock/ a, b, x, y

    print *, a, b
    print *, x, y
end program issue_1900_block_data_after_program

block data init_data
    implicit none
    integer :: a, b
    real :: x, y
    common /myblock/ a, b, x, y
    data a, b / 10, 20 /
    data x, y / 3.5, 7.2 /
end block data init_data
