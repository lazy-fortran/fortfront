! Test for issue #2419 - parser incorrectly treats variable named data
! as DATA statement keyword inside allocate() statement
program test_data_variable
    implicit none
    type :: foo
        integer, pointer :: a => null()
    end type foo
    type(foo), allocatable :: data(:,:)

    ! This should parse correctly - data is a variable name, not DATA statement
    allocate(data(1:1, 1))

    deallocate(data)
end program test_data_variable
