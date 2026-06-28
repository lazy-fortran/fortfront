! Test array constructor in argument list
program test_array_constructor_arg
    implicit none

    call process([integer :: 1, 2, 3])
    call process([real :: 1.0, 2.0, 3.0])

contains
    subroutine process(arr)
        class(*), intent(in) :: arr(:)
        select type (arr)
            type is (integer)
            print *, "Integer array:", arr
            type is (real)
            print *, "Real array:", arr
        end select
    end subroutine process
end program test_array_constructor_arg
