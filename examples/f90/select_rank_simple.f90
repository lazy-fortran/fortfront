program test_select_rank
  implicit none
  integer :: x(5), y(3,3)

  call process_array(x)
  call process_array(y)

contains

  subroutine process_array(arr)
    integer, intent(inout) :: arr(..)

    select rank(arr)
    rank(1)
      print *, "Processing rank-1 array"
      arr = 42
    rank(2)
      print *, "Processing rank-2 array"
      arr = 99
    rank default
      print *, "Unknown rank"
    end select
  end subroutine process_array

end program test_select_rank
