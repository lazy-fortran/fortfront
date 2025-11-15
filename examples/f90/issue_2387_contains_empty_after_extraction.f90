program test_assumed_rank_extraction
    implicit none
    integer :: i = 42
    integer :: result

    interface
        integer function external_func(x) bind(c)
            integer, intent(in) :: x
        end function external_func
    end interface

    result = process_data(i)
    print *, result

contains

    integer function process_data(data)
        type(*), dimension(..), optional, intent(in) :: data
        if (present(data)) then
            process_data = 1
        else
            process_data = 2
        endif
    end function process_data

end program test_assumed_rank_extraction