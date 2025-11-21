module interface_function_test
    implicit none

    interface
        pure integer function compute_length(n)
            integer, intent(in) :: n
        end function compute_length
    end interface

contains

    function create_string(n) result(str)
        integer, intent(in) :: n
        character(len=compute_length(n)) :: str

        str = ' '
    end function create_string

end module interface_function_test
