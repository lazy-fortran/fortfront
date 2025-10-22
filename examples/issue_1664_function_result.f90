program test_function_result
    implicit none
    real, dimension(3) :: x, result_array

    x = [1.0, 2.0, 3.0]
    result_array = double_array(x)
    print *, 'Input:', x
    print *, 'Output:', result_array

contains

    function double_array(arr) result(out)
        real, dimension(3), intent(in) :: arr
        real, dimension(3) :: out
        out = arr * 2.0
    end function double_array

end program test_function_result
