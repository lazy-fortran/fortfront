program test_case_3
    implicit none
    character(len=5) :: result_str
    result_str = get_name(5)
contains
    function get_name(n) result(str)
        integer, intent(in) :: n
        character(len=n) :: str
        str = repeat("A", n)
    end function get_name
end program test_case_3
