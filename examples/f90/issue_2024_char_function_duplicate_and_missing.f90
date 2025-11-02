program test_char_function
    implicit none
    character(len=20) :: result

    result = uppercase('hello')
    print *, 'Result:', trim(result)

contains

    function uppercase(str) result(upper)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: upper
        integer :: i, code

        do i = 1, len(str)
            code = ichar(str(i:i))
            if (code >= 97 .and. code <= 122) then
                upper(i:i) = char(code - 32)
            else
                upper(i:i) = str(i:i)
            end if
        end do
    end function uppercase

end program test_char_function
