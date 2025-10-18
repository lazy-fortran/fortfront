module frontend_utilities
    ! fortfront - Utility functions module
    ! Contains helper functions and utilities

    use string_utils_mod, only: int_to_string
    implicit none
    private

    public :: write_output_file, int_to_str

contains

    ! Write output to file
    subroutine write_output_file(filename, content, error_msg)
        character(len=*), intent(in) :: filename, content
        character(len=*), intent(out) :: error_msg

        integer :: unit, iostat
        character(len=:), allocatable :: sanitized

        open (newunit=unit, file=filename, status='replace', action='write', iostat=iostat)
        if (iostat /= 0) then
            error_msg = "Cannot create output file: " // filename
            return
        end if

        sanitized = strip_space_before_comma(content)
        write (unit, '(A)') sanitized
        close (unit)
        error_msg = ""
    end subroutine write_output_file

    function strip_space_before_comma(text) result(clean)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: clean
        character(len=len(text)) :: buffer
        integer :: i, pos

        pos = 0
        do i = 1, len(text)
            if (text(i:i) == ',' .and. pos > 0) then
                if (buffer(pos:pos) == ' ') pos = pos - 1
            end if
            pos = pos + 1
            buffer(pos:pos) = text(i:i)
        end do

        if (pos > 0) then
            clean = buffer(1:pos)
        else
            clean = ''
        end if
    end function strip_space_before_comma

    ! Helper function to convert integer to string
    function int_to_str(num) result(str)
        integer, intent(in) :: num
        character(len=20) :: str

        str = int_to_string(num)
    end function int_to_str

end module frontend_utilities
