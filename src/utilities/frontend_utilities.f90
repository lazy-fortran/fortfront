module frontend_utilities
    ! fortfront - Utility functions module
    ! Contains helper functions and utilities

    use path_validation, only: validate_output_path, path_validation_result_t

    implicit none
    private

    public :: write_output_file, int_to_str

contains

    ! Write output to file
    subroutine write_output_file(filename, content, error_msg)
        character(len=*), intent(in) :: filename, content
        character(len=*), intent(out) :: error_msg

        integer :: unit, iostat
        type(path_validation_result_t) :: validation_result
        character(len=:), allocatable :: sanitized

        ! Validate output file path for security
        validation_result = validate_output_path(filename)
        if (.not. validation_result%is_valid()) then
            error_msg = "Output path validation failed: " // validation_result%get_message()
            return
        end if

        open (newunit=unit, file=filename, status='replace', action='write', iostat=iostat)
        if (iostat /= 0) then
            error_msg = "Cannot create output file: "//filename
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
        write (str, '(I0)') num
    end function int_to_str

end module frontend_utilities
