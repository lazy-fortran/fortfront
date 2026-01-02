    subroutine read_example(filepath, content)
        use, intrinsic :: iso_fortran_env, only: error_unit
        implicit none
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: content
        integer :: status

        call read_all_stdin_or_file(.true., filepath, content, status)
        if (status /= 0) then
            write (error_unit, '(A)') 'FAIL: failed to read ' // trim(filepath)
            error stop 1
        end if
    end subroutine read_example
