program test_issue_2012_subroutine_local_inference
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    call test_subroutine_local_variable()
    print *, "Issue 2012 subroutine local variable inference test completed."

contains

    subroutine test_subroutine_local_variable()
        character(:), allocatable :: source, output, error_msg

        call read_example('examples/lf/issue_2012_subroutine_local_not_inferred.lf', &
                          source)

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "FAIL: Transformation failed:", trim(error_msg)
            error stop 1
        end if

        if (index(output, 'integer :: temp') == 0) then
            print *, "FAIL: Local variable 'temp' not declared"
            print *, "Output:", output
            error stop 1
        end if

        if (index(output, 'subroutine swap') == 0) then
            print *, "FAIL: Subroutine not found in output"
            error stop 1
        end if

        if (index(output, 'implicit none') == 0) then
            print *, "FAIL: implicit none not found"
            error stop 1
        end if

        print *, "[PASS] Subroutine local variable correctly inferred and declared"
    end subroutine test_subroutine_local_variable

    subroutine read_example(filepath, content)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: content
        integer :: unit, ios, file_size
        character(len=1), allocatable :: buffer(:)

        open (newunit=unit, file=filepath, status='old', action='read', &
              form='unformatted', access='stream', iostat=ios)
        if (ios /= 0) then
            print *, "Error opening file:", filepath
            error stop 1
        end if

        inquire (unit=unit, size=file_size)
        allocate (buffer(file_size))
        read (unit, iostat=ios) buffer
        close (unit)

        if (ios /= 0) then
            print *, "Error reading file:", filepath
            error stop 1
        end if

        allocate (character(len=file_size) :: content)
        content = transfer(buffer, content)
        deallocate (buffer)
    end subroutine read_example

end program test_issue_2012_subroutine_local_inference
