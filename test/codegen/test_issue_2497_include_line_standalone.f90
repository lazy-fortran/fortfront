program test_issue_2497_include_line_standalone
    use transformation_api, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: errors

    call read_example('examples/f90/issue_2497_include_line_standalone.f90', source)

    call transform_lazy_fortran_string(source, output, errors)

    if (len_trim(errors) > 0) then
        print *, "Unexpected errors:", trim(errors)
        error stop 1
    end if

    if (index(output, "include") == 0) then
        print *, "ERROR: INCLUDE line was dropped during round-trip"
        error stop 1
    end if

    if (index(output, "'other_file.f90'") == 0) then
        print *, "ERROR: INCLUDE filename was not preserved"
        error stop 1
    end if

    print *, "PASS: Standalone INCLUDE line preserved correctly"

contains

    subroutine read_example(filepath, content)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: content
        integer :: unit_num, ios, file_size
        character(len=1), allocatable :: buffer(:)
        integer :: i

        open (newunit=unit_num, file=filepath, status='old', &
              action='read', access='stream', iostat=ios)
        if (ios /= 0) then
            print *, "ERROR: Could not open file:", filepath
            error stop 1
        end if

        inquire (unit=unit_num, size=file_size)
        allocate (buffer(file_size))
        read (unit_num, iostat=ios) buffer
        close (unit_num)

        if (ios /= 0) then
            print *, "ERROR: Could not read file:", filepath
            error stop 1
        end if

        allocate (character(len=file_size) :: content)
        do i = 1, file_size
            content(i:i) = buffer(i)
        end do
    end subroutine read_example

end program test_issue_2497_include_line_standalone
