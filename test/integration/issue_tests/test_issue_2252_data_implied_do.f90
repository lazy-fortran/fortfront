program test_issue_2252_data_implied_do
    use fortfront, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg

    print *, "=== Test Issue #2252: DATA implied-do value lists ==="

    ! Test the reproducer from issue #2252
    call read_example('examples/f90/issue_2252_data_implied_do.f90', source)
    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        print *, "ERROR: Parsing failed with message:"
        print *, trim(error_msg)
        error stop 1
    end if

    if (.not. allocated(output)) then
        print *, "ERROR: No output generated"
        error stop 1
    end if

    ! Check that the output contains the data statement (roundtrip)
    if (index(output, 'data coeff') == 0 .and. index(output, 'DATA coeff') == 0) then
        print *, "ERROR: DATA statement not found in output"
        print *, "Output:", output
        error stop 1
    end if

    print *, "PASSED: DATA implied-do loops parse correctly"

contains

    subroutine read_example(filepath, content)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: content
        integer :: unit, iostat, file_size
        character(len=1), allocatable :: buffer(:)
        integer :: i

        open (newunit=unit, file=trim(filepath), status='old', &
              action='read', iostat=iostat, access='stream')
        if (iostat /= 0) then
            print *, "ERROR: Cannot open file: ", trim(filepath)
            error stop 1
        end if

        inquire (unit=unit, size=file_size)
        allocate (buffer(file_size))
        read (unit, iostat=iostat) buffer
        close (unit)

        if (iostat /= 0) then
            print *, "ERROR: Cannot read file: ", trim(filepath)
            error stop 1
        end if

        allocate (character(len=file_size) :: content)
        do i = 1, file_size
            content(i:i) = buffer(i)
        end do
    end subroutine read_example

end program test_issue_2252_data_implied_do
