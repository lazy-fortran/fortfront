program test_io_control_specifiers
    use transformation_api, only: transform_context_t, transform_with_context
    use frontend_transformation, only: INPUT_MODE_STANDARD
    implicit none
    character(len=:), allocatable :: source, output, error_msg
    type(transform_context_t) :: ctx

    call read_example('examples/f90/issue_2447_io_format.f90', source)

    ctx%input_mode = INPUT_MODE_STANDARD
    ctx%has_filename = .false.

    call transform_with_context(source, output, error_msg, ctx)

    if (.not. allocated(output)) then
        print *, "FAIL: Transformation produced no output"
        error stop 1
    end if

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, "FAIL: Transformation error:", trim(error_msg)
            error stop 1
        end if
    end if

    if (.not. index(output, 'iostat=io_stat') > 0) then
        print *, "FAIL: Missing iostat in READ statement"
        error stop 1
    end if

    if (.not. index(output, 'iomsg=io_msg') > 0) then
        print *, "FAIL: Missing iomsg in READ statement"
        error stop 1
    end if

    print *, "PASS: I/O control specifiers preserved"

contains

    subroutine read_example(filepath, content)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: content
        integer :: file_unit, file_size, iostat
        character(len=1024) :: line
        logical :: exists

        inquire(file=filepath, exist=exists, size=file_size)
        if (.not. exists) then
            print *, "ERROR: File not found:", filepath
            error stop 1
        end if

        open(newunit=file_unit, file=filepath, status='old', action='read', &
             iostat=iostat)
        if (iostat /= 0) then
            print *, "ERROR: Cannot open file:", filepath
            error stop 1
        end if

        content = ""
        do
            read(file_unit, '(A)', iostat=iostat) line
            if (iostat /= 0) exit
            if (len(content) > 0) content = content // new_line('a')
            content = content // trim(line)
        end do

        close(file_unit)
    end subroutine read_example

end program test_io_control_specifiers
