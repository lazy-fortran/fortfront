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


    include '../common/read_example.inc'
end program test_io_control_specifiers
