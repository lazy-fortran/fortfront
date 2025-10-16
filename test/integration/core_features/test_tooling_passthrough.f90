program test_tooling_passthrough
    use frontend, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: input_text
    character(len=:), allocatable :: output_text
    character(len=:), allocatable :: error_msg
    character(len=:), allocatable :: expected
    character(len=512) :: line_buffer
    integer :: unit_num
    integer :: ios
    logical :: first_line

    input_text = ''
    first_line = .true.
    open (newunit=unit_num, file='examples/external_tool_example.f90', &
          status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, 'FAIL: could not open examples/external_tool_example.f90'
        stop 1
    end if

    do
        read (unit_num, '(A)', iostat=ios) line_buffer
        if (ios /= 0) exit
        if (first_line) then
            input_text = trim(line_buffer)
            first_line = .false.
        else
            input_text = input_text // new_line('a') // trim(line_buffer)
        end if
    end do
    close (unit_num)

    if (first_line) then
        print *, 'FAIL: external_tool_example.f90 was empty'
        stop 1
    end if

    input_text = input_text // new_line('a')
    expected = input_text

    call transform_lazy_fortran_string(input_text, output_text, error_msg)

    if (.not. allocated(output_text)) then
        print *, 'FAIL: passthrough produced no output'
        stop 1
    end if

    if (output_text /= expected) then
        print *, 'FAIL: tooling example should be unchanged by transformation'
        print *, 'Expected:'
        print *, trim(expected)
        print *, 'Actual:'
        print *, trim(output_text)
        stop 1
    end if

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'FAIL: unexpected error message:'
            print *, trim(error_msg)
            stop 1
        end if
    end if

    print *, 'PASS: tooling passthrough preserved external tooling example'
end program test_tooling_passthrough
