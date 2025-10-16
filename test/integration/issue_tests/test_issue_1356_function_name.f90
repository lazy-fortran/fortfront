program test_issue_1356_function_name
    use frontend, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: input_text
    character(len=:), allocatable :: output_text
    character(len=:), allocatable :: error_msg

    call read_example('examples/issue_1356_function_name.lf', input_text)

    call transform_lazy_fortran_string(input_text, output_text, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'FAIL: transformation reported error:'
            print *, trim(error_msg)
            error stop 1
        end if
    end if

    if (.not. allocated(output_text)) then
        print *, 'FAIL: no output produced for issue_1356 example'
        error stop 1
    end if

    if (index(output_text, 'integer function double') == 0) then
        print *, 'FAIL: function double is not emitted with integer return type'
        print *, trim(output_text)
        error stop 1
    end if

    if (index(output_text, 'integer :: x') == 0) then
        print *, 'FAIL: parameter x is not inferred as integer'
        print *, trim(output_text)
        error stop 1
    end if

    if (index(output_text, 'integer :: a') == 0 .or. &
        index(output_text, 'integer :: b') == 0) then
        print *, 'FAIL: caller variables a/b lack inferred integer declarations'
        print *, trim(output_text)
        error stop 1
    end if

    if (index(output_text, 'real function double') > 0 .or. &
        index(output_text, 'real :: double') > 0) then
        print *, 'FAIL: real declarations for double remain in output'
        print *, trim(output_text)
        error stop 1
    end if

    print *, 'PASS: issue_1356 function inference retains integer types'

contains

    subroutine read_example(path, content)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: content
        integer :: unit_id, ios
        character(len=512) :: buffer
        logical :: first_line

        content = ''
        first_line = .true.

        open (newunit=unit_id, file=path, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'FAIL: could not open ', trim(path)
            error stop 1
        end if

        do
            read (unit_id, '(A)', iostat=ios) buffer
            if (ios /= 0) exit
            if (first_line) then
                content = trim(buffer)
                first_line = .false.
            else
                content = content // new_line('a') // trim(buffer)
            end if
        end do
        close (unit_id)

        content = content // new_line('a')
    end subroutine read_example

end program test_issue_1356_function_name
