program test_issue_1356_function_name
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
                                                                                iostat_eor
    use frontend, only: transform_lazy_fortran_string
    use lexer_core, only: to_lower
    implicit none

    character(len=:), allocatable :: input_text
    character(len=:), allocatable :: output_text
    character(len=:), allocatable :: lower_output_text
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

    lower_output_text = to_lower(output_text)

    if (index(lower_output_text, 'integer function double') == 0) then
        print *, 'FAIL: function double is not emitted with integer return type'
        print *, trim(output_text)
        error stop 1
    end if

    if (index(lower_output_text, 'integer :: x') == 0) then
        print *, 'FAIL: parameter x is not inferred as integer'
        print *, trim(output_text)
        error stop 1
    end if

    if (index(lower_output_text, 'integer :: a') == 0 .or. &
        index(lower_output_text, 'integer :: b') == 0) then
        print *, 'FAIL: caller variables a/b lack inferred integer declarations'
        print *, trim(output_text)
        error stop 1
    end if

    if (index(lower_output_text, 'real function double') > 0 .or. &
        index(lower_output_text, 'real :: double') > 0) then
        print *, 'FAIL: real declarations for double remain in output'
        print *, trim(output_text)
        error stop 1
    end if

    print *, 'PASS: issue_1356 function inference retains integer types'

contains

    include '../../common/cli_io_reader.inc'

    subroutine read_example(path, content)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: content
        integer :: status

        call read_all_stdin_or_file(.true., path, content, status)
        if (status /= 0) then
            print *, 'FAIL: failed to read ', trim(path)
            error stop 1
        end if
    end subroutine read_example

end program test_issue_1356_function_name
