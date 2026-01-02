program test_issue_2496_quotes_in_comments
    use transformation_api, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: error_unit, iostat_end
    use, intrinsic :: iso_fortran_env, only: iostat_eor, input_unit
    implicit none
    character(len=:), allocatable :: source, output, error_msg
    logical :: success

    print *, "=== Testing Issue #2496: Quotes in Comments ==="
    print *

    call read_example('examples/f90/issue_2496_quotes_in_comments.f90', source)

    print *, "Parsing source with quotes in comments..."
    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: Transformation failed: ' // error_msg
        stop 1
    end if

    if (.not. allocated(output) .or. len_trim(output) == 0) then
        write (error_unit, '(A)') 'FAIL: Transformation produced no output'
        stop 1
    end if

    print *, "Transformation: SUCCESS"

    success = .true.
    success = success .and. verify_contains(output, 'print *', &
                                            'print statement preserved')
    success = success .and. verify_contains(output, 'hello', &
                                            'string literal preserved')
    success = success .and. verify_contains(output, 'x = 5', &
                                            'assignment preserved')
    success = success .and. verify_contains(output, 'x = x + 3', &
                                            'second assignment preserved')

    if (.not. success) then
        write (error_unit, '(A)') 'FAIL: Output verification failed'
        stop 1
    end if

    print *
    print *, "PASS: All quotes in comments tests passed"
    print *, "      Single quotes in comments handled correctly"
    print *, "      Double quotes in comments handled correctly"
    print *, "      Code preserved correctly"

contains

    include 'common/cli_io_reader.inc'
    include 'common/read_example.inc'

    function verify_contains(text, pattern, description) result(found)
        character(len=*), intent(in) :: text, pattern, description
        logical :: found

        found = index(text, pattern) > 0
        if (found) then
            print *, "  PASS: " // trim(description)
        else
            write (error_unit, '(A)') '  FAIL: ' // trim(description)
            write (error_unit, '(A)') '        Expected pattern: ' // trim(pattern)
        end if
    end function verify_contains



end program test_issue_2496_quotes_in_comments
