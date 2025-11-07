program test_issue_2155_character_length_collision
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
                                             iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: errors

    print *, "=== Testing Issue #2155: Character length in mangled names ==="

    call read_example('examples/lf/issue_playtest5_subroutine_name_collision.lf', &
                      source)
    call transform_lazy_fortran_string(source, output, errors)

    if (len_trim(errors) > 0) then
        write (error_unit, '(A)') 'FAIL: transformation reported errors'
        write (error_unit, '(A)') trim(errors)
        error stop 1
    end if

    ! Check for distinct mangled names for different character lengths
    call assert_contains(output, 'greet__ch5', &
                         'missing character(len=5) greet specialization')
    call assert_contains(output, 'greet__ch7', &
                         'missing character(len=7) greet specialization')

    ! Verify the correct character lengths in declarations
    call assert_contains(output, 'character(len=5), intent(in) :: name', &
                         'missing character(len=5) declaration')
    call assert_contains(output, 'character(len=7), intent(in) :: name', &
                         'missing character(len=7) declaration')

    ! Check that the interface references both specializations
    call assert_contains(output, 'interface greet', &
                         'missing interface declaration')

    ! Ensure no duplicate names (this will fail if both are named greet__ch)
    call assert_no_duplicate_procedure_names(output)

    print *, "PASS: Character length collision test passed"

contains

    include 'common/cli_io_reader.inc'

    subroutine assert_contains(text, pattern, message)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: pattern
        character(len=*), intent(in) :: message

        if (index(text, pattern) <= 0) then
            write (error_unit, '(A)') 'FAIL: ' // trim(message)
            write (error_unit, '(A)') 'Expected to find: ' // trim(pattern)
            error stop 1
        end if
    end subroutine assert_contains

    subroutine assert_no_duplicate_procedure_names(text)
        character(len=*), intent(in) :: text
        integer :: pos1, pos2

        ! Check if greet__ch appears without a digit after it (which would indicate duplicate)
        pos1 = index(text, 'greet__ch ')
        pos2 = index(text, 'greet__ch(')

        if (pos1 > 0 .or. pos2 > 0) then
            ! Found greet__ch without digit - this is the bug!
            write (error_unit, '(A)') 'FAIL: Found greet__ch without length suffix'
            write (error_unit, '(A)') 'This indicates name collision bug is present'
            error stop 1
        end if
    end subroutine assert_no_duplicate_procedure_names

    subroutine read_example(path, content)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: content
        integer :: status

        call read_all_stdin_or_file(.true., path, content, status)
        if (status /= 0) then
            write (error_unit, '(A)') 'FAIL: failed to read ' // trim(path)
            error stop 1
        end if
    end subroutine read_example

end program test_issue_2155_character_length_collision
