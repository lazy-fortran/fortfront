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

    ! Character procedures should NOT be monomorphized (Fortran limitation)
    ! Instead, they should use assumed-length character(len=*)

    ! Verify we're NOT creating monomorphized variants with specific lengths
    call assert_not_contains(output, 'greet__ch5', &
                             'should not create character(len=5) specialization')
    call assert_not_contains(output, 'greet__ch7', &
                             'should not create character(len=7) specialization')

    ! Verify we're NOT creating a monomorphization module
    call assert_not_contains(output, 'module auto_greet', &
                             'should not monomorphize character-only variations')
    call assert_not_contains(output, 'interface greet', &
                             'should not create generic interface for character-only variations')

    ! Verify we're using assumed-length character
    call assert_contains(output, 'character(len=*)', &
                         'should use assumed-length character')

    ! Verify the subroutine is defined once with assumed-length
    call assert_contains(output, 'subroutine greet(name)', &
                         'missing greet subroutine definition')

    ! Ensure no duplicate procedure definitions
    call assert_no_duplicate_procedure_names(output)

    print *, "PASS: Character length collision test passed"

contains

    include 'common/read_example.inc'


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

    subroutine assert_not_contains(text, pattern, message)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: pattern
        character(len=*), intent(in) :: message

        if (index(text, pattern) > 0) then
            write (error_unit, '(A)') 'FAIL: ' // trim(message)
            write (error_unit, '(A)') 'Did not expect to find: ' // trim(pattern)
            error stop 1
        end if
    end subroutine assert_not_contains

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


end program test_issue_2155_character_length_collision
