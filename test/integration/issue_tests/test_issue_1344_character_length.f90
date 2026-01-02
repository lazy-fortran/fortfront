program test_issue_1344_character_length
    use fortfront, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, iostat_eor
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Issue #1344: Preserve character length specifiers ==='

    if (.not. test_character_length_preservation()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'Issue #1344 fixed!'
    else
        print *, 'Issue #1344 regression detected!'
        stop 1
    end if

contains

    include '../../common/cli_io_reader.inc'
    include '../../common/read_example.inc'


    logical function test_character_length_preservation()
        character(len=:), allocatable :: source, output, error_msg
        logical :: str1_ok, str2_ok, str3_ok, str4_ok

        test_character_length_preservation = .true.
        print *, 'Testing character length preservation...'

        call read_example('examples/f90/issue_1344_character_length.f90', source)

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Unexpected error -', trim(error_msg)
                test_character_length_preservation = .false.
                return
            end if
        end if

        if (.not. allocated(output)) then
            print *, '  FAIL: No output generated'
            test_character_length_preservation = .false.
            return
        end if

        str1_ok = index(output, 'character(len=10) :: str1') > 0
        str2_ok = index(output, 'character(len=*), parameter :: str2 =') > 0
        str3_ok = index(output, 'character(len=20) :: str3') > 0
        str4_ok = index(output, 'character(len=12) :: text') > 0

        if (.not. str1_ok) then
            print *, '  FAIL: str1 length specifier missing'
            test_character_length_preservation = .false.
        end if

        if (.not. str4_ok) then
            print *, '  FAIL: non-ASCII name normalization lost its length specifier'
            test_character_length_preservation = .false.
        end if

        if (.not. str2_ok) then
            print *, '  FAIL: str2 assumed length lost'
            test_character_length_preservation = .false.
        end if

        if (.not. str3_ok) then
            print *, '  FAIL: str3 legacy length not preserved'
            test_character_length_preservation = .false.
        end if

        if (test_character_length_preservation) then
            print *, '  PASS: Character lengths preserved in generated code'
        end if
    end function test_character_length_preservation

end program test_issue_1344_character_length
