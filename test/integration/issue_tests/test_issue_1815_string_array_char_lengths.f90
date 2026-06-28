program test_issue_1815_string_array_char_lengths
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Issue #1815: String array literal character lengths ==='

    if (.not. test_string_array_padding()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'Issue #1815 fixed!'
    else
        print *, 'Issue #1815 regression detected!'
        stop 1
    end if

contains

    logical function test_string_array_padding()
        character(len=:), allocatable :: source, output, error_msg
        logical :: alice_ok, bob_ok, charlie_ok

        test_string_array_padding = .true.
        print *, 'Testing string array literal padding...'

        source = 'names = ["Alice", "Bob", "Charlie"]' // new_line('a') // &
            'print *, names(1)' // new_line('a') // &
            'print *, names(2)' // new_line('a') // &
            'print *, names(3)'

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Unexpected error -', trim(error_msg)
                test_string_array_padding = .false.
                return
            end if
        end if

        if (.not. allocated(output)) then
            print *, '  FAIL: No output generated'
            test_string_array_padding = .false.
            return
        end if

        alice_ok = index(output, '"Alice  "') > 0
        bob_ok = index(output, '"Bob    "') > 0
        charlie_ok = index(output, '"Charlie"') > 0

        if (.not. alice_ok) then
            print *, '  FAIL: Alice not padded to 7 characters'
            test_string_array_padding = .false.
        end if

        if (.not. bob_ok) then
            print *, '  FAIL: Bob not padded to 7 characters'
            test_string_array_padding = .false.
        end if

        if (.not. charlie_ok) then
            print *, '  FAIL: Charlie should not be padded'
            test_string_array_padding = .false.
        end if

        if (test_string_array_padding) then
            print *, '  PASS: String array literals padded correctly'
        end if
    end function test_string_array_padding

end program test_issue_1815_string_array_char_lengths
