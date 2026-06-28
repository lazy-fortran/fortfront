program test_issue_1581_format_write
    use transformation_api, only: transform_lazy_fortran_string

    call test_format_statement_preservation()
    call test_write_with_label_reference()
    call test_full_format_write_integration()

contains

    subroutine test_format_statement_preservation()
        character(len=:), allocatable :: source, generated, error_msg

        print *, "Test: FORMAT statement preservation"

        source = "program test" // new_line('A') // &
            "100 format('test')" // new_line('A') // &
            "end program test"

        call transform_lazy_fortran_string(source, generated, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "FAIL: Compilation failed: ", trim(error_msg)
            stop 1
        end if

        if (index(generated, "100 format") == 0) then
            print *, "FAIL: FORMAT statement with label not preserved"
            print *, "Generated: ", generated
            stop 1
        end if

        if (index(generated, "('test')") == 0 .and. index(generated, " ('test')") == 0) then
            print *, "FAIL: FORMAT specification not preserved"
            print *, "Generated: ", generated
            stop 1
        end if

        print *, "PASS: FORMAT statement preservation"
    end subroutine test_format_statement_preservation

    subroutine test_write_with_label_reference()
        character(len=:), allocatable :: source, generated, error_msg

        print *, "Test: WRITE with label reference"

        source = "program test" // new_line('A') // &
            "integer :: x" // new_line('A') // &
            "x = 42" // new_line('A') // &
            "write(*, 100) x" // new_line('A') // &
            "100 format(I5)" // new_line('A') // &
            "end program test"

        call transform_lazy_fortran_string(source, generated, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "FAIL: Compilation failed: ", trim(error_msg)
            stop 1
        end if

        if (index(generated, "write(*, 100)") == 0) then
            print *, "FAIL: WRITE with label reference not preserved"
            print *, "Generated: ", generated
            stop 1
        end if

        if (index(generated, "100 format") == 0) then
            print *, "FAIL: FORMAT statement not generated"
            print *, "Generated: ", generated
            stop 1
        end if

        print *, "PASS: WRITE with label reference"
    end subroutine test_write_with_label_reference

    subroutine test_full_format_write_integration()
        character(len=:), allocatable :: source, generated, error_msg

        print *, "Test: Full FORMAT/WRITE integration from issue #1581"

        source = "program test_format" // new_line('A') // &
            "implicit none" // new_line('A') // &
            "integer :: x" // new_line('A') // &
            "real :: y" // new_line('A') // &
            "x = 42" // new_line('A') // &
            "y = 3.14159" // new_line('A') // &
            "write(*, 100) x, y" // new_line('A') // &
            "100 format('x = ', I5, ', y = ', F8.5)" // new_line('A') // &
            "end program test_format"

        call transform_lazy_fortran_string(source, generated, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "FAIL: Full integration failed: ", trim(error_msg)
            stop 1
        end if

        if (index(generated, "write(*, 100)") == 0) then
            print *, "FAIL: WRITE statement lost"
            print *, "Generated: ", generated
            stop 1
        end if

        if (index(generated, "100 format") == 0) then
            print *, "FAIL: FORMAT statement lost"
            print *, "Generated: ", generated
            stop 1
        end if

        if (index(generated, "x, y") == 0) then
            print *, "FAIL: WRITE arguments lost"
            print *, "Generated: ", generated
            stop 1
        end if

        if (index(generated, "'x = '") == 0) then
            print *, "FAIL: FORMAT specification lost"
            print *, "Generated: ", generated
            stop 1
        end if

        print *, "PASS: Full FORMAT/WRITE integration"
    end subroutine test_full_format_write_integration

end program test_issue_1581_format_write
