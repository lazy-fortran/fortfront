program test_semicolon_parsing
    ! Test semicolon-separated statement parsing
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    print *, "=== Semicolon-separated Statement Parsing Tests ==="

    all_passed = test_simple_semicolons()
    all_passed = all_passed .and. test_complex_semicolons()
    all_passed = all_passed .and. test_mixed_semicolons()

    if (all_passed) then
        print *, "All semicolon parsing tests passed!"
        stop 0
    else
        print *, "Some semicolon parsing tests failed!"
        stop 1
    end if

contains

    logical function test_simple_semicolons()
        character(len=:), allocatable :: source, output, error_msg

        test_simple_semicolons = .true.
        print *, "Testing simple semicolon-separated assignments..."

        source = "integer :: a, b, c; a = 1; b = 2; c = 3" // new_line('a') // &
                 "write(*,*) a, b, c"

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Compilation error:', trim(error_msg)
                test_simple_semicolons = .false.
                return
            end if
        end if

        if (index(output, 'a = 1') > 0 .and. &
            index(output, 'b = 2') > 0 .and. &
            index(output, 'c = 3') > 0 .and. &
            index(output, '!ERROR:') == 0) then
            print *, '  PASS: All semicolon-separated assignments found'
        else
            print *, '  FAIL: Missing assignments in generated code'
            test_simple_semicolons = .false.
        end if

    end function test_simple_semicolons

    logical function test_complex_semicolons()
        character(len=:), allocatable :: source, output, error_msg

        test_complex_semicolons = .true.
        print *, "Testing complex semicolon-separated statements..."

        ! Trailing newline avoids a gfortran 16 bug where a 64-character
        ! literal concat assigned to a deferred-length allocatable yields
        ! all-zero bytes.
        source = "integer :: i; real :: x; i = 42; x = 3.14" // new_line('a') // &
                 "print *, i; print *, x" // new_line('a')

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Compilation error:', trim(error_msg)
                test_complex_semicolons = .false.
                return
            end if
        end if

        if (index(output, 'integer') > 0 .and. index(output, 'i') > 0 .and. &
            index(output, 'real') > 0 .and. index(output, 'x') > 0 .and. &
            index(output, 'i = 42') > 0 .and. &
            index(output, 'x = 3.14') > 0 .and. &
            index(output, '!ERROR:') == 0) then
            print *, '  PASS: All complex semicolon statements found'
        else
            print *, '  FAIL: Missing statements in generated code'
            test_complex_semicolons = .false.
        end if

    end function test_complex_semicolons

    logical function test_mixed_semicolons()
        character(len=:), allocatable :: source, output, error_msg

        test_mixed_semicolons = .true.
        print *, "Testing mixed semicolon and regular statements..."

        source = "integer :: a; a = 100" // new_line('a') // &
                 "integer :: b" // new_line('a') // &
                 "b = 200; print *, a; print *, b"

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Compilation error:', trim(error_msg)
                test_mixed_semicolons = .false.
                return
            end if
        end if

        if (index(output, 'a = 100') > 0 .and. &
            index(output, 'integer') > 0 .and. index(output, 'b') > 0 .and. &
            index(output, 'b = 200') > 0 .and. &
            index(output, '!ERROR:') == 0) then
            print *, '  PASS: Mixed semicolon and regular statements found'
        else
            print *, '  FAIL: Missing mixed content in generated code'
            test_mixed_semicolons = .false.
        end if

    end function test_mixed_semicolons

end program test_semicolon_parsing
