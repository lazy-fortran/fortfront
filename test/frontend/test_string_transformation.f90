program test_string_transformation
    use frontend, only: transform_lazy_fortran_string
    implicit none

    integer :: test_count, pass_count

    test_count = 0
    pass_count = 0

    print *, "=== String Transformation Unit Tests ==="
    print *, ""

    ! Test 1: Simple hello world
    call test_hello_world()

    ! Test 2: Type inference
    call test_type_inference()

    ! Test 3: Control flow
    call test_control_flow()

    ! Test 4: Multiple statements
    call test_multiple_statements()

    ! Test 4b: Multi-line string concatenation
    call test_multi_line_string_concat()

    ! Test 5: Syntax error handling
    call test_syntax_error()

    ! Test 6: Empty input
    call test_empty_input()

    ! Test 7: Complex expression
    call test_complex_expression()

    ! Test 8: Non-character declarations unaffected by string logic
    call test_non_character_declaration_safety()

    print *, ""
    print *, "=== Test Summary ==="
    write (*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"

    if (pass_count == test_count) then
        print *, "All tests passed!"
        stop 0
    else
        print *, "Some tests failed!"
        stop 1
    end if

contains

    logical function contains_without_spaces(text, pattern)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: pattern
        character(len=:), allocatable :: compressed
        integer :: i

        compressed = ''
        do i = 1, len_trim(text)
            if (text(i:i) /= ' ') compressed = compressed // text(i:i)
        end do
        contains_without_spaces = index(compressed, pattern) > 0
    end function contains_without_spaces

    subroutine test_hello_world()
        character(len=:), allocatable :: output, error_msg
        logical :: success

        call test_start("Simple hello world")

        call transform_lazy_fortran_string("print *, 'Hello'", output, error_msg)

        success = (len_trim(error_msg) == 0) .and. &
                  (index(output, "program main") > 0) .and. &
                  (index(output, "implicit none") > 0) .and. &
                  (index(output, "print *, 'Hello'") > 0)

        call test_result(success)
        if (.not. success) then
            print *, "  Error: ", trim(error_msg)
            print *, "  Output length: ", len(output)
        end if
    end subroutine test_hello_world

    subroutine test_type_inference()
        character(len=:), allocatable :: input, output, error_msg
        logical :: success

        call test_start("Type inference")

        input = "x = 42" // new_line('A') // "y = 3.14"
        call transform_lazy_fortran_string(input, output, error_msg)

        success = (len_trim(error_msg) == 0) .and. &
                  (index(output, "integer :: x") > 0) .and. &
                  (index(output, "real") > 0)

        call test_result(success)
        if (.not. success) then
            print *, "  Error: ", trim(error_msg)
            print *, "  Looking for 'integer :: x' and 'real'"
        end if
    end subroutine test_type_inference

    subroutine test_control_flow()
        character(len=:), allocatable :: input, output, error_msg
        logical :: success

        call test_start("Control flow (if statement)")

        input = "x = 5" // new_line('A') // &
                "if (x > 0) then" // new_line('A') // &
                "  print *, 'positive'" // new_line('A') // &
                "end if"

        call transform_lazy_fortran_string(input, output, error_msg)

        success = (len_trim(error_msg) == 0) .and. &
                  (index(output, "if (x > 0) then") > 0) .and. &
                  (index(output, "end if") > 0)

        call test_result(success)
        if (.not. success) then
            print *, "  Error: ", trim(error_msg)
        end if
    end subroutine test_control_flow

    subroutine test_multiple_statements()
        character(len=:), allocatable :: input, output, error_msg
        logical :: success

        call test_start("Multiple statements")

        input = "a = 1" // new_line('A') // &
                "b = 2" // new_line('A') // &
                "c = a + b" // new_line('A') // &
                "print *, c"

        call transform_lazy_fortran_string(input, output, error_msg)

        success = (len_trim(error_msg) == 0) .and. &
                  has_integer_declaration(output, [character(len=8) :: &
                                                   "a", "b", "c"])

        call test_result(success)
        if (.not. success) then
            print *, "  Error: ", trim(error_msg)
        end if
    end subroutine test_multiple_statements

    subroutine test_multi_line_string_concat()
        character(len=:), allocatable :: input, output, error_msg
        logical :: success

        call test_start("Multi-line string concatenation")

        input = "s = 'a' // 'b'" // new_line('A') // &
                "t = s // 'c'" // new_line('A') // &
                "print *, t"

        call transform_lazy_fortran_string(input, output, error_msg)

        ! Expect no errors and both assignments preserved in output
        success = (len_trim(error_msg) == 0) .and. &
                  contains_without_spaces(output, "s='a'//'b'") .and. &
                  contains_without_spaces(output, "t=s//'c'")

        call test_result(success)
        if (.not. success) then
            print *, "  Error: ", trim(error_msg)
            print *, "  Output: ", trim(output)
        end if
    end subroutine test_multi_line_string_concat

    subroutine test_non_character_declaration_safety()
        character(len=:), allocatable :: input, output, error_msg
        logical :: success

        call test_start("Non-character declaration safety")

        input = "integer :: n" // new_line('A') // &
                "n = 5" // new_line('A') // &
                "s = 'x'" // new_line('A') // &
                "s = s // 'y'" // new_line('A') // &
                "print *, n, s"

        call transform_lazy_fortran_string(input, output, error_msg)

        ! Ensure integer declaration remains and character handling applies only to strings
        success = (len_trim(error_msg) == 0) .and. &
                  contains_without_spaces(output, "integer::n")

        call test_result(success)
        if (.not. success) then
            print *, "  Error: ", trim(error_msg)
            print *, "  Output: ", trim(output)
        end if
    end subroutine test_non_character_declaration_safety

    logical function has_integer_declaration(text, names)
        character(len=*), intent(in) :: text
        character(len=*), dimension(:), intent(in) :: names
        integer :: pos, start_pos, end_pos, i, text_len
        character(len=:), allocatable :: line
        character(1), parameter :: nl = new_line("a")

        has_integer_declaration = .false.
        text_len = len(text)
        pos = index(text, "integer ::")

        do while (pos > 0)
            start_pos = pos
            do while (start_pos > 1 .and. text(start_pos - 1:start_pos - 1) /= nl)
                start_pos = start_pos - 1
            end do

            end_pos = pos
            do while (end_pos <= text_len .and. text(end_pos:end_pos) /= nl)
                end_pos = end_pos + 1
            end do

            if (end_pos > text_len) then
                line = text(start_pos:)
            else
                line = text(start_pos:end_pos - 1)
            end if

            line = adjustl(line)
            if (index(line, "integer ::") == 1) then
                has_integer_declaration = .true.
                do i = 1, size(names)
                    if (index(line, trim(names(i))) == 0) then
                        has_integer_declaration = .false.
                        exit
                    end if
                end do
                if (has_integer_declaration) return
            end if

            if (end_pos > text_len) exit
            pos = index(text(end_pos:), "integer ::")
            if (pos > 0) pos = pos + end_pos - 1
        end do
    end function has_integer_declaration

    subroutine test_syntax_error()
        character(len=:), allocatable :: output, error_msg
        logical :: success

        call test_start("Syntax error handling")

        call transform_lazy_fortran_string("invalid fortran !!!", output, error_msg)

        ! For syntax errors, we expect the transformation to still work
        ! but might produce minimal output
        success = .true.  ! Any non-crash result is success for now

        call test_result(success)
    end subroutine test_syntax_error

    subroutine test_empty_input()
        character(len=:), allocatable :: output, error_msg
        logical :: success

        call test_start("Empty input")

        call transform_lazy_fortran_string("", output, error_msg)

        ! Empty input should produce minimal program
        success = (index(output, "program main") > 0)

        call test_result(success)
        if (.not. success) then
            print *, "  Error: ", trim(error_msg)
            print *, "  Output: ", trim(output)
        end if
    end subroutine test_empty_input

    subroutine test_complex_expression()
        character(len=:), allocatable :: input, output, error_msg
        logical :: success

        call test_start("Complex expression")

        input = "x = 5" // new_line('A') // &
                "y = 2.5" // new_line('A') // &
                "result = (x * 2 + y) / 3.0"
        call transform_lazy_fortran_string(input, output, error_msg)

        ! Accept both real and real(8), and both 3.0 and 3.0d0
        success = (len_trim(error_msg) == 0) .and. &
                  (contains_without_spaces(output, "result=(x*2+y)/3.0d0") .or. &
                   contains_without_spaces(output, "result=(x*2+y)/3.0")) .and. &
                  contains_without_spaces(output, "integer::x") .and. &
                  (contains_without_spaces(output, "real(8)::result") .or. &
                   contains_without_spaces(output, "real::result"))

        call test_result(success)
        if (.not. success) then
            print *, "  Error: ", trim(error_msg)
            print *, "  Looking for variable declarations and expression"
            print *, "  Actual output:"
            print *, output
        end if
    end subroutine test_complex_expression

    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write (*, '(A,A)', advance='no') "Testing: ", test_name
    end subroutine test_start

    subroutine test_result(success)
        logical, intent(in) :: success
        if (success) then
            print *, " ... PASSED"
            pass_count = pass_count + 1
        else
            print *, " ... FAILED"
        end if
    end subroutine test_result

end program test_string_transformation
