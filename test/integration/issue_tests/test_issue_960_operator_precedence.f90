program test_issue_960_operator_precedence
    use frontend, only: compile_source, compilation_options_t
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Issue #960: Operator precedence associativity (exponentiation) ==='

    if (.not. test_exponentiation_right_associative()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'Issue #960 tests passed!'
        stop 0
    else
        print *, 'Issue #960 tests failed!'
        stop 1
    end if

contains

    logical function test_exponentiation_right_associative()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg, line
        type(compilation_options_t) :: options
        integer :: unit, iostat
        logical :: saw_chain, saw_wrong_grouping

        test_exponentiation_right_associative = .true.
        print *, 'Testing exponentiation right-associativity (a**b**c == a**(b**c))...'

        ! Minimal input exercising chained exponentiation
        input_file = 'test_issue_960.lf'
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') 'a = 2'
        write(unit, '(a)') 'b = 3'
        write(unit, '(a)') 'c = 2'
        write(unit, '(a)') 'x = a**b**c'
        close(unit)

        output_file = 'test_issue_960_out.f90'
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_exponentiation_right_associative = .false.
            return
        end if

        saw_chain = .false.
        saw_wrong_grouping = .false.

        open(newunit=unit, file=output_file, status='old')
        do
            read(unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit

            if (contains_without_spaces(line, 'a**b**c')) then
                saw_chain = .true.
            end if

            ! Wrong grouping would look like (a**b)**c with or without spaces
            if (contains_without_spaces(line, '(a**b)**c')) then
                print *, '  FAIL: Found incorrect left-associative grouping: ', trim(line)
                saw_wrong_grouping = .true.
            end if
        end do
        close(unit)

        if (.not. saw_chain) then
            print *, '  FAIL: Did not find expected chained exponentiation in output'
            test_exponentiation_right_associative = .false.
            return
        end if

        if (saw_wrong_grouping) then
            test_exponentiation_right_associative = .false.
            return
        end if

    end function test_exponentiation_right_associative

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

end program test_issue_960_operator_precedence
