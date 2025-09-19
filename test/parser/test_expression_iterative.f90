program test_expression_iterative
    use frontend, only: compile_source, compilation_options_t
    implicit none

    logical :: all_passed

    print *, "=== Expression Iterative Parsing Tests ==="

    all_passed = test_deep_exponent_chain()
    all_passed = all_passed .and. test_deep_unary_minus_chain()
    all_passed = all_passed .and. test_logical_not_chain()
    all_passed = all_passed .and. test_mixed_precedence_output()
    all_passed = all_passed .and. test_extreme_parentheses_depth()

    if (all_passed) then
        print *, "All expression parsing tests passed!"
        stop 0
    else
        print *, "Some expression parsing tests failed!"
        stop 1
    end if

contains

    logical function test_deep_exponent_chain()
        integer, parameter :: depth = 16
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit, iostat, i
        character(len=512) :: line
        logical :: found_expr, has_bad_grouping

        print *, "Testing deep exponentiation chains..."
        test_deep_exponent_chain = .true.
        input_file = 'test_expression_iterative_exp.lf'
        output_file = 'test_expression_iterative_exp_out.f90'

        open(newunit=unit, file=input_file, status='replace', action='write')
        write(unit, '(a)') 'program test_exp_chain'
        write(unit, '(a)') '    real :: value'
        write(unit, '(a)', advance='no') '    value = 2.0'
        do i = 1, depth
            write(unit, '(a)', advance='no') ' ** 2.0'
        end do
        write(unit, '(a)') ''
        write(unit, '(a)') '    print *, value'
        write(unit, '(a)') 'end program test_exp_chain'
        close(unit)

        options%output_file = output_file
        call compile_source(input_file, options, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_deep_exponent_chain = .false.
            return
        end if

        open(newunit=unit, file=output_file, status='old', action='read', iostat=iostat)
        if (iostat /= 0) then
            print *, '  FAIL: Could not open generated file'
            test_deep_exponent_chain = .false.
            return
        end if

        found_expr = .false.
        has_bad_grouping = .false.
        do
            read(unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            if (index(trim(line), 'value =') > 0) then
                found_expr = .true.
                if (index(line, '(**') > 0 .or. index(line, ')**') > 0 .or. &
                    index(line, ') **') > 0) then
                    has_bad_grouping = .true.
                end if
            end if
        end do
        close(unit)

        if (.not. found_expr) then
            print *, '  FAIL: Did not find exponent expression in output'
            test_deep_exponent_chain = .false.
        else if (has_bad_grouping) then
            print *, '  FAIL: Exponent chain shows left-associative grouping'
            test_deep_exponent_chain = .false.
        else
            print *, '  PASS: Deep exponent chain parsed without recursion'
        end if
    end function test_deep_exponent_chain

    logical function test_deep_unary_minus_chain()
        integer, parameter :: depth = 24
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit, iostat, i, count, start, pos
        character(len=512) :: line
        logical :: found_expr

        print *, "Testing deep unary minus chains..."
        test_deep_unary_minus_chain = .true.
        input_file = 'test_expression_iterative_unary.lf'
        output_file = 'test_expression_iterative_unary_out.f90'

        open(newunit=unit, file=input_file, status='replace', action='write')
        write(unit, '(a)') 'program test_unary_chain'
        write(unit, '(a)') '    real :: base, value'
        write(unit, '(a)') '    base = 1.0'
        write(unit, '(a)', advance='no') '    value ='
        do i = 1, depth
            write(unit, '(a)', advance='no') ' -'
        end do
        write(unit, '(a)') ' base'
        write(unit, '(a)') '    print *, value'
        write(unit, '(a)') 'end program test_unary_chain'
        close(unit)

        options%output_file = output_file
        call compile_source(input_file, options, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_deep_unary_minus_chain = .false.
            return
        end if

        open(newunit=unit, file=output_file, status='old', action='read', iostat=iostat)
        if (iostat /= 0) then
            print *, '  FAIL: Could not open generated file'
            test_deep_unary_minus_chain = .false.
            return
        end if

        found_expr = .false.
        count = 0
        do
            read(unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            if (index(trim(line), 'value =') > 0) then
                found_expr = .true.
                start = 1
                do
                    pos = index(line(start:), '0 -')
                    if (pos == 0) exit
                    count = count + 1
                    start = start + pos
                end do
            end if
        end do
        close(unit)

        if (.not. found_expr) then
            print *, '  FAIL: Did not find unary expression in output'
            test_deep_unary_minus_chain = .false.
        else if (count /= depth) then
            print *, '  FAIL: Expected', depth, 'unary minus expansions but found', &
                count
            test_deep_unary_minus_chain = .false.
        else
            print *, '  PASS: Deep unary minus chain expanded iteratively'
        end if
    end function test_deep_unary_minus_chain

    logical function test_logical_not_chain()
        integer, parameter :: depth = 20
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit, iostat, i, count, start, pos
        character(len=512) :: line
        logical :: found_expr

        print *, "Testing repeated logical NOT chains..."
        test_logical_not_chain = .true.
        input_file = 'test_expression_iterative_not.lf'
        output_file = 'test_expression_iterative_not_out.f90'

        open(newunit=unit, file=input_file, status='replace', action='write')
        write(unit, '(a)') 'program test_not_chain'
        write(unit, '(a)') '    logical :: flag, result'
        write(unit, '(a)') '    flag = .true.'
        write(unit, '(a)', advance='no') '    result ='
        do i = 1, depth
            write(unit, '(a)', advance='no') ' .not.'
        end do
        write(unit, '(a)') ' flag'
        write(unit, '(a)') '    print *, result'
        write(unit, '(a)') 'end program test_not_chain'
        close(unit)

        options%output_file = output_file
        call compile_source(input_file, options, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_logical_not_chain = .false.
            return
        end if

        open(newunit=unit, file=output_file, status='old', action='read', iostat=iostat)
        if (iostat /= 0) then
            print *, '  FAIL: Could not open generated file'
            test_logical_not_chain = .false.
            return
        end if

        found_expr = .false.
        count = 0
        do
            read(unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            if (index(trim(line), 'result =') > 0) then
                found_expr = .true.
                start = 1
                do
                    pos = index(line(start:), '.not.')
                    if (pos == 0) exit
                    count = count + 1
                    start = start + pos + 4
                end do
            end if
        end do
        close(unit)

        if (.not. found_expr) then
            print *, '  FAIL: Did not find logical expression in output'
            test_logical_not_chain = .false.
        else if (count /= depth) then
            print *, '  FAIL: Expected', depth, 'logical NOT operators but found', count
            test_logical_not_chain = .false.
        else
            print *, '  PASS: Logical NOT chain preserved'
        end if
    end function test_logical_not_chain

    logical function test_mixed_precedence_output()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit, iostat, i
        character(len=256) :: line
        character(len=:), allocatable :: collapsed
        logical :: found_expr

        print *, "Testing mixed precedence expression..."
        test_mixed_precedence_output = .true.
        input_file = 'test_expression_iterative_mixed.lf'
        output_file = 'test_expression_iterative_mixed_out.f90'

        open(newunit=unit, file=input_file, status='replace', action='write')
        write(unit, '(a)') 'program test_mixed'
        write(unit, '(a)') '    integer :: value'
        write(unit, '(a)') '    value = -2 ** 2 + 3'
        write(unit, '(a)') '    print *, value'
        write(unit, '(a)') 'end program test_mixed'
        close(unit)

        options%output_file = output_file
        call compile_source(input_file, options, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_mixed_precedence_output = .false.
            return
        end if

        open(newunit=unit, file=output_file, status='old', action='read', iostat=iostat)
        if (iostat /= 0) then
            print *, '  FAIL: Could not open generated file'
            test_mixed_precedence_output = .false.
            return
        end if

        found_expr = .false.
        do
            read(unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            if (index(trim(line), 'value =') > 0) then
                found_expr = .true.
                collapsed = ''
                do i = 1, len_trim(line)
                    if (line(i:i) /= ' ' .and. line(i:i) /= char(9)) then
                        collapsed = collapsed // line(i:i)
                    end if
                end do
                if (index(collapsed, 'value=(0-2)**2+3') == 0) then
                    print *, '  FAIL: Mixed precedence expression not normalized as expected'
                    test_mixed_precedence_output = .false.
                end if
            end if
        end do
        close(unit)

        if (.not. found_expr) then
            print *, '  FAIL: Did not find mixed expression in output'
            test_mixed_precedence_output = .false.
        else if (test_mixed_precedence_output) then
            print *, '  PASS: Mixed precedence expression uses expected ordering'
        end if
    end function test_mixed_precedence_output

    logical function test_extreme_parentheses_depth()
        integer, parameter :: depth = 4096
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit, iostat, i
        integer :: paren_group
        integer :: chunk_size, remaining

        print *, "Testing extreme parentheses nesting (depth =", depth, ')'
        test_extreme_parentheses_depth = .true.
        input_file = 'test_expression_iterative_extreme.lf'
        output_file = 'test_expression_iterative_extreme_out.f90'

        open(newunit=unit, file=input_file, status='replace', action='write')
        write(unit, '(a)') 'program test_extreme_nesting'
        write(unit, '(a)') '    implicit none'
        write(unit, '(a)') '    real :: value'
        write(unit, '(a)', advance='no') '    value = '
        paren_group = 0
        i = 1
        do while (i <= depth)
            remaining = depth - i + 1
            chunk_size = min(64, remaining)
            write(unit, '(a)', advance='no') repeat('(', chunk_size)
            paren_group = paren_group + chunk_size
            if (paren_group >= 64 .and. i + chunk_size <= depth) then
                write(unit, '(a)') '&'
                write(unit, '(a)', advance='no') '    & '
                paren_group = 0
            end if
            i = i + chunk_size
        end do
        write(unit, '(a)', advance='no') '1.0'
        paren_group = 0
        i = 1
        do while (i <= depth)
            remaining = depth - i + 1
            chunk_size = merge(64, remaining, remaining >= 64)
            if (chunk_size > remaining) chunk_size = remaining
            if (paren_group == 0 .and. i > 1 .and. i < depth) then
                write(unit, '(a)') '&'
                write(unit, '(a)', advance='no') '    & '
            end if
            write(unit, '(a)', advance='no') repeat(')', chunk_size)
            paren_group = mod(paren_group + chunk_size, 64)
            i = i + chunk_size
        end do
        write(unit, '(a)') ''
        write(unit, '(a)') '    print *, value'
        write(unit, '(a)') 'end program test_extreme_nesting'
        close(unit)

        options%output_file = output_file
        call compile_source(input_file, options, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error for extreme nesting:', trim(error_msg)
            test_extreme_parentheses_depth = .false.
            return
        end if

        open(newunit=unit, file=output_file, status='old', action='read', iostat=iostat)
        if (iostat /= 0) then
            print *, '  FAIL: Could not open generated file for extreme nesting'
            test_extreme_parentheses_depth = .false.
            return
        end if
        close(unit)

        print *, '  PASS: Extreme parentheses nesting parsed successfully'
    end function test_extreme_parentheses_depth

end program test_expression_iterative
