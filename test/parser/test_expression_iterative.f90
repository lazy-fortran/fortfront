program test_expression_iterative
    use transformation_api, only: compile_source, compilation_options_t

    logical :: all_passed

    print *, "=== Expression Iterative Parsing Tests ==="

    all_passed = test_deep_exponent_chain()
    all_passed = all_passed .and. test_deep_unary_minus_chain()
    all_passed = all_passed .and. test_logical_not_chain()
    all_passed = all_passed .and. test_not_associated_postfix()
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

    function get_temp_filepath(basename) result(path)
        character(len=*), intent(in) :: basename
        character(len=:), allocatable :: path
        character(len=256) :: envtmp
        integer :: ios, last
        character(len=:), allocatable :: prefix
        logical :: is_windows
        logical :: tmp_exists

        call get_environment_variable('TMPDIR', envtmp, status=ios)
        if (ios /= 0 .or. len_trim(envtmp) == 0) then
            call get_environment_variable('TEMP', envtmp, status=ios)
        end if
        if (ios /= 0 .or. len_trim(envtmp) == 0) then
            call get_environment_variable('TMP', envtmp, status=ios)
        end if
        if (ios /= 0 .or. len_trim(envtmp) == 0) then
            is_windows = .false.
            call get_environment_variable('OS', envtmp, status=ios)
            if (ios == 0) then
                if (envtmp(1:7) == 'Windows') is_windows = .true.
            end if
            if (.not. is_windows) then
                call get_environment_variable('WINDIR', envtmp, status=ios)
                if (ios == 0) is_windows = .true.
            end if
            envtmp = '/tmp'
            if (is_windows) then
                inquire (file=envtmp, exist=tmp_exists)
                if (.not. tmp_exists) envtmp = '.'
            end if
        end if

        last = len_trim(envtmp)
        if (last == 0) then
            prefix = '.'
        else if (envtmp(last:last) == '/' .or. envtmp(last:last) == '\') then
            prefix = trim(envtmp)
        else
            prefix = trim(envtmp) // '/'
        end if
        path = prefix // trim(basename)
    end function get_temp_filepath

    subroutine emit_open_parens(u, depth)
        implicit none
        integer, intent(in) :: u, depth
        integer :: i, remaining, chunk_size, paren_group

        paren_group = 0
        i = 1
        do while (i <= depth)
            remaining = depth - i + 1
            chunk_size = min(64, remaining)
            write (u, '(a)', advance='no') repeat('(', chunk_size)
            paren_group = paren_group + chunk_size
            if (paren_group >= 64 .and. i + chunk_size <= depth) then
                write (u, '(a)') '&'
                write (u, '(a)', advance='no') '    & '
                paren_group = 0
            end if
            i = i + chunk_size
        end do
    end subroutine emit_open_parens

    subroutine emit_close_parens(u, depth)
        implicit none
        integer, intent(in) :: u, depth
        integer :: i, remaining, chunk_size, paren_group

        paren_group = 0
        i = 1
        do while (i <= depth)
            remaining = depth - i + 1
            chunk_size = min(64, remaining)
            if (paren_group == 0 .and. i > 1 .and. i < depth) then
                write (u, '(a)') '&'
                write (u, '(a)', advance='no') '    & '
            end if
            write (u, '(a)', advance='no') repeat(')', chunk_size)
            paren_group = mod(paren_group + chunk_size, 64)
            i = i + chunk_size
        end do
    end subroutine emit_close_parens

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
        input_file = get_temp_filepath('test_expression_iterative_exp.lf')
        output_file = get_temp_filepath('test_expression_iterative_exp_out.f90')

        open (newunit=unit, file=input_file, status='replace', action='write')
        write (unit, '(a)') 'program test_exp_chain'
        write (unit, '(a)') '    real :: value'
        write (unit, '(a)', advance='no') '    value = 2.0'
        do i = 1, depth
            write (unit, '(a)', advance='no') ' ** 2.0'
        end do
        write (unit, '(a)') ''
        write (unit, '(a)') '    print *, value'
        write (unit, '(a)') 'end program test_exp_chain'
        close (unit)

        options%output_file = output_file
        call compile_source(input_file, options, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_deep_exponent_chain = .false.
            return
        end if

        open (newunit=unit, file=output_file, status='old', action='read', &
              iostat=iostat)
        if (iostat /= 0) then
            print *, '  FAIL: Could not open generated file'
            test_deep_exponent_chain = .false.
            return
        end if

        found_expr = .false.
        has_bad_grouping = .false.
        do
            read (unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            if (index(trim(line), 'value =') > 0) then
                found_expr = .true.
                if (index(line, '(**') > 0 .or. index(line, ')**') > 0 .or. &
                    index(line, ') **') > 0) then
                    has_bad_grouping = .true.
                end if
            end if
        end do
        close (unit)

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
        integer :: unit, iostat, i, minus_count
        character(len=512) :: line
        logical :: found_expr
        character(len=:), allocatable :: collapsed

        print *, "Testing deep unary minus chains..."
        test_deep_unary_minus_chain = .true.
        input_file = get_temp_filepath('test_expression_iterative_unary.lf')
        output_file = get_temp_filepath('test_expression_iterative_unary_out.f90')

        open (newunit=unit, file=input_file, status='replace', action='write')
        write (unit, '(a)') 'program test_unary_chain'
        write (unit, '(a)') '    real :: base, value'
        write (unit, '(a)') '    base = 1.0'
        write (unit, '(a)', advance='no') '    value ='
        do i = 1, depth
            write (unit, '(a)', advance='no') ' -'
        end do
        write (unit, '(a)') ' base'
        write (unit, '(a)') '    print *, value'
        write (unit, '(a)') 'end program test_unary_chain'
        close (unit)

        options%output_file = output_file
        call compile_source(input_file, options, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_deep_unary_minus_chain = .false.
            return
        end if

        open (newunit=unit, file=output_file, status='old', action='read', &
              iostat=iostat)
        if (iostat /= 0) then
            print *, '  FAIL: Could not open generated file'
            test_deep_unary_minus_chain = .false.
            return
        end if

        found_expr = .false.
        minus_count = 0
        do
            read (unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            if (index(trim(line), 'value =') > 0) then
                found_expr = .true.
                collapsed = ''
                do i = 1, len_trim(line)
                    if (line(i:i) /= ' ' .and. line(i:i) /= char(9)) then
                        collapsed = collapsed // line(i:i)
                    end if
                end do
                minus_count = 0
                do i = 1, len(collapsed)
                    if (collapsed(i:i) == '-') minus_count = minus_count + 1
                end do
            end if
        end do
        close (unit)

        if (.not. found_expr) then
            print *, '  FAIL: Did not find unary expression in output'
            test_deep_unary_minus_chain = .false.
        else if (minus_count /= depth) then
            print *, '  FAIL: Expected', depth, 'unary minus expansions but found', &
                minus_count
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
        input_file = get_temp_filepath('test_expression_iterative_not.lf')
        output_file = get_temp_filepath('test_expression_iterative_not_out.f90')

        open (newunit=unit, file=input_file, status='replace', action='write')
        write (unit, '(a)') 'program test_not_chain'
        write (unit, '(a)') '    logical :: flag, result'
        write (unit, '(a)') '    flag = .true.'
        write (unit, '(a)', advance='no') '    result ='
        do i = 1, depth
            write (unit, '(a)', advance='no') ' .not.'
        end do
        write (unit, '(a)') ' flag'
        write (unit, '(a)') '    print *, result'
        write (unit, '(a)') 'end program test_not_chain'
        close (unit)

        options%output_file = output_file
        call compile_source(input_file, options, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_logical_not_chain = .false.
            return
        end if

        open (newunit=unit, file=output_file, status='old', action='read', &
              iostat=iostat)
        if (iostat /= 0) then
            print *, '  FAIL: Could not open generated file'
            test_logical_not_chain = .false.
            return
        end if

        found_expr = .false.
        count = 0
        do
            read (unit, '(a)', iostat=iostat) line
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
        close (unit)

        if (.not. found_expr) then
            print *, '  FAIL: Did not find logical expression in output'
            test_logical_not_chain = .false.
        else if (count /= depth) then
            print *, '  FAIL: Expected', depth, &
                'logical NOT operators but found', count
            test_logical_not_chain = .false.
        else
            print *, '  PASS: Logical NOT chain preserved'
        end if
    end function test_logical_not_chain

    logical function test_not_associated_postfix()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit, iostat
        character(len=512) :: line
        logical :: found_if, found_allocate, found_deallocate

        print *, "Testing logical NOT with associated and allocation..."
        test_not_associated_postfix = .true.
        input_file = get_temp_filepath('test_not_associated_postfix.lf')
        output_file = get_temp_filepath('test_not_associated_postfix_out.f90')

        open (newunit=unit, file=input_file, status='replace', action='write')
        write (unit, '(a)') 'program test_ptr_flow'
        write (unit, '(a)') '    integer, pointer :: ptr => null()'
        write (unit, '(a)') '    if (.not. associated(ptr)) print *, 1'
        write (unit, '(a)') '    allocate(ptr)'
        write (unit, '(a)') '    deallocate(ptr)'
        write (unit, '(a)') 'end program test_ptr_flow'
        close (unit)

        options%output_file = output_file
        call compile_source(input_file, options, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_not_associated_postfix = .false.
            return
        end if

        open (newunit=unit, file=output_file, status='old', action='read', &
              iostat=iostat)
        if (iostat /= 0) then
            print *, '  FAIL: Could not open generated file'
            test_not_associated_postfix = .false.
            return
        end if

        found_if = .false.
        found_allocate = .false.
        found_deallocate = .false.
        do
            read (unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            if (.not. found_if) then
                if (index(line, 'if (.not. associated(ptr)) then') > 0) then
                    found_if = .true.
                end if
            end if
            if (.not. found_allocate) then
                if (index(line, 'allocate(ptr)') > 0) then
                    found_allocate = .true.
                end if
            end if
            if (.not. found_deallocate) then
                if (index(line, 'deallocate(ptr)') > 0) then
                    found_deallocate = .true.
                end if
            end if
        end do
        close (unit)

        if (.not. found_if) then
            print *, '  FAIL: Logical NOT with associated lost arguments'
            test_not_associated_postfix = .false.
        else if (.not. found_allocate) then
            print *, '  FAIL: ALLOCATE statement was dropped'
            test_not_associated_postfix = .false.
        else if (.not. found_deallocate) then
            print *, '  FAIL: DEALLOCATE statement was dropped'
            test_not_associated_postfix = .false.
        else
            print *, '  PASS: Prefix and postfix intrinsics preserved'
        end if
    end function test_not_associated_postfix

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
        input_file = get_temp_filepath('test_expression_iterative_mixed.lf')
        output_file = get_temp_filepath('test_expression_iterative_mixed_out.f90')

        open (newunit=unit, file=input_file, status='replace', action='write')
        write (unit, '(a)') 'program test_mixed'
        write (unit, '(a)') '    integer :: value'
        write (unit, '(a)') '    value = -2 ** 2 + 3'
        write (unit, '(a)') '    print *, value'
        write (unit, '(a)') 'end program test_mixed'
        close (unit)

        options%output_file = output_file
        call compile_source(input_file, options, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_mixed_precedence_output = .false.
            return
        end if

        open (newunit=unit, file=output_file, status='old', action='read', &
              iostat=iostat)
        if (iostat /= 0) then
            print *, '  FAIL: Could not open generated file'
            test_mixed_precedence_output = .false.
            return
        end if

        found_expr = .false.
        do
            read (unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            if (index(trim(line), 'value =') > 0) then
                found_expr = .true.
                collapsed = ''
                do i = 1, len_trim(line)
                    if (line(i:i) /= ' ' .and. line(i:i) /= char(9)) then
                        collapsed = collapsed // line(i:i)
                    end if
                end do
                if (index(collapsed, 'value=(-2)**2+3') == 0) then
                    print *, &
                        '  FAIL: Mixed precedence expression not normalized as expected'
                    test_mixed_precedence_output = .false.
                end if
            end if
        end do
        close (unit)

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
        integer :: unit, iostat

        print *, "Testing extreme parentheses nesting (depth =", depth, ')'
        test_extreme_parentheses_depth = .true.
        input_file = get_temp_filepath('test_expression_iterative_extreme.lf')
        output_file = get_temp_filepath('test_expression_iterative_extreme_out.f90')

        open (newunit=unit, file=input_file, status='replace', action='write')
        write (unit, '(a)') 'program test_extreme_nesting'
        write (unit, '(a)') '    implicit none'
        write (unit, '(a)') '    real :: value'
        write (unit, '(a)', advance='no') '    value = '
        call emit_open_parens(unit, depth)
        write (unit, '(a)', advance='no') '1.0'
        call emit_close_parens(unit, depth)
        write (unit, '(a)') ''
        write (unit, '(a)') '    print *, value'
        write (unit, '(a)') 'end program test_extreme_nesting'
        close (unit)

        options%output_file = output_file
        call compile_source(input_file, options, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error for extreme nesting:', trim(error_msg)
            test_extreme_parentheses_depth = .false.
            return
        end if

        open (newunit=unit, file=output_file, status='old', action='read', &
              iostat=iostat)
        if (iostat /= 0) then
            print *, '  FAIL: Could not open generated file for extreme nesting'
            test_extreme_parentheses_depth = .false.
            return
        end if
        close (unit)

        print *, '  PASS: Extreme parentheses nesting parsed successfully'
    end function test_extreme_parentheses_depth

end program test_expression_iterative
